module KohnNirenberg
  ( main )
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.Foldable                     (toList)
import           Data.IORef
import           Data.Sequence                     (Seq)
import           Data.Vector.Unboxed               (Vector, (!))
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes.Voxel               (Voxel, makeVoxel)
import           SurfaceNets
import           Text.Printf

type XYZ = (Double, Double, Double)

white,black,navy :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
navy = Color4 0 0 0.5 1

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

fKN :: XYZ -> Double
fKN (x,y,z') =
  z + z^2*b + b^4 + 15/7*b*(x^6 - 15*x^4*y^2 + 15*x^2*y^4 - y^6) +
    k*(x^2 + y^2 + z^2)^5
  where
  z = 3*z'
  b = x*x + y*y
  k = 0.1

voxel :: Voxel
voxel = makeVoxel fKN ((-3.1,3.1),(-3.5,3.5),(-0.6,0.5)) (100, 100, 100)

kn :: ((Vector XYZ, Seq [Int]), Vector XYZ)
kn = surfaceNets voxel 0

vertices :: Vector XYZ
vertices = fst $ fst kn

faces :: [[Int]]
faces = toList $ snd $ fst kn

normals :: Vector XYZ
normals = snd kn

triangle :: [Int] -> ((XYZ, XYZ, XYZ), (XYZ, XYZ, XYZ))
triangle face =
  ( (vertices ! i, vertices ! j, vertices ! k)
  , (normals ! i, normals ! j, normals ! k))
  where
  i = face !! 0
  j = face !! 1
  k = face !! 2

triangles :: [((XYZ, XYZ, XYZ), (XYZ, XYZ, XYZ))]
triangles = map triangle faces

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),(n1,n2,n3)) = do
      normal (toNormal n1)
      materialDiffuse Front $= navy
      vertex (toVertex v1)
      normal (toNormal n2)
      materialDiffuse Front $= navy
      vertex (toVertex v2)
      normal (toNormal n3)
      materialDiffuse Front $= navy
      vertex (toVertex v3)
      where
        toNormal (x,y,z) = Normal3 x y z
        toVertex (x,y,z) = Vertex3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-9+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> IORef Bool -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim c _ = do
  case c of
    'a' -> writeIORef anim True
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef GLfloat -> IORef Int -> IdleCallback
idle anim rot3 snapshots = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/kohnNirenberg%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
        print s
      rot3 $~! (+ 1.0)
      snapshots $~! (+ 1)
    postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Kohn-Nirenberg surface"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  snapshots <- newIORef 0
  anim <- newIORef False
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim)
  idleCallback $= Just (idle anim rot3 snapshots)
  putStrLn "*** Kohn-Nirenberg surface ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
