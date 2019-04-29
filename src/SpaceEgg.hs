module SpaceEgg
  ( main )
  where
import           Data.IORef
import           Data.Vector.Unboxed          (Vector, (!))
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes2
import           System.IO.Unsafe
import           Utils.Palettes
import           Utils.Color4Unbox

type XYZ = (Double, Double, Double)

white,black :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1

funColor :: Double -> Double -> Double -> Color4 GLfloat
funColor dmin dmax d = clrs !! j
  where
  clrs = colorRamp' "viridis" 256
  j = floor((d-dmin)*255/(dmax-dmin))

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    }

fEgg :: XYZ -> Double
fEgg (x,y,z) =
  - sq(cos x * sin y + cos y * sin z + cos z * sin x) +
    0.05-exp(100.0*(x*x/64+y*y/64 + z*z/(1.6*64)*exp(-0.4*z/8) - 1))
  where
  sq a = a*a

voxel :: Voxel
voxel = makeVoxel fEgg ((-7.6,7.6),(-7.6,7.6),(-8,14))
                  (200, 200, 200)

egg :: (((Vector XYZ, [[Int]]), Vector XYZ), Vector Double)
{-# NOINLINE egg #-}
egg = unsafePerformIO $ computeContour3d'' voxel Nothing 0.0 False True

colors :: Vector (Color4 GLfloat)
colors = VU.map (funColor dmin dmax) (snd egg)
  where
  dmin = VU.minimum (snd egg)
  dmax = VU.maximum (snd egg)

vertices :: Vector XYZ
vertices = fst $ fst $ fst egg

faces :: [[Int]]
faces = snd $ fst $ fst egg

normals :: Vector XYZ
normals = snd $ fst egg

triangle :: [Int] -> ((XYZ, XYZ, XYZ), (XYZ, XYZ, XYZ), (Color4 GLfloat, Color4 GLfloat, Color4 GLfloat))
triangle face =
  ( (vertices ! i, vertices ! j, vertices ! k)
  , (normals ! i, normals ! j, normals ! k)
  , (colors ! i, colors ! j, colors ! k))
  where
  i = face !! 0
  j = face !! 2
  k = face !! 1

triangles :: [((XYZ, XYZ, XYZ), (XYZ, XYZ, XYZ), (Color4 GLfloat, Color4 GLfloat, Color4 GLfloat))]
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
    drawTriangle ((v1,v2,v3),(n1,n2,n3),(c1,c2,c3)) = do
      normal (toNormal n1)
      materialDiffuse FrontAndBack $= c1
      vertex (toVertex v1)
      normal (toNormal n2)
      materialDiffuse FrontAndBack $= c2
      vertex (toVertex v2)
      normal (toNormal n3)
      materialDiffuse FrontAndBack $= c3
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
  lookAt (Vertex3 0 (-36+zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom c _ = do
  case c of
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


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Space egg"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-100) 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
--  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Space egg ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
