module Mandelbulb2
  ( main )
  where
import           Data.IORef
import           Data.Tuple.Extra             (second)
import           Data.Vector.Unboxed          (Vector, (!), fromList)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes2
import           System.IO.Unsafe

type XYZ = (Double, Double, Double)

white,black,navy :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
navy = Color4 0 0 0.5 1

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    }

fMandelbulb :: XYZ -> Double
fMandelbulb p0@(x0,y0,z0) = if ssq p0 >= 4 then 0/0 else go 24 p0 (ssq p0)
  where
  ssq (x,y,z) = x*x + y*y + z*z
  go :: Int -> XYZ -> Double -> Double
  go n (x,y,z) r2 =
    if r2 > 4
      then sqrt r2
      else
        let theta = 8 * atan2 (sqrt(x*x+y*y)) z in
        let phi = 8 * atan2 y x in
        let r8 = r2*r2*r2*r2 in
        let xyz = ( r8 * cos phi * sin theta + x0
                  , r8 * sin phi * sin theta + y0
                  , r8 * cos theta + z0) in
        if n>1 then go (n-1) xyz (ssq xyz) else sqrt r2

voxel :: Voxel
voxel = makeVoxel fMandelbulb ((-1.1,1.1),(-1.1,1.1),(-1.1,1.1))
                  (70, 70, 70)

mandelbulb :: ((Vector XYZ, [[Int]]), [XYZ])
{-# NOINLINE mandelbulb #-}
mandelbulb = unsafePerformIO $ computeContour3d'' voxel Nothing 2.0 True True

mandelbulb' :: ((Vector XYZ, [[Int]]), Vector XYZ)
mandelbulb' = second fromList mandelbulb

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  let vertices = fst $ fst mandelbulb'
      faces = snd $ fst mandelbulb'
      normals = snd mandelbulb'
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ (drawTriangle vertices normals) faces
  swapBuffers
  where
    drawTriangle vs ns face = do
      let j0 = face !! 0
          j1 = face !! 2
          j2 = face !! 1
      materialDiffuse Front $= navy
      normal (toNormal $ ns ! j0)
      vertex (toVertex $ vs ! j0)
      normal (toNormal $ ns ! j1)
      vertex (toVertex $ vs ! j1)
      normal (toNormal $ ns ! j2)
      vertex (toVertex $ vs ! j2)
      where
        toNormal (x,y,z) = Normal3 x y z
        toVertex (x,y,z) = Vertex3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
  _ <- createWindow "Mandelbulb"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  -- lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
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
  putStrLn "*** Mandelbulb ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
