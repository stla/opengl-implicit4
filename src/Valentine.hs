module Valentine
  ( main )
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           System.IO.Unsafe
import           Text.Printf
import           Utils.OpenGL

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

red :: Color4 GLfloat
red = Color4 1 0 0 1

f_sphere :: Double -> XYZ -> Double
f_sphere r (x,y,z) = x*x + y*y + z*z - r*r

f_torus :: Double -> Double -> XYZ -> Double
f_torus rM rm (x,y,z) = (rM - sqrt(x*x+y*y))^2 + z*z - rm*rm

f1 :: XYZ -> Double
f1 (x,y,z) = f_sphere 0.6 (x, y - 0.5 * abs x ** 0.8, z)

f2 :: XYZ -> Double
f2 (x,y,z) = f_torus 0.8 0.1 (x, y - 0.5 * abs x ** 0.8, z)

voxel1 :: Voxel
voxel1 = makeVoxel f1 ((-0.7,0.7),(-0.7,0.8),(-0.7,0.7))
                      (100, 100, 100)
voxmax1 :: Double
voxmax1 = voxelMax voxel1

voxel2 :: Voxel
voxel2 = makeVoxel f2 ((-1.2,1.2),(-1.0,2.0),(-0.2,0.2))
                      (150, 200, 100)
voxmax2 :: Double
voxmax2 = voxelMax voxel2

triangles1 :: [NTriangle]
{-# NOINLINE triangles1 #-}
triangles1 = unsafePerformIO $ do
  triangles <- computeContour3d'' voxel1 (Just voxmax1) 0.0 False
  return $ map fromTriangle triangles

triangles2 :: [NTriangle]
{-# NOINLINE triangles2 #-}
triangles2 = unsafePerformIO $ do
  triangles <- computeContour3d'' voxel2 (Just voxmax2) 0.0 False
  return $ map fromTriangle triangles

display :: Context -> IORef GLfloat -> DisplayCallback
display context alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate alpha' $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ do
    materialDiffuse FrontAndBack $= red
    mapM_ drawTriangle (triangles1 ++ triangles2)
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), norm) = do
      normal norm
      vertex v1
      vertex v2
      vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-4+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
idle anim alpha snapshots = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/valentine%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      alpha $~! (+ 1.0)
      snapshots $~! (+ 1)
    postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Valentine heart"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  alpha <- newIORef 0.0
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
                             alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim)
  idleCallback $= Just (idle anim alpha snapshots)
  putStrLn "*** Valentine heart ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \"
  mainLoop
