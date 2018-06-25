module Decocube
  (main)
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Text.Printf
import           Utils.OpenGL

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextVoxel     :: IORef Voxel
    , contextTriangles :: IORef [NTriangle]
    }

blue :: Color4 GLfloat
blue = Color4 0 0 1 1

fDecocube :: Double -> XYZ -> Double
fDecocube a (x,y,z) =
  (x2y2a2*x2y2a2 + (z2-1)*(z2-1)) *
  (y2z2a2*y2z2a2 + (x2-1)*(x2-1)) *
  (z2x2a2*z2x2a2 + (y2-1)*(y2-1))
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z
  a2 = a*a
  x2y2a2 = x2+y2-a2
  y2z2a2 = y2+z2-a2
  z2x2a2 = z2+x2-a2

voxel :: Double -> Voxel
voxel a = makeVoxel (fDecocube a) ((-1.3,1.3),(-1.3,1.3),(-1.3,1.3))
                    (130, 130, 130)

trianglesDecocube :: Voxel -> Double -> IO [NTriangle]
trianglesDecocube vxl l = do
  triangles <- computeContour3d'' vxl Nothing l False
  return $ map fromTriangle triangles

display :: Context -> IORef GLfloat -> DisplayCallback
display context alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  rotate alpha' $ Vector3 0 1 0
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate alpha' $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ do
    materialDiffuse FrontAndBack $= blue
    mapM_ drawTriangle triangles
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
  lookAt (Vertex3 0 0 (-5+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- parameter a
         -> IORef Double -- isolevel
         -> IORef Voxel
         -> IORef [NTriangle]
         -> IORef Double -- zoom
         -> IORef Bool -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a l voxelRef trianglesRef zoom anim c _ = do
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
    'f' -> do
             a $~! (+ 0.025)
             a' <- get a
             let vxl = voxel a'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesDecocube vxl l'
             writeIORef trianglesRef triangles
    'v' -> do
             a $~! subtract 0.025
             a' <- get a
             let vxl = voxel a'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesDecocube vxl l'
             writeIORef trianglesRef triangles
    'h' -> do
             l $~! (+ 0.0025)
             l' <- get l
             vxl <- get voxelRef
             triangles <- trianglesDecocube vxl l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! (\x -> if x>0.0025 then x-0.0025 else x)
             l' <- get l
             putStrLn ("level: " ++ show l')
             if l'>1e-16
              then do
               vxl <- get voxelRef
               triangles <- trianglesDecocube vxl l'
               writeIORef trianglesRef triangles
              else l $~! (+ 0.0025)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef GLfloat -> IORef Int -> IdleCallback
idle anim alpha snapshots = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/decocube%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      alpha $~! (+ 1.0)
      snapshots $~! (+ 1)
    postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Decocube"
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
  a <- newIORef 0.95
  l <- newIORef 0.01
  let vxl = voxel 0.95
  voxelRef <- newIORef vxl
  triangles <- trianglesDecocube vxl 0.01
  trianglesRef <- newIORef triangles
  alpha <- newIORef 0.0
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
                             alpha
  reshapeCallback $= Just (resize 0)
  anim <- newIORef False
  keyboardCallback $=
    Just (keyboard rot1 rot2 rot3 a l voxelRef trianglesRef zoom anim)
  idleCallback $= Just (idle anim alpha snapshots)
  putStrLn "*** Decocube ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameter:\n\
        \        f, v\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \    Animation: a\n\
        \"
  mainLoop
