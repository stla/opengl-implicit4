module Goursat
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
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

red :: Color4 GLfloat
red = Color4 1 0 0 1

fGoursat :: Double -> Double -> XYZ -> Double
fGoursat a b (x,y,z) =
  x4 + y4 + z4 + a*x2y2z2*x2y2z2 + b*x2y2z2
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z
  x2y2z2 = x2+y2+z2
  x4 = x2*x2
  y4 = y2*y2
  z4 = z2*z2

voxel :: Double -> Double -> Voxel
voxel a b = makeVoxel (fGoursat a b) ((-3,3),(-3,3),(-3,3))
                      (200, 200, 200)

trianglesGoursat :: Voxel -> Double -> IO [NTriangle]
trianglesGoursat vxl l = do
  triangles <- computeContour3d'' vxl Nothing l False
  return $ map fromTriangle triangles

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), norm) = do
      materialDiffuse FrontAndBack $= red
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
  lookAt (Vertex3 0 0 (-10+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -> IORef Double -- parameters a and b
         -> IORef Double -- isolevel
         -> IORef Voxel
         -> IORef [NTriangle]
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b l voxelRef trianglesRef zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'f' -> do
             a $~! (+ 0.02)
             a' <- get a
             b' <- get b
             let vxl = voxel a' b'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'v' -> do
             a $~! subtract 0.02
             a' <- get a
             b' <- get b
             let vxl = voxel a' b'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'g' -> do
             b $~! (+ 0.03)
             a' <- get a
             b' <- get b
             let vxl = voxel a' b'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'b' -> do
             b $~! subtract 0.03
             a' <- get a
             b' <- get b
             let vxl = voxel a' b'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'h' -> do
             l $~! (+ 0.25)
             l' <- get l
             vxl <- get voxelRef
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! subtract 0.25
             l' <- get l
             vxl <- get voxelRef
             triangles <- trianglesGoursat vxl l'
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Goursat surface"
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
  a <- newIORef (-0.27)
  b <- newIORef (-0.5)
  l <- newIORef 2.0
  let vxl = voxel (-0.27) (-0.5)
  voxelRef <- newIORef vxl
  triangles <- trianglesGoursat vxl 2.0
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b l voxelRef trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Goursat surface ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \"
  mainLoop
