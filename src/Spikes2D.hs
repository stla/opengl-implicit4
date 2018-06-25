module Spikes2D
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

whitesmoke :: Color4 GLfloat
whitesmoke = Color4 0.96 0.96 0.96 1

fSpikes2D :: Double -- Height of central spike
          -> Double -- Frequency of spikes in the X direction
          -> Double -- Frequency of spikes in the Z direction
          -> Double -- Rate at which the spikes reduce as you move away from the center
          -> XYZ
          -> Double
fSpikes2D a b c d (x,y,z) =
  -(a * cos(b*x) * cos(c*z) * exp(-d*(x*x+z*z)) - y)

voxel :: Double -> Double -> Double -> Double -> Voxel
voxel a b c d = makeVoxel (fSpikes2D a b c d)
                          ((-1.1,1.1),(-a,a),(-1.1,1.1))
                          (100, 100, 100)

trianglesSpikes2D :: Voxel -> IO [NTriangle]
trianglesSpikes2D vxl = do
  triangles <- computeContour3d'' vxl Nothing 0.0 True
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
  rotate (r1+135) $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ do
    materialDiffuse FrontAndBack $= whitesmoke
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
  lookAt (Vertex3 0 0 (-4+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -> IORef Double -> IORef Double -> IORef Double -- parameters a, b, c and d
         -> IORef Voxel
         -> IORef [NTriangle]
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b c d voxelRef trianglesRef zoom char _ = do
  case char of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'd' -> do
             a $~! (+ 0.1)
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'c' -> do
             a $~! subtract 0.1
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'f' -> do
             b $~! (+ 0.1)
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'v' -> do
             b $~! subtract 0.1
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'g' -> do
             c $~! (+ 0.1)
             c' <- get c
             a' <- get a
             b' <- get b
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'b' -> do
             c $~! subtract 0.1
             c' <- get c
             a' <- get a
             b' <- get b
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'h' -> do
             d $~! (+ 0.5)
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             writeIORef voxelRef vxl
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'n' -> do
             d $~! subtract 0.5
             a' <- get a
             b' <- get b
             c' <- get c
             d' <- get d
             let vxl = voxel a' b' c' d'
             triangles <- trianglesSpikes2D vxl
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Spikes2D"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= white
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  a <- newIORef 0.4
  b <- newIORef 15.0
  c <- newIORef 15.0
  d <- newIORef 2.5
  let vxl = voxel 0.4 15 15 2.5
  voxelRef <- newIORef vxl
  triangles <- trianglesSpikes2D vxl
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b c d voxelRef trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Spikes2D ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        d, c, f, v, g, b, h, n\n\
        \"
  mainLoop
