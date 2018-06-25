module SuperEllipsoid
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT hiding (Triangle)
import           MarchingCubes
import           Utils.OpenGL
import           MarchingCubes.Utils.Triangles (triangleNorm2Center)
import           Utils.Palettes

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextVoxel     :: IORef Voxel
    , contextTriangles :: IORef ([NTriangle], [Color4 GLfloat])
    }

funColor :: Double -> Triangle -> Color4 GLfloat
funColor dmax triangle = colors !! j
  where
  colors = colorRamp' "viridis" 256
  d = triangleNorm2Center triangle
  j = floor (d*255/dmax)

fSE :: Double -> Double -> XYZ -> Double
fSE n1 n2 (x,y,z) =
  (abs x ** (2/n2) + abs y ** (2/n2)) ** (n2/n1) + abs z ** (2/n1)

voxel :: Double -> Double -> Voxel
voxel n1 n2 = makeVoxel (fSE n1 n2) ((-1.5,1.5),(-1.5,1.5),(-1.5,1.5))
                        (100, 100, 100)

trianglesSE :: Voxel -> IO ([NTriangle], [Color4 GLfloat])
trianglesSE vxl = do
  (triangles, d2max) <- computeContour3d''' vxl Nothing 1.0 False
  let ntriangles = map fromTriangle triangles
      colors = map (funColor d2max) triangles
  return (ntriangles, colors)

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  (triangles, colors) <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle (zip triangles colors)
  swapBuffers
  where
    drawTriangle (((v1,v2,v3), norm), col) = do
      materialDiffuse FrontAndBack $= col
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
         -> IORef Double -> IORef Double -- parameters n1 and n2
         -> IORef Voxel
         -> IORef ([NTriangle], [Color4 GLfloat])
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 n1 n2 voxelRef trianglesRef zoom c _ = do
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
             n1 $~! (+ 0.2)
             n1' <- get n1
             n2' <- get n2
             let vxl = voxel n1' n2'
             writeIORef voxelRef vxl
             triangles <- trianglesSE vxl
             writeIORef trianglesRef triangles
    'v' -> do
             n1 $~! (\x -> if x-0.2 > 0 then x-0.2 else x)
             n1' <- get n1
             n2' <- get n2
             let vxl = voxel n1' n2'
             writeIORef voxelRef vxl
             triangles <- trianglesSE vxl
             writeIORef trianglesRef triangles
    'g' -> do
             n2 $~! (+ 0.2)
             n1' <- get n1
             n2' <- get n2
             let vxl = voxel n1' n2'
             writeIORef voxelRef vxl
             triangles <- trianglesSE vxl
             writeIORef trianglesRef triangles
    'b' -> do
             n2 $~! (\x -> if x-0.2 > 0 then x-0.2 else x)
             n1' <- get n1
             n2' <- get n2
             let vxl = voxel n1' n2'
             writeIORef voxelRef vxl
             triangles <- trianglesSE vxl
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Super ellipsoid"
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
  n1 <- newIORef 1.0
  n2 <- newIORef 2.0
  let vxl = voxel 1.0 2.0
  voxelRef <- newIORef vxl
  triangles <- trianglesSE vxl
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 n1 n2 voxelRef trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Super ellipsoid ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b\n\
        \"
  mainLoop
