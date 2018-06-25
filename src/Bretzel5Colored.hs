module Bretzel5Colored
  ( main )
  where
-- import           Control.Monad                 (when)
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT              hiding (Triangle)
import           MarchingCubes
import           MarchingCubes.Utils.Triangles (triangleNorm2Center)
import           Utils.OpenGL
import           Utils.Palettes

funColor :: Double -> Triangle -> Color4 GLfloat
funColor dmax triangle = colors !! j
  where
  colors = colorRamp' "inferno" 256
  d = triangleNorm2Center triangle
  j = floor (d*255/dmax)

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef ([NTriangle], [Color4 GLfloat])
    }

fBretz :: XYZ -> Double
fBretz (x,y,z) = ((x2+y2/4-1)*(x2/4+y2-1))^2 + z*z
  where
  x2 = x*x
  y2 = y*y

voxel :: Voxel
voxel = makeVoxel fBretz ((-2.5,2.5),(-2.5,2.5),(-1,1))
                         (200, 200, 200)

voxmax :: Double
voxmax = voxelMax voxel

trianglesBretz :: Double -> IO ([NTriangle], [Color4 GLfloat])
trianglesBretz level = do
  (triangles, d2max) <- computeContour3d''' voxel (Just voxmax) level False
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
      -- let ((v1,v2,v3), norm) = fromTriangle triangle
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
  lookAt (Vertex3 0 0 (-6+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- isolevel
         -> IORef ([NTriangle], [Color4 GLfloat])
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 l trianglesRef zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'h' -> do
             l $~! (+ 0.1)
             l' <- get l
             triangles <- trianglesBretz l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! (\x -> if x>0.1 then x-0.1 else x)
             l' <- get l
             putStrLn ("l': " ++ show l')
             if l'>1e-16
              then do
               triangles <- trianglesBretz l'
               writeIORef trianglesRef triangles
              else l $~! (+ 0.1)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Bretzel"
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
  level <- newIORef 0.1
  zoom <- newIORef 0.0
  triangles <- trianglesBretz 0.1
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 level trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Bretzel ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease isolevel: h, n\n\
        \"
  mainLoop
