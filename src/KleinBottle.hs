module KleinBottle
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT              hiding (Triangle)
import           MarchingCubes
import           MarchingCubes.Utils.Triangles (triangleNorm2Center)
import           System.IO.Unsafe
import           Utils.OpenGL
import           Utils.Palettes

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    }

funColor :: Double -> Triangle -> Color4 GLfloat
funColor dmax triangle = colors !! j
  where
  colors = colorRamp' "viridis" 256
  d = triangleNorm2Center triangle
  j = floor (d*255/dmax)

fKB :: XYZ -> Double
fKB (x,y,z) = (x2y2z2 + 2*y -1)*(a*a - 8*z2) + 16*x*z*a
  where
  z2 = z*z
  x2y2z2 = x*x + y*y + z2
  a = x2y2z2 - 2*y - 1

voxel :: Voxel
voxel = makeVoxel fKB ((-2.7,3.2),(-2.2,3.3),(-3.5,3.9))
                      (100, 100, 150)

voxmax :: Double
voxmax = voxelMax voxel

trianglesKB :: ([NTriangle], [Color4 GLfloat])
{-# NOINLINE trianglesKB #-}
trianglesKB = unsafePerformIO $ do
  (triangles, d2max) <- computeContour3d''' voxel (Just voxmax) 0.0 True
  let ntriangles = map fromTriangle triangles
      colors = map (funColor d2max) triangles
  return (ntriangles, colors)


display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  let (triangles, colors) = trianglesKB
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
  lookAt (Vertex3 0 0 (-12+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
  _ <- createWindow "Klein Bottle"
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
--  trianglesRef <- newIORef trianglesKB
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom
--                                      contextTriangles = trianglesRef
                                    }
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Klein bottle ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
