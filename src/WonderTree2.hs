module WonderTree2
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes2
import           System.IO.Unsafe
import Data.Vector.Unboxed (Vector)

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    }

fWonderTree :: (Double,Double,Double) -> Double
fWonderTree (x,y,z) =
  cos(4*x/(xyz+0.0001)) + sin(4*y/(xyz+0.0001)) +
  cos(4*y/(xyz+0.0001)) + sin(4*z/(xyz+0.0001)) +
  cos(4*z/(xyz+0.0001)) + sin(4*x/(xyz+0.0001)) +
  exp(0.1*(xyz-0.2)) - exp(-10*(xyz-0.15))
  where
  xyz = x*x+y*y+z*z

voxel :: Voxel
voxel = makeVoxel fWonderTree ((-1.9,1.3),(-1.9,1.3),(-1.9,1.3))
                              (50, 50, 50)

wonderTree :: (Vector (Double, Double, Double), [[Int]])
{-# NOINLINE wonderTree #-}
wonderTree = unsafePerformIO $ computeContour3d' voxel Nothing 0.0 True

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  let triangles = fst trianglesWonderTree
  colors <- get (contextColors context)
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
    drawTriangle (triangle, color) = do
      let ((v1,v2,v3), norm) = fromTriangle triangle
      materialDiffuse FrontAndBack $= color
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
  lookAt (Vertex3 0 0 (-7+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef [Color4 GLfloat]
         -> IORef Int -- color palette
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 colorsRef p zoom char _ = do
  case char of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'p' -> do
             p $~! (\i -> (i+1) `mod` 5)
             p' <- get p
             let (triangles, d2max) = trianglesWonderTree
             writeIORef colorsRef $ map (funColor p' d2max) triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Wonder tree"
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
  colorsRef <- newIORef (map (funColor 0 (snd trianglesWonderTree)) (fst trianglesWonderTree))
  pRef <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextColors = colorsRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 colorsRef pRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Wonder tree ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Change color palette: p (this takes a while...) \n\
        \"
  mainLoop
