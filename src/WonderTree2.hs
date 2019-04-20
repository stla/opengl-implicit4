module WonderTree2
  ( main )
  where
import           Data.IORef
import           Data.Vector.Unboxed          (Vector, (!))
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
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

fWonderTree :: XYZ -> Double
fWonderTree (x,y,z) =
  cos(4*x/(xyz+0.0001)) + sin(4*y/(xyz+0.0001)) +
  cos(4*y/(xyz+0.0001)) + sin(4*z/(xyz+0.0001)) +
  cos(4*z/(xyz+0.0001)) + sin(4*x/(xyz+0.0001)) +
  exp(0.1*(xyz-0.2)) - exp(-10*(xyz-0.15))
  where
  xyz = x*x+y*y+z*z

voxel :: Voxel
voxel = makeVoxel fWonderTree ((-1.9,1.3),(-1.9,1.3),(-1.9,1.3))
                              (15, 15, 15)

wonderTree :: ((Vector XYZ, [[Int]]), [XYZ])
{-# NOINLINE wonderTree #-}
wonderTree = unsafePerformIO $ computeContour3d' voxel Nothing 0.0 True

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  let vertices = fst $ fst wonderTree
      faces = snd $ fst wonderTree
      normals = snd wonderTree
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ (drawTriangle vertices faces normals) [0 .. length faces - 1]
  swapBuffers
  where
    drawTriangle vs fs ns i = do
      let face = fs !! i
          j0 = face !! 0
          j1 = face !! 1
          j2 = face !! 2
      materialDiffuse FrontAndBack $= navy
      normal (toNormal $ ns !! j0)
      vertex (toVertex $ vs ! j0)
      normal (toNormal $ ns !! j1)
      vertex (toVertex $ vs ! j1)
      normal (toNormal $ ns !! j2)
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
  lookAt (Vertex3 0 0 (-7+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom char _ = do
  case char of
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
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Wonder tree ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
