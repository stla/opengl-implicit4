module BanchoffChmutov
  (main)
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Math.Polynomial.Chebyshev         (evalT)
import           Text.Printf
import           Utils.ConvertPPM
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

function :: Int -> XYZ -> Double
function n (x,y,z) = cheb4 x + cheb4 y + cheb4 z
  where
  cheb4 = evalT n

voxel :: Int -> Voxel
voxel n = makeVoxel (function n) ((-1.3,1.3),(-1.3,1.3),(-1.3,1.3))
                    (100, 100, 100)

trianglesBC :: Voxel -> Double -> IO [NTriangle]
trianglesBC vxl l = do
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
         -> IORef Int -- parameter n
         -> IORef Double -- isolevel
         -> IORef Voxel
         -> IORef [NTriangle]
         -> IORef Double -- zoom
         -> IORef GLint -- screenshot
         -> KeyboardCallback
keyboard rot1 rot2 rot3 n l voxelRef trianglesRef zoom capture c _ = do
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
             n $~! (+ 1)
             n' <- get n
             let vxl = voxel n'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesBC vxl l'
             writeIORef trianglesRef triangles
    'v' -> do
             n $~! (\x -> if x==1 then x else x-1)
             n' <- get n
             let vxl = voxel n'
             writeIORef voxelRef vxl
             l' <- get l
             triangles <- trianglesBC vxl l'
             writeIORef trianglesRef triangles
    'h' -> do
             l $~! (+ 0.1)
             l' <- get l
             vxl <- get voxelRef
             triangles <- trianglesBC vxl l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! subtract 0.1
             l' <- get l
             vxl <- get voxelRef
             triangles <- trianglesBC vxl l'
             writeIORef trianglesRef triangles
    'c' -> do
      i <- get capture
      let ppm = printf "png/pic%04d.ppm" i
          png = printf "png/BanchoffChmutov%04d.png" i
      (>>=) capturePPM (B.writeFile ppm)
      convert ppm png True
      capture $~! (+1)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Banchoff-Chmutov surface"
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
  n <- newIORef 4
  let vxl = voxel 4
  voxelRef <- newIORef vxl
  l <- newIORef 0.0
  triangles <- trianglesBC vxl 0.0
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  capture <- newIORef 0
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 n l voxelRef trianglesRef zoom capture)
  idleCallback $= Nothing
  putStrLn "*** Decocube ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease degree:\n\
        \        f, v\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \    Screenshot: c\n\
        \"
  mainLoop
