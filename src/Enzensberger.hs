module Enzensberger
  ( main )
  where
import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import qualified Data.ByteString as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           System.Directory (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf
import           Utils.OpenGL

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextAlpha     :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef [NNNTriangle]
    }

fuchsia :: Color4 GLfloat
fuchsia = Color4 1.00 0.00 1.00 1
discord :: Color4 GLfloat
discord = Color4 0.21 0.22 0.25 1

f :: XYZ -> Double
f (x,y,z) = 400*(x2*y2 + y2*z2 + x2*z2) - (1 - x2 - y2 - z2)^(3 :: Int)
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z

gradient :: XYZ -> XYZ
gradient (x,y,z) =
  (
    6*x*(1 - x2 - y2 - z2)^(2 :: Int) + 800*x*(y2 + z2),
    6*y*(1 - x2 - y2 - z2)^(2 :: Int) + 800*y*(x2 + z2),
    6*z*(1 - x2 - y2 - z2)^(2 :: Int) + 800*z*(x2 + y2)
  )
  where
    x2 = x*x
    y2 = y*y
    z2 = z*z

voxel :: Voxel
voxel = makeVoxel f ((-0.9,0.9),(-0.9,0.9),(-0.9,0.9)) (150, 150, 150)

voxmax :: Double
voxmax = voxelMax voxel

mesh :: IO [NNNTriangle]
mesh = do
  triangles <- computeContour3d'' voxel (Just voxmax) 0.0 True
  return $ map (fromTriangle' gradient) triangles

display :: Context -> DisplayCallback
display context = do
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  alpha <- get (contextAlpha context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  clear [ColorBuffer, DepthBuffer]
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha $ Vector3 1 1 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), (n1,n2,n3)) = do
      materialDiffuse Front $= fuchsia
      normal (negateNormal n1)
      vertex v1
      normal (negateNormal n3)
      vertex v3
      normal (negateNormal n2)
      vertex v2

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> IORef Bool -- animation
         -> IORef Bool -- save
         -> IORef Int -- delay
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim save delay c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.5)
    'l' -> zoom $~! subtract 0.5
    'a' -> anim $~! not
    's' -> save $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-10000)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle :: IORef Bool
     -> IORef Bool
     -> IORef Int
     -> IORef Int
     -> IORef GLfloat
     -> IdleCallback
idle anim save delay snapshots alpha = do
  a <- get anim
  snapshot <- get snapshots
  s <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && snapshot < 360) $ do
      let ppm = printf "ppm/Enzensberger%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+ 1)
    alpha $~! (+ 1)
    _ <- threadDelay d
    postRedisplay Nothing
  return ()


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Enzensberger star"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialAmbient Front $= black
  materialDiffuse Front $= white
  materialEmission Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-1000) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  alpha <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 0
  triangles <- mesh 
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextAlpha = alpha,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim save delay)
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot alpha)
  putStrLn "*** Enzensberger star ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
