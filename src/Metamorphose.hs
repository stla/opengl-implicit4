module Metamorphose
  (main)
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Math.Polynomial                   (polyDeriv, evalPoly)
import           Math.Polynomial.Chebyshev         (evalT, t)
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.OpenGL

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef [NNNTriangle]
    }

blue :: Color4 GLfloat
blue = Color4 0 0 1 1
whitesmoke :: Color4 GLfloat
whitesmoke = Color4 0.96 0.96 0.96 1
discord :: Color4 GLfloat
discord = Color4 0.21 0.22 0.25 1

fun1 :: XYZ -> Double
fun1 (x,y,z) = cheb4 (x/1.77) + cheb4 (y/1.77) + cheb4 (z/1.77)
  where
  cheb4 = evalT 6

gradient1 :: XYZ -> XYZ
gradient1 (x,y,z) = (1/1.77 * cheb4' (x/1.77), 1/1.77 * cheb4' (y/1.77), 1/1.77 * cheb4' (z/1.77))
  where
    cheb4' = evalPoly (polyDeriv (t 6))

fun2 :: XYZ -> Double
fun2 (x,y,z) = x*x*x*x-5*x*x+y*y*y*y-5*y*y+z*z*z*z-5*z*z+11.8

gradient2 :: XYZ -> XYZ
gradient2 (x,y,z) =
  (
    4*x*x*x-10*x,
    4*y*y*y-10*y,
    4*z*z*z-10*z
  )

doubleVoxel :: DoubleVoxel
doubleVoxel = makeDoubleVoxel fun1 fun2
                              ((-2.3,2.3),(-2.3,2.3),(-2.3,2.3))
                              (100, 100, 100)
voxel :: Double -> Voxel
voxel = voxelAverage doubleVoxel

gradient :: Double -> XYZ -> XYZ
gradient lambda xyz =
  (
    lambda*x1 + (1-lambda)*x2,
    lambda*y1 + (1-lambda)*y2,
    lambda*z1 + (1-lambda)*z2
  )
  where
    (x1,y1,z1) = gradient1 xyz
    (x2,y2,z2) = gradient2 xyz

trianglesBC :: Double -> Double -> IO [NNNTriangle]
trianglesBC lambda l = do
  triangles <- computeContour3d'' (voxel lambda) Nothing l False
  return $ map (fromTriangle' (gradient lambda)) triangles

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
    materialDiffuse FrontAndBack $= whitesmoke
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), (n1,n2,n3)) = do
        normal n1
        vertex v1
        normal n2
        vertex v2
        normal n3
        vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-9+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- parameter lambda
         -> IORef Double -- isolevel
         -> IORef [NNNTriangle]
         -> IORef Double -- zoom
         -> IORef GLint -- screenshot
         -> IORef Bool -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 lambda l trianglesRef zoom capture anim c _ = do
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
             lambda $~! (\x -> if x==1 then 1 else x+0.01)
             lambda' <- get lambda
             l' <- get l
             triangles <- trianglesBC lambda' l'
             writeIORef trianglesRef triangles
    'v' -> do
             lambda $~! (\x -> if x==0 then 0 else x-0.01)
             lambda' <- get lambda
             l' <- get l
             triangles <- trianglesBC lambda' l'
             writeIORef trianglesRef triangles
    'h' -> do
             l $~! (+ 0.1)
             l' <- get l
             lambda' <- get lambda
             triangles <- trianglesBC lambda' l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! subtract 0.1
             l' <- get l
             lambda' <- get lambda
             triangles <- trianglesBC lambda' l'
             writeIORef trianglesRef triangles
    'c' -> do
      i <- get capture
      let ppm = printf "png/pic%04d.ppm" i
          png = printf "png/Metamorphose%04d.png" i
      (>>=) capturePPM (B.writeFile ppm)
      convert ppm png True
      capture $~! (+1)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


idle :: IORef Bool -> IORef Int -> IORef Double -> IORef Double
     -> IORef [NNNTriangle] -> IdleCallback
idle anim snapshots lambda l trianglesRef = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s <= 100) $ do
        let ppm = printf "ppm/M%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      lambda $~! (+ 0.01)
      snapshots $~! (+ 1)
      l' <- get l
      lambda' <- get lambda
      triangles <- trianglesBC lambda' l'
      writeIORef trianglesRef triangles
    postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Metamorphose"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialAmbient FrontAndBack $= white
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
  lambda <- newIORef 0.0
  l <- newIORef 0.0
  triangles <- trianglesBC 0.0 0.0
  trianglesRef <- newIORef triangles
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  capture <- newIORef 0
  anim <- newIORef False
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 lambda l trianglesRef zoom capture anim)
  idleCallback $= Just (idle anim snapshots lambda l trianglesRef)
  putStrLn "*** Metamorphose ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease lambda:\n\
        \        f, v\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \    Screenshot: c\n\
        \    Animation: a\n\
        \"
  mainLoop
