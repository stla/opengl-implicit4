module Spiral
  ( main )
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Text.Printf
import           Utils.Misc
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

purple :: Color4 GLfloat
purple = Color4 0.63 0.13 0.94 1

fSpiral :: Double -- Distance between windings
        -> Double -- Thickness
        -> Double -- Cross section: 0 square 1 circle 2 diamond 3 concave diamond
        -> XYZ -> Double
fSpiral a b c (x,y,z) = - (min (1 - r'') (b - min r2' r''))
  where
    x2 = x*x
    y2 = y*y
    z2 = z*z
    r = sqrt(x2+z2)
    th = atan2 z x
    r' = r + a * th / 2 / pi
    r2 = modulo r' a - a * 0.5
    r2' | c == 1 = sqrt(r2 * r2 + y2)
        | c /= 0 = (abs r2 ** temp + abs y ** temp) ** (1/temp)
        | otherwise = max (abs r2) (abs y)
      where
      temp = 2/c
    r'' = sqrt (x2+y2+z2)

voxel :: Double -> Double -> Double -> Voxel
voxel a b c = makeVoxel (fSpiral a b c)
                        ((-1,1.1),(-0.2,0.2),(-1,1.1))
                        (200, 100, 200)

trianglesSpiral :: Voxel -> IO [NTriangle]
trianglesSpiral vxl = do
  triangles <- computeContour3d'' vxl Nothing 0.0 True
  return $ map fromTriangle triangles

display :: Context -> IORef GLfloat -> DisplayCallback
display context alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
--  rotate alpha' $ Vector3 0 1 0
  rotate (r1+130) $ Vector3 1 0 0
  rotate (r2+alpha') $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ do
    materialDiffuse FrontAndBack $= purple
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
         -> IORef Double -> IORef Double -> IORef Double -- parameters a, b and c
         -> IORef Voxel
         -> IORef [NTriangle]
         -> IORef Double -- zoom
         -> IORef Bool -- anim
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b c voxelRef trianglesRef zoom anim char _ = do
  case char of
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
             a $~! (+ 0.025)
             a' <- get a
             b' <- get b
             c' <- get c
             let vxl = voxel a' b' c'
             writeIORef voxelRef vxl
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'v' -> do
             a $~! subtract 0.025
             a' <- get a
             b' <- get b
             c' <- get c
             let vxl = voxel a' b' c'
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'g' -> do
             b $~! (+ 0.1)
             a' <- get a
             b' <- get b
             c' <- get c
             let vxl = voxel a' b' c'
             writeIORef voxelRef vxl
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'b' -> do
             b $~! subtract 0.1
             a' <- get a
             b' <- get b
             c' <- get c
             let vxl = voxel a' b' c'
             writeIORef voxelRef vxl
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'h' -> do
             c $~! (\x -> if x<=2 then x+1 else 0)
             c' <- get c
             a' <- get a
             b' <- get b
             let vxl = voxel a' b' c'
             writeIORef voxelRef vxl
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'n' -> do
             c $~! (\x -> if x>=1 then x-1 else 3)
             c' <- get c
             a' <- get a
             b' <- get b
             let vxl = voxel a' b' c'
             writeIORef voxelRef vxl
             triangles <- trianglesSpiral vxl
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef GLfloat -> IORef Int -> IdleCallback
idle anim alpha snapshots = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/spiral%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      alpha $~! (+ 1.0)
      snapshots $~! (+ 1)
    postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Spiral"
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
  a <- newIORef 0.3
  b <- newIORef 0.1
  c <- newIORef 1.0
  let vxl = voxel 0.3 0.1 1.0
  voxelRef <- newIORef vxl
  triangles <- trianglesSpiral vxl
  trianglesRef <- newIORef triangles
  alpha <- newIORef 0.0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextVoxel = voxelRef,
                                      contextTriangles = trianglesRef}
                             alpha
  reshapeCallback $= Just (resize 0)
  anim <- newIORef False
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b c voxelRef trianglesRef zoom anim)
  snapshots <- newIORef 0
  idleCallback $= Just (idle anim alpha snapshots)
  putStrLn "*** Spiral ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b, h, n\n\
        \    Animation: a\n\
        \"
  mainLoop
