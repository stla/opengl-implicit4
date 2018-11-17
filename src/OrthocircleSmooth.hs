module OrthocircleSmooth
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
    , contextTriangles :: IORef [NNNTriangle]
    }

whitesmoke :: Color4 GLfloat
whitesmoke = Color4 0.96 0.96 0.96 1

fOrthocircle :: Double -> Double -> XYZ -> Double
fOrthocircle a b (x,y,z) =
  (xy2*xy2 + z2) * (yz2*yz2 + x2) *
  (zx2*zx2 + y2) - a*a*(1 + b*(x2 + y2 + z2))
  where
    x2 = x*x
    y2 = y*y
    z2 = z*z
    xy2 = x2+y2-1
    yz2 = y2+z2-1
    zx2 = z2+x2-1

-- ((x^2+y^2-1)^2 + z^2) * ((y^2+z^2-1)^2 + x^2) * ((x^2+z^2-1)^2 + y^2) - a^2*(1+b*(x^2+y^2+z^2))

gradient :: Double -> Double -> XYZ -> XYZ
gradient a b (x,y,z) =
    (
       -2*a*a*b*x + 2*x*(xy2*xy2+z2)*(zx2*zx2+y2) +
       4*x*zx2*(xy2*xy2+z2)*(yz2*yz2+x2) +
       4*x*xy2*(zx2*zx2+y2)*(yz2*yz2+x2),
       -2*a*a*b*y + 2*y*(xy2*xy2+z2)*(yz2*yz2+x2) +
       4*y*yz2*(xy2*xy2+z2)*(zx2*zx2+y2) +
       4*y*xy2*(zx2*zx2+y2)*(yz2*yz2+x2),
       -2*a*a*b*z + 2*z*(zx2*zx2+y2)*(yz2*yz2+x2) +
       4*z*yz2*(xy2*xy2+z2)*(zx2*zx2+y2) +
       4*z*zx2*(xy2*xy2+z2)*(yz2*yz2+x2)
    )
    where
        x2 = x*x
        y2 = y*y
        z2 = z*z
        xy2 = x2+y2-1
        yz2 = y2+z2-1
        zx2 = z2+x2-1

voxel :: Double -> Double -> Voxel
voxel a b = makeVoxel (fOrthocircle a b)
                      ((-1.3,1.3),(-1.3,1.3),(-1.3,1.3))
                      (100, 100, 100)

trianglesOrthocircle :: Double -> Double -> Double -> IO [NNNTriangle]
trianglesOrthocircle a b l = do
  triangles <- computeContour3d'' (voxel a b) Nothing l True
  return $ map (fromTriangle' (gradient a b)) triangles

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
  lookAt (Vertex3 0 0 (-4+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -> IORef Double -- parameters a and b
         -> IORef Double -- isolevel
         -> IORef [NNNTriangle]
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b l trianglesRef zoom c _ = do
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
             a $~! (+ 0.025)
             a' <- get a
             b' <- get b
             l' <- get l
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'v' -> do
             a $~! subtract 0.025
             a' <- get a
             b' <- get b
             l' <- get l
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'g' -> do
             b $~! (+ 0.1)
             a' <- get a
             b' <- get b
             l' <- get l
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'b' -> do
             b $~! subtract 0.1
             a' <- get a
             b' <- get b
             l' <- get l
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'h' -> do
             l $~! (+ 0.02)
             l' <- get l
             a' <- get a
             b' <- get b
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! subtract 0.02
             l' <- get l
             a' <- get a
             b' <- get b
             triangles <- trianglesOrthocircle a' b' l'
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Orthocircle"
  windowSize $= Size 400 400
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
  a <- newIORef 0.075
  b <- newIORef 3.0
  l <- newIORef 0.0
  triangles <- trianglesOrthocircle 0.075 3.0 0.0
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b l trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Orthocircle ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b\n\
        \    Increase/decrease isolevel:\n\
        \        h, n\n\
        \"
  mainLoop
