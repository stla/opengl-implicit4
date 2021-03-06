module TogliattiSmooth
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Utils.OpenGL
import           MultiPol

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef [NNNTriangle]
    }

fuchsia :: Color4 GLfloat
fuchsia = Color4 1.00 0.00 1.00 1
discord :: Color4 GLfloat
discord = Color4 0.21 0.22 0.25 1

f :: XYZ -> Double
f (x,y,z) = if x2+y2+z2<24
  then 64*(x-w)*
       (sqr x2 -4*x2*x*w-10*x2*y2-4*x2*w2+16*x*w2*w-20*x*y2*w+5*sqr y2+16*sqr w2-20*y2*w2) -
       5*sqrt(5-sqrt 5)*(2*z-sqrt(5-sqrt 5)*w)*sqr(4*(x2+y2-z2)+(1+3*sqrt 5)*w2)
  else 0/0
  where
  w = 1
  w2 = w*w
  x2 = x*x
  y2 = y*y
  z2 = z*z
  sqr u = u*u

togliatti :: Polynomial
togliatti = ((M (Monomial 64 (1,0,0)) :+: M (Monomial (-64*w) (0,0,0))) :*:
            fromListOfMonomials [ Monomial 1 (4,0,0)
                                 , Monomial (-4*w) (3,0,0)
                                 , Monomial (-10) (2,2,0)
                                 , Monomial (-4*w*w) (2,0,0)
                                 , Monomial (16*w*w*w) (1,0,0)
                                 , Monomial (-20*w) (1,2,0)
                                 , Monomial 5 (0,4,0)
                                 , Monomial (16*w*w*w*w) (0,0,0)
                                 , Monomial (-20*w*w) (0,2,0)]) :+:
            ((M (Monomial (-10*c) (0,0,1)) :+: M (Monomial (5*c*c*w) (0,0,0))) :*:
            fromListOfMonomials [Monomial 4 (2,0,0)
                               , Monomial 4 (0,2,0)
                               , Monomial (-4) (0,0,2)
                               , Monomial ((1+3*sqrt 5)*w*w) (0,0,0)] :*:
            fromListOfMonomials [Monomial 4 (2,0,0)
                               , Monomial 4 (0,2,0)
                               , Monomial (-4) (0,0,2)
                               , Monomial ((1+3*sqrt 5)*w*w) (0,0,0)])
  where
    w = 1
    c = sqrt(5 - sqrt 5)

dxTogliatti :: Polynomial
dxTogliatti = derivPoly togliatti 'x'

dxTogliatti' :: (Double, Double, Double) -> Double
dxTogliatti' = evalPoly dxTogliatti

dyTogliatti :: Polynomial
dyTogliatti = derivPoly togliatti 'y'

dyTogliatti' :: (Double, Double, Double) -> Double
dyTogliatti' = evalPoly dyTogliatti

dzTogliatti :: Polynomial
dzTogliatti = derivPoly togliatti 'z'

dzTogliatti' :: (Double, Double, Double) -> Double
dzTogliatti' = evalPoly dzTogliatti

gradient :: (Double, Double, Double) -> (Double, Double, Double)
gradient xyz = (dxTogliatti' xyz, dyTogliatti' xyz, dzTogliatti' xyz)

voxel :: Voxel
voxel = makeVoxel f ((-5,5),(-5,5),(-4,4)) (150, 150, 150)

voxmax :: Double
voxmax = voxelMax voxel

trianglesTogli :: Double -> IO [NNNTriangle]
trianglesTogli level = do
  triangles <- computeContour3d'' voxel (Just voxmax) level False
  return $ map (fromTriangle' gradient) triangles

display :: Context -> DisplayCallback
display context = do
  -- clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  clear [ColorBuffer, DepthBuffer]
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), (n1,n2,n3)) = do
      materialDiffuse FrontAndBack $= fuchsia
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
  lookAt (Vertex3 0 0 (-13+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- isolevel
         -> IORef [NNNTriangle]
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
             triangles <- trianglesTogli l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! (\x -> if x>=0.1 then x-0.1 else x)
             l' <- get l
             putStrLn ("l': " ++ show l')
             triangles <- trianglesTogli l'
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Togliatti surface"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialAmbient FrontAndBack $= black
  materialDiffuse FrontAndBack $= white
  materialEmission FrontAndBack $= Color4 0 0 0 0
  materialSpecular FrontAndBack $= white
  materialShininess FrontAndBack $= 50
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-1000) 1
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  lightModelAmbient $= Color4 0.35 0.35 0.35 1
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Nothing
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  level <- newIORef 0.0
  zoom <- newIORef 0.0
  triangles <- trianglesTogli 0.0
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 level trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Togliatti surface ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease isolevel: h, n\n\
        \"
  mainLoop
