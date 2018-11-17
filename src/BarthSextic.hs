module BarthSextic
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

fuchsia :: Color4 GLfloat
fuchsia = Color4 1.00 0.00 1.00 1
discord :: Color4 GLfloat
discord = Color4 0.21 0.22 0.25 1

f :: XYZ -> Double
f (x,y,z) = if x2+y2+z2<3
  then 4*(phi2*x2-y2)*(phi2*y2-z2)*(phi2*z2-x2) - (1+2*phi)*(x2+y2+z2-1)*(x2+y2+z2-1)
  else 0/0
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z
  phi = (1+sqrt 5)/2
  phi2 = phi*phi

gradient :: XYZ -> XYZ
gradient (x,y,z) =
    (
        8*x*phi2*(z2*phi2-x2)*(y2*phi2-z2) - 8*x*(x2*phi2-y2)*(y2*phi2-z2) -
        4*x*(2*phi+1)*(x2+y2+z2-1),
        8*y*phi2*(x2*phi2-y2)*(z2*phi2-x2) - 8*y*(z2*phi2-x2)*(y2*phi2-z2) -
        4*y*(2*phi+1)*(x2+y2+z2-1),
        8*z*phi2*(x2*phi2-y2)*(y2*phi2-z2) - 8*z*(x2*phi2-y2)*(z2*phi2-x2) -
        4*z*(2*phi+1)*(x2+y2+z2-1)
    )
    where
        x2 = x*x
        y2 = y*y
        z2 = z*z
        phi = (1+sqrt 5)/2
        phi2 = phi*phi

voxel :: Voxel
voxel = makeVoxel f ((-1.8,1.8),(-1.8,1.8),(-1.8,1.8)) (150, 150, 150)

voxmax :: Double
voxmax = voxelMax voxel

trianglesBarth :: Double -> IO [NNNTriangle]
trianglesBarth level = do
  triangles <- computeContour3d'' voxel (Just voxmax) level True
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
      materialDiffuse Front $= fuchsia
      normal n1
      vertex v1
      normal n3
      vertex v3
      normal n2
      vertex v2

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
             triangles <- trianglesBarth l'
             writeIORef trianglesRef triangles
    'n' -> do
             l $~! (\x -> if x>=0.1 then x-0.1 else x)
             l' <- get l
             putStrLn ("l': " ++ show l')
             triangles <- trianglesBarth l'
             writeIORef trianglesRef triangles
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Barth Sextic"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBMode, DoubleBuffered, WithDepthBuffer]
  -- colorMask $= Color4 Enabled Disabled Enabled Enabled
  clearColor $= discord
  clientState ColorArray $= Disabled
  --colorMaterial $= Just (Front, Emission)
  materialAmbient Front $= black
  materialDiffuse Front $= white
  materialEmission Front $= Color4 0 0 0 0
  materialSpecular Front $= white
  materialShininess Front $= 50
  lighting $= Enabled
  -- lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 500 500 (-1000) 1
  --ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  -- attenuation (Light 0) $= (0.5,0.5,0.5)
--  spotDirection (Light 0) $= Normal3 0 1 0
  -- light (Light 1) $= Enabled
  -- position (Light 1) $= Vertex4 0 0 (1000) 1
  -- ambient (Light 1) $= white
  -- diffuse (Light 1) $= white
  -- specular (Light 1) $= white
  -- light (Light 2) $= Enabled
  -- position (Light 2) $= Vertex4 0 (-1000) 0 1
  -- ambient (Light 2) $= white
  -- diffuse (Light 2) $= white
  -- specular (Light 2) $= white
  -- light (Light 3) $= Enabled
  -- position (Light 3) $= Vertex4 0 (1000) 0 1
  -- ambient (Light 3) $= white
  -- diffuse (Light 3) $= white
  -- specular (Light 3) $= white
  -- light (Light 4) $= Enabled
  -- position (Light 4) $= Vertex4 (-1000) 0 0 1
  -- ambient (Light 4) $= white
  -- diffuse (Light 4) $= white
  -- specular (Light 4) $= white
  -- light (Light 5) $= Enabled
  -- position (Light 5) $= Vertex4 1000 0 0 1
  -- ambient (Light 5) $= white
  -- diffuse (Light 5) $= white
  -- specular (Light 5) $= white
  -- light (Light 6) $= Enabled
  -- position (Light 6) $= Vertex4 (0) (0) (0) 1
  -- ambient (Light 6) $= black
  -- diffuse (Light 6) $= white
  -- specular (Light 6) $= white
  lightModelAmbient $= Color4 0.35 0.35 0.35 1
  depthMask $= Enabled
  depthFunc $= Just Lequal
  shadeModel $= Smooth
  fog $= Disabled
  -- fogColor $= Color4 1 0 1 1
  -- fogCoordSrc $= FragmentDepth
  -- fogDistanceMode $= EyePlaneAbsolute
  polygonMode $= (Fill, Fill)
  polygonSmooth $= Enabled
  cullFace $= Just Front
  rescaleNormal $= Enabled
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  level <- newIORef 0.0
  zoom <- newIORef 0.0
  triangles <- trianglesBarth 0.0
  trianglesRef <- newIORef triangles
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 level trianglesRef zoom)
  idleCallback $= Nothing
  putStrLn "*** Barth sextic ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease isolevel: h, n\n\
        \"
  mainLoop
