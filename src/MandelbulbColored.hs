module MandelbulbColored
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL hiding (color)
import           Graphics.UI.GLUT       hiding (Triangle, color)
import           MarchingCubes
import           MarchingCubes.Utils.Triangles (triangleNorm2Center)
import           Utils.OpenGL
import           Utils.Palettes

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef [NTriangle]
    , contextColors    :: IORef [Color4 GLfloat]
    }

fMandelbulb :: XYZ -> Double
fMandelbulb p0@(x0,y0,z0) = if ssq p0 >= 4 then 0/0 else go 24 p0 (ssq p0)
  where
  ssq (x,y,z) = x*x + y*y + z*z
  go :: Int -> XYZ -> Double -> Double
  go n (x,y,z) r2 =
    if r2 > 4
      then sqrt r2
      else
        let theta = 8 * atan2 (sqrt(x*x+y*y)) z in
        let phi = 8 * atan2 y x in
        let r8 = r2*r2*r2*r2 in
        let xyz = ( r8 * cos phi * sin theta + x0
                  , r8 * sin phi * sin theta + y0
                  , r8 * cos theta + z0) in
        if n>1 then go (n-1) xyz (ssq xyz) else sqrt r2

voxel :: Voxel
voxel = makeVoxel fMandelbulb ((-1.1,1.1),(-1.1,1.1),(-1.1,1.1))
                  (50, 50, 50)

trianglesMandelbulb :: IO ([Triangle], Double)
trianglesMandelbulb = computeContour3d''' voxel Nothing 1 True

funColor :: Int -> Double -> Triangle -> Color4 GLfloat
funColor colormap dmax triangle = palette !! j
  where
    d = triangleNorm2Center triangle
    j = floor (d*255/dmax)
    palettes = ["inferno", "magma", "plasma", "viridis", "cviridis"]
    palette = colorRamp' (palettes !! colormap) 256

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
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
    drawTriangle (((v1,v2,v3), norm), color) = do
      materialDiffuse Front $= color
      normal $ negateNormal norm
      vertex v1
      vertex v2
      vertex v3

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
  _ <- createWindow "Mandelbulb"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  -- lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Front
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  triangles <- trianglesMandelbulb
  trianglesRef <- newIORef (map fromTriangle (fst triangles))
  colorsRef <- newIORef (map (funColor 0 (snd triangles)) (fst triangles))
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = trianglesRef,
                                      contextColors = colorsRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Mandelbulb ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
