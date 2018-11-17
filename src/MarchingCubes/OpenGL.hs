module MarchingCubes.OpenGL
  where
import           Graphics.Rendering.OpenGL.GL  (Vertex3 (..), Normal3 (..))
import           MarchingCubes.Utils.Triangles (Triangle)
import           Utils.OpenGL                  (NTriangle, NNNTriangle, negateNormal, normalize, triangleNormal)

type Gradient = (Double,Double,Double) -> (Double,Double,Double)

fromTriangle :: Triangle -> NTriangle
fromTriangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) = (triangle, n)
  where
  x1' = realToFrac x1
  y1' = realToFrac y1
  z1' = realToFrac z1
  x2' = realToFrac x2
  y2' = realToFrac y2
  z2' = realToFrac z2
  x3' = realToFrac x3
  y3' = realToFrac y3
  z3' = realToFrac z3
  triangle = (Vertex3 x1' y1' z1', Vertex3 x2' y2' z2', Vertex3 x3' y3' z3')
  n = triangleNormal triangle

fromTriangle' :: Gradient -> Triangle -> NNNTriangle
fromTriangle' gradient ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
  (triangle, (negateNormal n1, negateNormal n2, negateNormal n3))
  where
    x1' = realToFrac x1
    y1' = realToFrac y1
    z1' = realToFrac z1
    x2' = realToFrac x2
    y2' = realToFrac y2
    z2' = realToFrac z2
    x3' = realToFrac x3
    y3' = realToFrac y3
    z3' = realToFrac z3
    triangle = (Vertex3 x1' y1' z1', Vertex3 x2' y2' z2', Vertex3 x3' y3' z3')
    (n1x, n1y, n1z) = gradient (x1,y1,z1)
    n1 = normalize $ Normal3 (realToFrac n1x) (realToFrac n1y) (realToFrac n1z)
    (n2x, n2y, n2z) = gradient (x2,y2,z2)
    n2 = normalize $ Normal3 (realToFrac n2x) (realToFrac n2y) (realToFrac n2z)
    (n3x, n3y, n3z) = gradient (x3,y3,z3)
    n3 = normalize $ Normal3 (realToFrac n3x) (realToFrac n3y) (realToFrac n3z)
