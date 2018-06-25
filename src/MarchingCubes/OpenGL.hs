module MarchingCubes.OpenGL
  where
import           Graphics.Rendering.OpenGL.GL  (Vertex3 (..))
import           MarchingCubes.Utils.Triangles (Triangle)
import           Utils.OpenGL                  (NTriangle, triangleNormal)

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
