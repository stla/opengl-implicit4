module MarchingCubes.Utils.Triangles
  where
import           Data.List.Split (chunksOf)

type XYZ = (Double,Double,Double)
type Triangle = (XYZ, XYZ, XYZ)

toTriangles :: [[Double]] -> [Triangle]
toTriangles trianglesAsList = map toTriangle (chunksOf 3 trianglesAsList)
  where
    toTriangle :: [[Double]] -> Triangle
    toTriangle triangleAsList = toTriplet (map toTriplet triangleAsList)
      where
      toTriplet [x,y,z] = (x,y,z)
      toTriplet _       = undefined

-- maxXYZ :: XYZ -> Double
-- maxXYZ (x,y,z) = maximum [x,y,z]

norm2XYZ :: XYZ -> Double
norm2XYZ (x,y,z) = x*x + y*y + z*z

triangleCenter :: Triangle -> XYZ
triangleCenter ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
  ((x1+x2+x3)/3, (y1+y2+y3)/3, (z1+z2+z3)/3)

triangleNorm2Center :: Triangle -> Double
triangleNorm2Center = norm2XYZ . triangleCenter
