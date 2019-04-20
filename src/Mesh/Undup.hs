module Mesh.Undup
  (undupMesh)
  where
import           Data.Maybe
import           Data.Vector.Unboxed (Unbox, Vector, elemIndex, (!), cons)
import qualified Data.Vector.Unboxed as VU

nub :: (Eq a, Unbox a) => Vector a -> Vector a
nub v = if VU.null v
  then VU.empty
  else let x = VU.head v in cons x (nub (VU.filter (\y -> x/=y) (VU.tail v)))

unique :: (Eq a, Unbox a) => Vector a -> (Vector a, Vector Int)
unique vs = (vsnub, indices)
  where
  vsnub = nub vs
  indices = VU.map (\v -> fromJust $ elemIndex v vsnub) vs

undupMesh :: (Unbox a, Eq a) => (Vector a, [[Int]]) -> (Vector a, [[Int]])
undupMesh (vs, faces) = (newvs, newfaces)
  where
  (newvs, idx) = unique vs
  newfaces = map (map (idx !)) faces

-- vs :: Vector Double
-- vs = fromList [1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4]
--
-- faces :: [[Int]]
-- faces = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
--
-- mesh = undupMesh (vs, faces)
