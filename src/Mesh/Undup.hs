module Mesh.Undup
  (undupMesh)
  where
import           Data.Maybe
import           Data.Vector.Unboxed (Unbox, Vector, elemIndex, uniq, (!))
import qualified Data.Vector.Unboxed as VU

unique :: (Eq a, Unbox a) => Vector a -> (Vector a, Vector Int)
unique vs = (vsnub, indices)
  where
  vsnub = uniq vs
  indices = VU.map (\v -> fromJust $ elemIndex v vsnub) vs

undupMesh :: (Unbox a, Eq a) => (Vector a, [[Int]]) -> (Vector a, [[Int]])
undupMesh (vs, faces) = (newvs, newfaces)
  where
  (newvs, idx) = unique vs
  newfaces = map (map (idx !)) faces
