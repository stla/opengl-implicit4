module Mesh.Normals
  (normals)
  where
import           Data.IntMap.Strict  (IntMap, unionWith)
import qualified Data.IntMap.Strict  as M
import           Data.List           (foldl')
import           Data.Vector.Unboxed (Unbox, Vector, (!))
import           Linear              (V3 (..), cross, signorm, (^+^), (^-^))

-- vertices :: Vector (Float,Float,Float)
-- vertices = fromList [(1,1,1), (-1,1,-1), (-1,-1,1), (1,-1,-1)]
--
-- faces :: [[Int]]
-- faces = [[0,1,2], [0,2,3], [0,3,1], [3,2,1]]

toV3 :: Floating a => (a,a,a) -> V3 a
toV3 (x,y,z) = V3 x y z

normal :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a) -> V3 a
normal v1 v2 v3 = signorm $ cross (v2' ^-^ v1') (v3' ^-^ v1')
  where
  v1' = toV3 v1
  v2' = toV3 v2
  v3' = toV3 v3

faceNormals :: (Floating a, Unbox a) => Vector (a,a,a) -> [Int] -> IntMap (V3 a)
faceNormals vs face = M.fromList nrmls
  where
  nrml = normal (vs ! (face !! 0)) (vs ! (face !! 1)) (vs ! (face !! 2))
  nrmls = map (\i -> (i, nrml)) face

normals :: (Floating a, Unbox a) => Vector (a,a,a) -> [[Int]] -> IntMap (V3 a)
normals vs faces =
  M.map signorm (foldl' (unionWith (^+^)) M.empty (map (faceNormals vs) faces))
