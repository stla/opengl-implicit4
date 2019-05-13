module Mesh.Normals
  (normals, normals')
  where
import           Data.IntMap.Strict  (IntMap, unionWith, elems)
import qualified Data.IntMap.Strict  as M
import           Data.Foldable       (foldl')
import           Data.Sequence       (Seq)
import           Data.Vector.Unboxed (Unbox, Vector, (!), fromList)
import           Linear              (V3 (..), cross, signorm, (^+^), (^-^))

-- vertices :: Vector (Float,Float,Float)
-- vertices = fromList [(1,1,1), (-1,1,-1), (-1,-1,1), (1,-1,-1)]
--
-- faces :: [[Int]]
-- faces = [[0,1,2], [0,2,3], [0,3,1], [3,2,1]]

toV3 :: Floating a => (a,a,a) -> V3 a
toV3 (x,y,z) = V3 x y z

fromV3 :: Floating a => V3 a -> (a,a,a)
fromV3 (V3 x y z) = (x, y, z)

normal :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a) -> V3 a
normal v1 v2 v3 = signorm $ cross (v3' ^-^ v1') (v2' ^-^ v1')
  where
  v1' = toV3 v1
  v2' = toV3 v2
  v3' = toV3 v3

normal' :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a) -> V3 a
normal' v1 v2 v3 = signorm $ cross (v2' ^-^ v1') (v3' ^-^ v1')
  where
  v1' = toV3 v1
  v2' = toV3 v2
  v3' = toV3 v3

faceNormals :: (Floating a, Unbox a) => Vector (a,a,a) -> [Int] -> IntMap (V3 a)
faceNormals vs face = M.fromList nrmls
  where
  nrml = normal (vs ! (face !! 0)) (vs ! (face !! 1)) (vs ! (face !! 2))
  nrmls = map (\i -> (i, nrml)) face

normalsV3 :: (Floating a, Unbox a) => (Vector (a,a,a), [[Int]]) -> IntMap (V3 a)
normalsV3 (vs, faces) =
  M.map signorm (foldl' (unionWith (^+^)) M.empty (map (faceNormals vs) faces))

normals :: (Floating a, Unbox a) => (Vector (a,a,a), [[Int]]) -> [(a,a,a)]
normals mesh = map fromV3 (elems $ normalsV3 mesh)

faceNormals' :: (Floating a, Unbox a) => Vector (a,a,a) -> [Int] -> IntMap (V3 a)
faceNormals' vs face = M.fromList nrmls
  where
  nrml = normal' (vs ! (face !! 0)) (vs ! (face !! 1)) (vs ! (face !! 2))
  nrmls = map (\i -> (i, nrml)) face

normalsV3' :: (Floating a, Unbox a) => (Vector (a,a,a), Seq [Int]) -> IntMap (V3 a)
normalsV3' (vs, faces) =
  M.map signorm (foldl' (unionWith (^+^)) M.empty (fmap (faceNormals' vs) faces))

normals' :: (Floating a, Unbox a) => (Vector (a,a,a), Seq [Int]) -> Vector (a,a,a)
normals' mesh = fromList $ map fromV3 (elems $ normalsV3' mesh)
