{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh.ConnectedComponents3
  where
import           Control.Monad              ((<$!>))
import           Data.List                  (elemIndices, sort)
import           Data.List.Unique           (count_)
import           Data.Permute               (Permute, at, inverse)
import qualified Data.Permute               as P
import           Foreign.C.Types
import           Foreign.Marshal.Alloc      (free, mallocBytes)
import           Foreign.Marshal.Array      (peekArray, pokeArray)
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (sizeOf)
import           Math.Combinat.Permutations (Permutation, permuteList,
                                             toPermutation)

type Faces = [[Int]]

foreign import ccall unsafe "connComps" c_connComps
    :: Ptr CUInt -> CUInt -> IO (Ptr CInt)

connectedComponents :: Faces -> IO [Int]
connectedComponents faces = do
  let n = length faces
      faces' = P.sort (length faces) $ map sort faces
      pinv = inverse $ snd faces'
      pinvAsList = map (\i -> at pinv i + 1) [0 .. length faces - 1]
      pinvAsPerm = toPermutation pinvAsList
  facesPtr <- mallocBytes (3 * n * sizeOf (undefined :: CUInt))
  pokeArray facesPtr (map fromIntegral $ concat (fst faces'))
  result <- c_connComps facesPtr (fromIntegral n)
  free facesPtr
  (<$!>) (permuteList pinvAsPerm . map fromIntegral) (peekArray n result)

biggestComponent :: Faces -> IO Faces
biggestComponent faces = do
  connComps <- connectedComponents faces
  let counts = count_ connComps
      biggest = fst $ last counts
  return [faces !! i | i <- elemIndices biggest connComps]
