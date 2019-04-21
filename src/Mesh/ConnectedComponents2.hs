{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh.ConnectedComponents2
  where
import           Control.Monad         ((<$!>))
import           Data.List             (elemIndices)
import           Data.List.Unique      (count_)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (sizeOf)

type Faces = [[Int]]

foreign import ccall unsafe "connComps" c_connComps
    :: Ptr CUInt -> CUInt -> IO (Ptr CInt)

connectedComponents :: Faces -> IO [Int]
connectedComponents faces = do
  let n = length faces
  facesPtr <- mallocBytes (3 * n * sizeOf (undefined :: CUInt))
  pokeArray facesPtr (map fromIntegral $ concat faces)
  result <- c_connComps facesPtr (fromIntegral n)
  free facesPtr
  (<$!>) (map fromIntegral) (peekArray n result)

biggestComponent :: Faces -> IO Faces
biggestComponent faces = do
  connComps <- connectedComponents faces
  let counts = count_ connComps
      biggest = fst $ last counts
  return [faces !! i | i <- elemIndices biggest connComps]
