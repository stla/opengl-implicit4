{-# LINE 1 "hsc/Edges.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh.Edges
  ( c_makeEdges, cEdgeToPair )
  where
import           Control.Monad       ((<$!>), (=<<))
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc      (free, mallocBytes)
import           Foreign.Marshal.Array      (pokeArray, peekArray)



data CEdge = CEdge {
    _first :: CUInt
  , _second :: CUInt
}

instance Storable CEdge where
    sizeOf    __ = (8)
{-# LINE 20 "hsc/Edges.hsc" #-}
    alignment __ = 4
{-# LINE 21 "hsc/Edges.hsc" #-}
    peek ptr = do
      first'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 23 "hsc/Edges.hsc" #-}
      second' <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 24 "hsc/Edges.hsc" #-}
      return CEdge { _first = first'
                   , _second = second' }
    poke ptr (CVertex r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 29 "hsc/Edges.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 30 "hsc/Edges.hsc" #-}

foreign import ccall unsafe "makeEdges" c_makeEdges
  :: Ptr CUInt -- faces
  -> CUInt -- nfaces
  -> Ptr CUInt -- nedges
  -> IO (Ptr CEdge)

cEdgeToPair :: CEdge -> (Int, Int)
cEdgeToPair cedge = (fromIntegral $ _first cedge, fromIntegral $ _second cedge)

makeEdges :: [[Int]] -> IO [(Int,Int)]
makeEdges faces = do
  let nfaces = length faces
  facesPtr <- mallocBytes (nfaces * 3 * sizeOf (undefined :: CUInt))
  pokeArray facesPtr (concatMap (map fromIntegral) faces)
  nedgesPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  result <- c_makeEdges facesPtr (fromIntegral nfaces) nedgesPtr
  nedges <- (<$!>) fromIntegral (peek nedgesPtr)
  free nedgesPtr
  free facesPtr
  (<$!>) cEdgeToPair (peekArray nedges result)
