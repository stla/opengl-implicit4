{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh.Edges
  ( c_makeEdges, makeEdges )
  where
import           Control.Monad       ((<$!>))
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc      (free, mallocBytes)
import           Foreign.Marshal.Array      (pokeArray, peekArray)

#include "connComps.h"

data CEdge = CEdge {
    _first :: CUInt
  , _second :: CUInt
}

instance Storable CEdge where
    sizeOf    __ = #{size edge}
    alignment __ = #{alignment edge}
    peek ptr = do
      first'  <- #{peek edge, first} ptr
      second' <- #{peek edge, second} ptr
      return CEdge { _first = first'
                   , _second = second' }
    poke ptr (CEdge r1 r2)
      = do
          #{poke edge, first} ptr r1
          #{poke edge, second} ptr r2

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
  (<$!>) (map cEdgeToPair) (peekArray nedges result)
