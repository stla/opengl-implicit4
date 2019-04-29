{-# LANGUAGE BangPatterns #-}
module Mesh.ConnectedComponents5
  where
import           Data.Foldable (toList)
import           Data.Graph    (buildG, components)
import           Data.List     (elemIndex, intersect, sort)
import           Data.Permute  (at)
import qualified Data.Permute  as P
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import           Data.Tree     (flatten)
import           Data.Vector   (Vector, fromList, (!))

type Faces = [[Int]]

facesExpl :: [[Int]]
facesExpl = [[0,1,2], [0,2,3], [0,3,1], [3,2,1], [3,4,5]]

makeEdges :: Vector [Int] -> Seq (Int,Int)
makeEdges faces = go (length faces) 0 S.empty
  where
  go !n i !edges | i == n = edges
                 | otherwise = go n (i+1) (inner n i (i+1) edges)
  inner !n !i j !edges | j == n = edges
                       | otherwise =
                         if facei !! 2 <= facej !! 0
                           then edges
                           else
                             if length(facei `intersect` facej) == 2
                               then inner n i (j+1) (edges |> (i,j))
                               else inner n i (j+1) edges
                           where
                             facei = faces ! i
                             facej = faces ! j

biggestComponent :: Faces -> Faces
biggestComponent faces = [faces !! at p i | i <- biggestComp]
  where
  n = length faces
  (faces', p) = P.sort n $ map sort faces
  edges = toList $ makeEdges (fromList faces')
  maxindex = max (maximum $ map fst edges) (maximum $ map snd edges)
  graph = buildG (0,maxindex) edges
  comps = components graph
  comps' = map flatten comps
  lengths = map length comps'
  maxcount = maximum lengths
  Just argmax = elemIndex maxcount lengths
  biggestComp = comps' !! argmax
