module Mesh.ConnectedComponents4
  where
import           Data.Graph                 (buildG, components)
import           Data.List                  (sort, elemIndex)
-- import           Data.List.Unique           (count_)
-- import           Data.Permute               (Permute, at, inverse)
import           Data.Permute               (at)
import qualified Data.Permute               as P
import           Data.Tree                  (flatten)
-- import           Math.Combinat.Permutations (Permutation, permuteList,
--                                              toPermutation)
import           Mesh.Edges

type Faces = [[Int]]

biggestComponent :: Faces -> IO Faces
biggestComponent faces = do
  let n = length faces
      (faces', p) = P.sort n $ map sort faces
      -- pinv = inverse $ snd faces'
      -- pinvAsList = map (\i -> at pinv i + 1) [0 .. n - 1]
      -- pinvAsPerm = toPermutation pinvAsList
  edges <- makeEdges faces'
  let maxindex = max (maximum $ map fst edges) (maximum $ map snd edges)
      graph = buildG (0,maxindex) edges
      comps = components graph
      comps' = map flatten comps
      lengths = map length comps'
      maxcount = maximum lengths
      Just argmax = elemIndex maxcount lengths
      biggestComp = comps' !! argmax
  putStrLn "Number of components:"
  print $ length comps
  return [faces !! at p i | i <- biggestComp]
--  return [fst faces' !! i | i <- biggestComp] -- lost faces orientation ! => faces !! (at pinv i)
  -- (<$!>) (permuteList pinvAsPerm . map fromIntegral) (peekArray n result)

-- biggestComponent :: Faces -> IO Faces
-- biggestComponent faces = do
--   connComps <- connectedComponents faces
--   let counts = count_ connComps
--       biggest = fst $ last counts
--   return [faces !! i | i <- elemIndices biggest connComps]
