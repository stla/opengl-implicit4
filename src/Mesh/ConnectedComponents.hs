module Mesh.ConnectedComponents
  where
import           Data.Graph      (flattenSCC, stronglyConnComp, graphFromEdges, components)
import Data.Tree (flatten)
import           Data.List       (elemIndex, findIndices, intersect)
import           Data.List.Index (imap)

type Faces = [[Int]]

connectedComponents :: Faces -> [Faces]
connectedComponents faces = map flattenSCC (stronglyConnComp x)
  where
  x = imap (\i face -> (face, i, findIndices (connectedFaces face) faces)) faces
  connectedFaces face1 face2 = length(face1 `intersect` face2) == 2

biggestComponent :: Faces -> Faces
biggestComponent faces = connComps !! i
  where
  connComps = connectedComponents faces
  lengths = map length connComps
  maxlength = maximum lengths
  Just i = elemIndex maxlength lengths

-- facesExpl :: [[Int]]
-- facesExpl = [[0,1,2], [0,2,3], [0,3,1], [3,2,1], [3,4,5]]
--
-- graph = graphFromEdges $
--   imap (\i face -> (face, i, findIndices (connectedFaces face) facesExpl)) facesExpl
--   where
--     connectedFaces face1 face2 = length(face1 `intersect` face2) == 2
--
-- graph' = let (g, _, _) = graph in g
--
-- cs = components graph'
--
-- c = flatten (cs !! 0)
