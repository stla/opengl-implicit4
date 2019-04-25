module MarchingCubes.ComputeContour3d2
  (computeContour3d', computeContour3d'')
  where
import           Control.Monad                  (when, (=<<))
import           Data.List                      (transpose)
import           Data.List.Split                (chunksOf)
import           Data.Tuple.Extra               (fst3, snd3, thd3)
import           Data.Vector.Unboxed            (Vector, fromList)
import qualified Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array          (peekArray)
import           MarchingCubes.ComputeContour3d (computeContour3d)
import           MarchingCubes.Voxel
import           Mesh.ConnectedComponents2
import           Mesh.Normals
import           Mesh.Undup

type XYZ = (Double, Double, Double)

rescale :: Floating a => ((a,a),(a,a),(a,a)) -> (Int,Int,Int) -> (a,a,a)
           -> (a,a,a)
rescale ((xm,xM),(ym,yM),(zm,zM)) (nx,ny,nz) (x,y,z) = (sx x, sy y, sz z)
  where
  s a b n u = a + (b-a) * u / fromIntegral (n+1)
  sx = s xm xM nx
  sy = s ym yM ny
  sz = s zm zM nz

-- computeContour3d' :: Voxel -> Maybe Double -> Double -> Bool -> Bool
--                   -> IO ((Vector XYZ, [[Int]]), [XYZ])
-- computeContour3d' voxel voxmax level isolate summary = do
--   (ppCDouble, nrows) <- computeContour3d voxel voxmax level
--   points <- mapM (peekArray 3) =<< peekArray nrows ppCDouble
--   let xyzbounds = thd3 voxel
--       nxyz = snd3 voxel
--   when summary $ do
--     let tpoints = transpose (map (map realToFrac) points)
--         xm = minimum (tpoints !! 0)
--         xM = maximum (tpoints !! 0)
--         ym = minimum (tpoints !! 1)
--         yM = maximum (tpoints !! 1)
--         zm = minimum (tpoints !! 2)
--         zM = maximum (tpoints !! 2)
--     putStrLn "Prebounds:"
--     print ((xm,ym,zm),(xM,yM,zM))
--     putStrLn "Bounds:"
--     print (rescale xyzbounds nxyz (xm,ym,zm), rescale xyzbounds nxyz (xM,yM,zM))
--   let points' = fromList $ map ((\p -> (p!!0, p!!1, p!!2)) . map realToFrac) points
--       points'' = VU.map (rescale xyzbounds nxyz) points'
--       faces = chunksOf 3 [0 .. VU.length points'' - 1]
--       mesh = undupMesh (points'', faces)
--   putStrLn "length points:"
--   print $ VU.length points'
--   putStrLn "length new points:"
--   print $ VU.length $ fst mesh
--   putStrLn "mesh unduped"
--   if isolate
--     then do
--       faces' <- biggestComponent (snd mesh)
--       let mesh' = (fst mesh, faces')
--       putStrLn "number of faces:"
--       print $ length (snd mesh')
--       let nrmls = normals mesh -- pb if normals mesh'
--       print $ length nrmls
--       putStrLn "normals done"
--       return (mesh', nrmls)
--     else do
--       let nrmls = normals mesh
--       print $ length nrmls
--       putStrLn "normals done"
--       return (mesh, nrmls)

computeContour3d' :: Voxel -> Maybe Double -> Double -> Bool -> Bool
                  -> IO ((Vector XYZ, [[Int]]), Vector XYZ)
computeContour3d' voxel voxmax level isolate summary = do
  (ppCDouble, nrows) <- computeContour3d voxel voxmax level
  points <- mapM (peekArray 3) =<< peekArray nrows ppCDouble
  let xyzbounds = thd3 voxel
      nxyz = snd3 voxel
  when summary $ do
    let tpoints = transpose (map (map realToFrac) points)
        xm = minimum (tpoints !! 0)
        xM = maximum (tpoints !! 0)
        ym = minimum (tpoints !! 1)
        yM = maximum (tpoints !! 1)
        zm = minimum (tpoints !! 2)
        zM = maximum (tpoints !! 2)
    putStrLn "Prebounds:"
    print ((xm,ym,zm),(xM,yM,zM))
    putStrLn "Bounds:"
    print (rescale xyzbounds nxyz (xm,ym,zm), rescale xyzbounds nxyz (xM,yM,zM))
  let points' = map ((\p -> (p!!0, p!!1, p!!2)) . map realToFrac) points
      points'' = map (rescale xyzbounds nxyz) points'
      faces = chunksOf 3 [0 .. length points'' - 1]
      mesh = undupMesh' (points'', faces)
  putStrLn "length points:"
  print $ length points'
  putStrLn "length new points:"
  print $ VU.length $ fst mesh
  putStrLn "mesh unduped"
  if isolate
    then do
      putStrLn "Isolating"
      faces' <- biggestComponent (snd mesh)
      let mesh' = (fst mesh, faces')
      putStrLn "number of faces:"
      print $ length (snd mesh')
      putStrLn "Computing normals"
      let nrmls = normals mesh -- pb if normals mesh'
      print $ length nrmls
      putStrLn "normals done"
      return (mesh', fromList nrmls)
    else do
      let nrmls = normals mesh
      print $ length nrmls
      putStrLn "normals done"
      return (mesh, fromList nrmls)

centroid :: Vector XYZ -> XYZ
centroid points = (a/n, b/n, c/n)
  where
  n = fromIntegral (VU.length points)
  a = VU.sum (VU.map fst3 points)
  b = VU.sum (VU.map snd3 points)
  c = VU.sum (VU.map thd3 points)

dist :: XYZ -> XYZ -> Double
dist (x,y,z) (x',y',z') = sqrt(sq(x-x')+sq(y-y')+sq(z-z'))
  where
  sq a = a*a

computeContour3d'' :: Voxel -> Maybe Double -> Double -> Bool -> Bool
                  -> IO (((Vector XYZ, [[Int]]), Vector XYZ), Vector Double)
computeContour3d'' voxel voxmax level isolate summary = do
  mesh <- computeContour3d' voxel voxmax level isolate summary
  let vertices = fst (fst mesh)
      center = centroid vertices
      distances = VU.map (dist center) vertices
  return (mesh, distances)
