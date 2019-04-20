module MarchingCubes.ComputeContour3d2
  (computeContour3d', computeContour3d'')
  where
import           Control.Monad                  (when, (=<<))
import           Data.List                      (transpose)
import           Data.List.Split                (chunksOf)
import           Data.Tuple.Extra               (snd3, thd3, second)
import           Data.Vector.Unboxed            (Vector, fromList)
import qualified Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array          (peekArray)
import           MarchingCubes.ComputeContour3d (computeContour3d)
import           MarchingCubes.Voxel
import           Mesh.ConnectedComponents
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

computeContour3d' :: Voxel -> Maybe Double -> Double -> Bool -> Bool
                  -> IO ((Vector XYZ, [[Int]]), [XYZ])
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
  let points' = fromList $ map ((\p -> (p!!0, p!!1, p!!2)) . map realToFrac) points
      points'' = VU.map (rescale xyzbounds nxyz) points'
      faces = chunksOf 3 [0 .. VU.length points'' - 1]
      mesh = undupMesh (points'', faces)
  putStrLn "length points:"
  print $ VU.length points'
  putStrLn "length new points:"
  print $ VU.length $ fst mesh
  putStrLn "mesh unduped"
  if isolate
    then do
      let mesh' = second biggestComponent mesh
      putStrLn "number of faces:"
      print $ length (snd mesh')
      let nrmls = normals mesh -- pb if normals mesh'
      print $ length nrmls
      putStrLn "normals done"
      return (mesh', nrmls)
    else do
      let nrmls = normals mesh
      print $ length nrmls
      putStrLn "normals done"
      return (mesh, nrmls)

computeContour3d'' :: Voxel -> Maybe Double -> Double -> Bool -> Bool
                  -> IO ((Vector XYZ, [[Int]]), [XYZ])
computeContour3d'' voxel voxmax level isolate summary = do
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
      let mesh' = second biggestComponent mesh
      putStrLn "number of faces:"
      print $ length (snd mesh')
      let nrmls = normals mesh -- pb if normals mesh'
      print $ length nrmls
      putStrLn "normals done"
      return (mesh', nrmls)
    else do
      let nrmls = normals mesh
      print $ length nrmls
      putStrLn "normals done"
      return (mesh, nrmls)

-- computeContour3d'' :: Voxel -> Maybe Double -> Double -> Bool -> IO [Triangle]
-- computeContour3d'' voxel voxmax level summary = do
--   triangles <- computeContour3d' voxel voxmax level summary
--   let nxyz = snd3 voxel
--   let xyzbounds = thd3 voxel
--   return $ map (rescale xyzbounds nxyz) triangles
--
-- computeContour3d''' :: Voxel -> Maybe Double -> Double -> Bool
--                     -> IO ([Triangle], Double)
-- computeContour3d''' voxel voxmax level summary = do
--   triangles <- computeContour3d'' voxel voxmax level summary
--   let norm2max = maximum (map triangleNorm2Center triangles)
--   return (triangles, norm2max)
