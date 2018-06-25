{-# LANGUAGE ForeignFunctionInterface #-}
module MarchingCubes.ComputeContour3d
  (computeContour3d, computeContour3d', computeContour3d'', computeContour3d''')
  where
import           Control.Monad                 ((=<<), when)
import           Data.List                     (transpose)
import           Data.Maybe                    (fromMaybe)
import           Data.Tuple.Extra              (fst3, snd3, thd3, (&&&))
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (free, mallocBytes)
import           Foreign.Marshal.Array         (peekArray, pokeArray)
import           Foreign.Ptr                   (Ptr)
import           Foreign.Storable              (peek, sizeOf)
import           MarchingCubes.Utils.Triangles (Triangle, toTriangles,
                                                triangleNorm2Center)
import           MarchingCubes.Voxel

foreign import ccall unsafe "computeContour3d" c_computeContour3d
    :: Ptr CDouble
    -> CUInt
    -> CUInt
    -> CUInt
    -> CDouble
    -> CDouble
    -> Ptr CSize
    -> IO (Ptr (Ptr CDouble))

computeContour3d :: Voxel -> Maybe Double -> Double
                 -> IO (Ptr (Ptr CDouble), Int)
computeContour3d voxel voxmax level = do
  let max' = fromMaybe (maximum (fst3 voxel)) voxmax
      (_, (nx,ny,nz), _) = voxel
      voxel' = map realToFrac (fst3 voxel)
  putStrLn "Allocating voxel"
  voxelPtr <- mallocBytes (nx*ny*nz * sizeOf (undefined :: CDouble))
  putStrLn "Poking voxel"
  pokeArray voxelPtr voxel'
  nrowsPtr <- mallocBytes (sizeOf (undefined :: CSize))
  putStrLn "Run MC"
  result <- c_computeContour3d voxelPtr
            (fromIntegral nx) (fromIntegral ny) (fromIntegral nz)
            (realToFrac max') (realToFrac level) nrowsPtr
  nrows <- peek nrowsPtr
  free nrowsPtr
  free voxelPtr
  return (result, fromIntegral nrows)

rescale :: ((Double,Double),(Double,Double),(Double,Double)) -> (Int,Int,Int)
        -> Triangle -> Triangle
rescale ((xm,xM),(ym,yM),(zm,zM)) (nx,ny,nz) ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
  ((sx x1, sy y1, sz z1), (sx x2, sy y2, sz z2), (sx x3, sy y3, sz z3))
  where
  s a b n u = a + (b-a) * u / fromIntegral (n-1)
  sx = s xm xM nx
  sy = s ym yM ny
  sz = s zm zM nz

computeContour3d' :: Voxel -> Maybe Double -> Double -> Bool -> IO [Triangle]
computeContour3d' voxel voxmax level summary = do
  (ppCDouble, nrows) <- computeContour3d voxel voxmax level
  points <- mapM (peekArray 3) =<< peekArray nrows ppCDouble
  when summary $ do
    let nxyz = snd3 voxel
    let xyzbounds = thd3 voxel
    let tpoints = transpose (map (map realToFrac) points)
        xm = minimum (tpoints !! 0)
        xM = maximum (tpoints !! 0)
        ym = minimum (tpoints !! 1)
        yM = maximum (tpoints !! 1)
        zm = minimum (tpoints !! 2)
        zM = maximum (tpoints !! 2)
    putStrLn "Bounds:"
    print $ (fst3 &&& snd3) $
      rescale xyzbounds nxyz ((xm,ym,zm),(xM,yM,zM),(0/0,0/0,0/0))
  return $ toTriangles (map (map realToFrac) points)

computeContour3d'' :: Voxel -> Maybe Double -> Double -> Bool -> IO [Triangle]
computeContour3d'' voxel voxmax level summary = do
  triangles <- computeContour3d' voxel voxmax level summary
  let nxyz = snd3 voxel
  let xyzbounds = thd3 voxel
  return $ map (rescale xyzbounds nxyz) triangles

computeContour3d''' :: Voxel -> Maybe Double -> Double -> Bool
                    -> IO ([Triangle], Double)
computeContour3d''' voxel voxmax level summary = do
  triangles <- computeContour3d'' voxel voxmax level summary
  let norm2max = maximum (map triangleNorm2Center triangles)
  return (triangles, norm2max)
