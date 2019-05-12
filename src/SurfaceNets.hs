module SurfaceNets
  where
import Data.Bits
import Data.List.Index (imap)
import Data.Sequence (Seq, (|>), adjust', update, index)
import qualified Data.Sequence as S
import MarchingCubes.Voxel -- (Voxel)

cubeEdges :: [Int]
cubeEdges = [
  0, 1, 0, 2, 0, 4, 1, 3, 1, 5, 2, 3, 2, 6, 3, 7, 4, 5, 4, 6, 5, 7, 6, 7]

edgeTable :: [Int]
edgeTable = [
  0, 7, 25, 30, 98, 101, 123, 124, 168, 175, 177, 182, 202, 205,
  211, 212, 772, 771, 797, 794, 870, 865, 895, 888, 940, 939, 949,
  946, 974, 969, 983, 976, 1296, 1303, 1289, 1294, 1394, 1397,
  1387, 1388, 1464, 1471, 1441, 1446, 1498, 1501, 1475, 1476, 1556,
  1555, 1549, 1546, 1654, 1649, 1647, 1640, 1724, 1723, 1701, 1698,
  1758, 1753, 1735, 1728, 2624, 2631, 2649, 2654, 2594, 2597, 2619,
  2620, 2792, 2799, 2801, 2806, 2698, 2701, 2707, 2708, 2372, 2371,
  2397, 2394, 2342, 2337, 2367, 2360, 2540, 2539, 2549, 2546, 2446,
  2441, 2455, 2448, 3920, 3927, 3913, 3918, 3890, 3893, 3883, 3884,
  4088, 4095, 4065, 4070, 3994, 3997, 3971, 3972, 3156, 3155, 3149,
  3146, 3126, 3121, 3119, 3112, 3324, 3323, 3301, 3298, 3230, 3225,
  3207, 3200, 3200, 3207, 3225, 3230, 3298, 3301, 3323, 3324, 3112,
  3119, 3121, 3126, 3146, 3149, 3155, 3156, 3972, 3971, 3997, 3994,
  4070, 4065, 4095, 4088, 3884, 3883, 3893, 3890, 3918, 3913, 3927,
  3920, 2448, 2455, 2441, 2446, 2546, 2549, 2539, 2540, 2360, 2367,
  2337, 2342, 2394, 2397, 2371, 2372, 2708, 2707, 2701, 2698, 2806,
  2801, 2799, 2792, 2620, 2619, 2597, 2594, 2654, 2649, 2631, 2624,
  1728, 1735, 1753, 1758, 1698, 1701, 1723, 1724, 1640, 1647, 1649,
  1654, 1546, 1549, 1555, 1556, 1476, 1475, 1501, 1498, 1446, 1441,
  1471, 1464, 1388, 1387, 1397, 1394, 1294, 1289, 1303, 1296, 976,
  983, 969, 974, 946, 949, 939, 940, 888, 895, 865, 870, 794, 797,
  771, 772, 212, 211, 205, 202, 182, 177, 175, 168, 124, 123, 101,
  98, 30, 25, 7, 0]

surfaceNets :: Voxel -> Seq Int -- Seq (Seq Double)
surfaceNets (dat, (nx,ny,nz), ((xmin,xmax),(ymin,ymax),(zmin,zmax))) =
  loop2 0 1 (S.empty, S.replicate (2*(nx+1)*(ny+1)) 0, S.empty)
  where
  loop2 :: Int -> Int -> (Seq (Seq Double), Seq Int, Seq [Int]) -> Seq Int -- Seq (Seq Double)
  loop2 x2 b (vs,bf,fs) | x2 == nz-1 = bf -- vs
                       | otherwise =
                         loop2 (x2+1) (1-b) (loop1 0 x2 m (vs,bf,fs))
                           where
                           m = 1 + (nx + 1) * (1 + b * (ny + 1))
  -- loop1 :: Int -> Int -> Int -> Seq (Seq Double) -> Seq (Seq Double)
  loop1 x1 x2 m (vs,bf,fs) | x1 == ny-1 = (vs,bf,fs)
                          | otherwise =
                            loop1 (x1+1) x2 (m+nx-1+2)
                                  (loop0 0 x1 x2 m (vs,bf,fs))
  loop0 x0 x1 x2 m (vs,bf,fs) | x0 == nx-1 = (vs,bf,fs)
                              | otherwise =
                                loop0 (x0+1) x1 x2 (m+1) (f1 vs fs x0 x1 x2 bf m)
  f1 vs fs x0 x1 x2 bf m = (vsv, bf', fs')
    where
    grid = [dat !! (x0 + i + (x1 + j) * nx + (x2 + k) * nx * ny) |
            k <- [0,1], j <- [0,1], i <- [0,1]]
    is = imap (\i d -> if d<0 then shiftL (1::Int) i else (0::Int)) grid
    mask = foldr (.|.) 0 is
    edgeMask = edgeTable !! mask
    (vsv,bf',fs') = if mask == 0 || mask == 255
      then
        (vs, bf, fs)
      else
        (vs |> v,
         update m (S.length vs) bf,
         updatef fs bf m mask edgeMask [x0,x1,x2])
      where
        v = updatevx2 (f2 grid edgeMask (S.replicate 3 0)) x0 x1 x2
  f2 :: [Double] -> Int -> Seq Double -> (Seq Double, Double)
  f2 grid edgeMask v0 = vAndEcount
    where
    vAndEcount = oloop 0 0.0 v0
      where
      oloop :: Int -> Double -> Seq Double -> (Seq Double, Double)
      oloop i ecount vx | i == 12 = (vx, ecount)
                        | otherwise =
                          if edgeMask .&. shiftL 1 i == 0
                            then
                              oloop (i+1) ecount vx
                            else
                              oloop (i+1) (ecount+1)
                                    (f3 e0 e1 t vx)
                            where
                            idx = shiftL i 1
                            e0 = cubeEdges !! idx
                            e1 = cubeEdges !! (idx + 1)
                            g0 = grid !! e0
                            g1 = grid !! e1
                            t = g0 / (g0-g1)
  f3 e0 e1 t v = v'
    where
    v' = iloop 0 1 v
      where
      iloop :: Int -> Int -> Seq Double -> Seq Double
      iloop j k vx | j == 3 = vx
                   | otherwise =
                     iloop (j+1) (shiftL k 1)
                           (updatevx (e0 .&. k) (e1 .&. k) vx j)
      updatevx a b vx j =
        if a /= b
          then
            adjust' (+ (if a>0 then 1-t else t)) j vx
          else
            adjust' (+ (if a>0 then 1 else 0)) j vx
  updatevx2 (v, eCount) x0 x1 x2 = v'''
    where
      scx = (xmax - xmin) / (fromIntegral nx - 1)
      scy = (ymax - ymin) / (fromIntegral ny - 1)
      scz = (zmax - zmin) / (fromIntegral nz - 1)
      x0' = fromIntegral x0
      x1' = fromIntegral x1
      x2' = fromIntegral x2
      v' = adjust' (\v0 -> scx * (x0' + v0 / eCount) + xmin) 0 v
      v'' = adjust' (\v1 -> scy * (x1' + v1 / eCount) + ymin) 1 v'
      v''' = adjust' (\v2 -> scz * (x2' + v2 / eCount) + zmin) 2 v''
  updatef = loop 0
  loop i fs bf m mask edgeMask x | i==3 = fs
                                 | otherwise =
                                   if (edgeMask .&. shiftL 1 i) == 0 || (x !! iu) == 0 || (x !! iv) == 0
                                     then
                                       loop (i+1) fs bf m mask edgeMask x
                                     else
                                       loop (i+1) fs' bf m mask edgeMask x
                                   where
                                   iu = mod (i+1) 3
                                   iv = mod (i+2) 3
                                   du = iu -- r !! iu
                                   dv = iv -- r !! iv
                                   (fc1, fc2) = if (mask .&. 1) > 0
                                     then
                                       ([index bf m, index bf (m-du), index bf (m-dv)],
                                        [index bf (m-dv), index bf (m-du), index bf (m-du-dv)])
                                     else
                                       ([index bf m, index bf (m-dv), index bf (m-du)],
                                        [index bf (m-du), index bf (m-dv), index bf (m-du-dv)])
                                   fs' = (fs |> fc1) |> fc2


ftest :: (Double, Double, Double) -> Double
ftest (x,y,z) = x*x + y*y + z*z - 1

voxel :: Voxel
voxel = makeVoxel ftest ((-1,1), (-1,1), (-1,1)) (5,5,5)
