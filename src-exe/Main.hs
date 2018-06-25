module Main
  where
import           Foreign
import           Foreign.C.Types
import           MarchingCubes.ComputeContour3d
import           MarchingCubes.Voxel

f :: (Double, Double, Double) -> Double
f (x,y,z) = x*x + y*y + z*z

-- vox :: IO Voxel
-- vox = makeVoxel f ((-2,2),(-2,2),(-2,2)) (5,5,5)

-- contour3d :: IO ((Ptr (Ptr CDouble)), Int)
-- contour3d = do
--     voxl <- vox
--     computeContour3d voxl (Just 12) 1

-- test :: IO [Triangle]
-- test = do
--     putStrLn "start"
--     vox <- makeVoxel f ((-2,2),(-2,2),(-2,2)) (5,5,5)
--     putStrLn "voxel done"
--     computeContour3d' vox (Just 100) 1

main :: IO ()
main = do
    -- c <- test
    -- putStrLn "c <- test done"
    -- print $ c
    vxl <- makeVoxel f ((-2,2),(-2,2),(-2,2)) (5,5,5)
    c <- computeContour3d vxl (Just 12) 1 
    print c
