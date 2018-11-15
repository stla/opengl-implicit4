module MarchingCubes.Voxel
  (makeVoxel, Voxel, voxelMax, DoubleVoxel, makeDoubleVoxel, voxelAverage)
  where

type Voxel = ([Double], (Int,Int,Int),
              ((Double,Double),(Double,Double),(Double,Double)))
type DoubleVoxel = (([Double],[Double]), (Int,Int,Int),
              ((Double,Double),(Double,Double),(Double,Double)))

makeVoxel :: ((Double,Double,Double) -> Double)
          -> ((Double,Double),(Double,Double),(Double,Double))
          -> (Int, Int, Int)
          -> Voxel
makeVoxel fun ((xm,xM),(ym,yM),(zm,zM)) (nx, ny, nz) =
  (values, (nx,ny,nz), ((xm,xM),(ym,yM),(zm,zM)))
  where
  x_ = [xm + (xM-xm) * fracx i | i <- [0..nx-1]]
  fracx p = realToFrac p / (realToFrac nx - 1)
  y_ = [ym + (yM-ym) * fracy i | i <- [0..ny-1]]
  fracy p = realToFrac p / (realToFrac ny - 1)
  z_ = [zm + (zM-zm) * fracz i | i <- [0..nz-1]]
  fracz p = realToFrac p / (realToFrac nz - 1)
  values = map fun [(x,y,z) | x <- x_, y <- y_, z <- z_]

voxelMax :: Voxel -> Double
voxelMax (vox,_,_) = maximum vox

makeDoubleVoxel :: ((Double,Double,Double) -> Double)
          -> ((Double,Double,Double) -> Double)
          -> ((Double,Double),(Double,Double),(Double,Double))
          -> (Int, Int, Int)
          -> DoubleVoxel
makeDoubleVoxel fun1 fun2 ((xm,xM),(ym,yM),(zm,zM)) (nx, ny, nz) =
  ((values1,values2), (nx,ny,nz), ((xm,xM),(ym,yM),(zm,zM)))
  where
  x_ = [xm + (xM-xm) * fracx i | i <- [0..nx-1]]
  fracx p = realToFrac p / (realToFrac nx - 1)
  y_ = [ym + (yM-ym) * fracy i | i <- [0..ny-1]]
  fracy p = realToFrac p / (realToFrac ny - 1)
  z_ = [zm + (zM-zm) * fracz i | i <- [0..nz-1]]
  fracz p = realToFrac p / (realToFrac nz - 1)
  grid = [(x,y,z) | x <- x_, y <- y_, z <- z_]
  values1 = map fun1 grid
  values2 = map fun2 grid

voxelAverage :: DoubleVoxel -> Double -> Voxel
voxelAverage ((vals1,vals2), ns, bounds) lambda = (values, ns, bounds)
  where
  values = zipWith (\x y -> lambda*x+(1-lambda)*y) vals1 vals2
