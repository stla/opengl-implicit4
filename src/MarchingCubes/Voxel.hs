module MarchingCubes.Voxel
  (makeVoxel, Voxel, voxelMax)
  where

type Voxel = ([Double], (Int,Int,Int),
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
