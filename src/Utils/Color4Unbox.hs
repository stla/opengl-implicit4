{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Utils.Color4Unbox
  where
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox)
import           Graphics.Rendering.OpenGL.GL (Color4 (..))

derivingUnbox "Color4"
    [t| forall a . (Unbox a) => Color4 a -> (a, a, a, a) |]
    [| \(Color4 r g b alpha) -> (r, g, b, alpha) |]
    [| \(r, g, b, alpha) -> Color4 r g b alpha |]
