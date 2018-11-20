module MultiPol
  (evalPoly, derivPoly, fromListOfMonomials, Polynomial(..), Monomial(..))
  where
-- import Data.List
-- import Data.Function

data Polynomial = M Monomial
                | Polynomial :+: Polynomial
                | Polynomial :*: Polynomial
                deriving(Show, Eq)

data Monomial = Monomial {
        coefficient :: Double,
        powers      :: (Int,Int,Int)
    }
    deriving(Show, Eq)

zero :: Polynomial
zero = M Monomial { coefficient = 0, powers = (0,0,0) }

derivMonomial :: Monomial -> Char -> Polynomial
derivMonomial mono var = let (px,py,pz) = powers mono in
  case var of
    'x' -> if px >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral px
                     , powers = (px-1,py,pz)
          }
      else zero
    'y' -> if py >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral py
                     , powers = (px,py-1,pz)
          }
      else zero
    'z' -> if pz >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral pz
                     , powers = (px,py,pz-1)
          }
      else zero
    _ -> error "only variables x,y,z are allowed"

derivPoly :: Polynomial -> Char -> Polynomial
derivPoly pol var = case pol of
  M mono -> derivMonomial mono var
  a :+: b -> derivPoly a var :+: derivPoly b var
  a :*: b -> (derivPoly a var :*: b) :+: (a :*: derivPoly b var)

evalMonomial :: (Double, Double, Double) -> Monomial -> Double
evalMonomial (x,y,z) monomial =
  coefficient monomial * x^px * y^py * z^pz
  where
    (px,py,pz) = powers monomial

evalPoly :: Polynomial -> (Double,Double,Double)-> Double
evalPoly pol xyz = case pol of
  M mono -> evalMonomial xyz mono
  a :+: b -> evalPoly a xyz + evalPoly b xyz
  a :*: b -> evalPoly a xyz * evalPoly b xyz

fromListOfMonomials :: [Monomial] -> Polynomial
fromListOfMonomials ms = foldl1 (:+:) (map M ms)
