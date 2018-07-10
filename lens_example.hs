{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens hiding (element)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

data Atom =
          Atomo { _element :: String, _point :: Point }
        | Atoma { _atoma :: [Atom] }
    deriving (Show)

makeLenses ''Atom
makeLenses ''Point

shiftAtoma :: Atom -> Atom
shiftAtoma = over (atoma . traverse . point . y) (* 32)

main :: IO ()
main = do
    let atom1 = Atomo { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
    let atom2 = Atomo { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
    let atoma = Atoma { _atoma = [atom1, atom2] }
    print $ shiftAtoma atoma
