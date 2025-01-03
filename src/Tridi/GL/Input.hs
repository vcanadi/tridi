{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Tridi.GL.Input where
import Data.Map.Strict (Map, fromList)
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Map as M
import Data.Function ((&))
import Data.Functor.Identity (Identity(runIdentity, Identity))
import Data.Bool (bool)

import Orth.P3 (P3 (P3))

{- Handle keyboard input -}

data Input = Input
  { iF :: Bool -- ^ Forward
  , iB :: Bool -- ^ Backward
  , iL :: Bool -- ^ Leftward
  , iR :: Bool -- ^ Rightward
  , iU :: Bool -- ^ Upward
  , iD :: Bool -- ^ Downward
  , iRL :: Bool -- ^ Rotation left
  , iRR :: Bool -- ^ Rotation rightj
  , iRU :: Bool -- ^ Rotation up
  , iRD :: Bool -- ^ Rotation down
  } deriving (Show)

defInput :: Input
defInput = Input False False False False False False False False False False

data InputKey = IKF | IKB | IKL | IKR | IKU | IKD | IKRL | IKRR | IKRU | IKRD

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- | Simple lens construction without depending on 'lens' library
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

set :: Lens s t a b -> b -> s -> t
set l b x = runIdentity $ l (\_ -> Identity b) x

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

_key :: InputKey -> Lens' Input Bool
_key IKF  = lens iF (\i s -> i { iF = s })
_key IKB  = lens iB (\i s -> i { iB = s })
_key IKL  = lens iL (\i s -> i { iL = s })
_key IKR  = lens iR (\i s -> i { iR = s })
_key IKU  = lens iU (\i s -> i { iU = s })
_key IKD  = lens iD (\i s -> i { iD = s })
_key IKRL = lens iRL (\i s -> i { iRL = s })
_key IKRR = lens iRR (\i s -> i { iRR = s })
_key IKRU = lens iRU (\i s -> i { iRU = s })
_key IKRD = lens iRD (\i s -> i { iRD = s })

-- | Bind GLFW's keys with internal model's representation (InputKey)
inputKeys :: Map GLFW.Key InputKey
inputKeys = fromList
  [ (GLFW.Key'W, IKF)
  , (GLFW.Key'S, IKB)
  , (GLFW.Key'A, IKL)
  , (GLFW.Key'D, IKR)
  , (GLFW.Key'I, IKU)
  , (GLFW.Key'U, IKD)
  , (GLFW.Key'H, IKRL)
  , (GLFW.Key'L, IKRR)
  , (GLFW.Key'K, IKRU)
  , (GLFW.Key'J, IKRD)
  ]

readInputKey :: GLFW.Key -> Maybe InputKey
readInputKey k = M.lookup k inputKeys

pressKey :: Maybe InputKey -> Input -> Input
pressKey Nothing i  = i
pressKey (Just k) i = i & (_key k .~ True)

releaseKey :: Maybe InputKey -> Input -> Input
releaseKey Nothing i  = i
releaseKey (Just k) i = i & _key k .~ False

-- | Oberver positional diff i.e. movement direction based on given input
inputV :: (Num n) => Input -> P3 n
inputV Input{..} = P3 (bool 0 1 iF + bool 0 (-1) iB)
                      (bool 0 1 iL + bool 0 (-1) iR)
                      (bool 0 1 iU + bool 0 (-1) iD)

-- | Observer rotational diff i.e. horizontal and vertical look direction
inputR :: Num n => Input -> (n,n)
inputR Input{..} = ( bool 0 1 iRL + bool 0 (-1) iRR
                   , bool 0 1 iRU + bool 0 (-1) iRD
                   )

