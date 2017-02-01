{-# LANGUAGE Arrows #-}

module Nw where

import FRP.Netwire
import Control.Wire
import Prelude hiding ((.),id)

wire :: (Monad m) => Wire s () m a Integer
wire = pure 15

twire :: (HasTime t s) => Wire s () m a t
twire = time

intwire :: (HasTime c s, Monad m, Fractional c) => Wire s () m a c
intwire = proc _ -> do
            t <- time -< ()
            integral 5 -< t

tmain :: IO ()
tmain = testWire clockSession_ intwire
