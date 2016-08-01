{-# LANGUAGE CPP #-}
module FRP.Elerea.Simple.Compat
    ( module FRP.Elerea.Simple
    , module FRP.Elerea.Simple.Compat
    ) where
import FRP.Elerea.Simple

#if !MIN_VERSION_elerea(2, 9, 0)
till :: Signal Bool -> SignalGen (Signal Bool)
till = FRP.Elerea.Simple.until
#endif
