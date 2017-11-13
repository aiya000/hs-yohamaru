{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Proxy (Proxy(..))
import Data.Singletons (SingI(..), Sing)
import GHC.TypeLits (Symbol, symbolVal, Nat, natVal, KnownSymbol, KnownNat)
import Maru.Type

-- | Maru.Type.SExpr.SExprと同じ形の型を定義
data HighSExpr = HCons HighSExpr HighSExpr
               | HAtomInt Nat
               | HAtomSymbol Symbol
               | HNil

-- 'SExpr'の各値と一意な各割り当て`(x :: HighSExpr) -> Sing x`を定義
newtype instance Sing HNil = HNilS ()
newtype instance Sing (HAtomSymbol s) = HAtomSymbolS MaruSymbol
newtype instance Sing (HAtomInt n) = HAtomIntS Int
newtype instance Sing (HCons x y) = HConsS ((Sing x), (Sing y))


main :: IO ()
main = putStrLn "天界にてLispで定理証明…やっていくわよ！"
