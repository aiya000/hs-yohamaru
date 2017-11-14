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
import qualified Maru.Type as MSym

-- | Maru.Type.SExpr.SExprと同じ形の型を定義
data HighSExpr = HCons HighSExpr HighSExpr
               | HAtomInt Nat
               | HAtomSymbol Symbol
               | HNil

-- 'SExpr'の各値と一意な各割り当て`(x :: HighSExpr) -> Sing x`を定義
newtype instance Sing (HAtomSymbol s) = HAtomSymbolS MaruSymbol
newtype instance Sing (HAtomInt n) = HAtomIntS Int
data instance Sing HNil = HNilS
data instance Sing (HCons x y) = HConsS (Sing x) (Sing y)

instance SingI HNil where
  sing :: Sing HNil
  sing = HNilS

instance KnownSymbol s => SingI (HAtomSymbol s) where
  sing :: Sing (HAtomSymbol s)
  sing = HAtomSymbolS . MSym.pack $ symbolVal (Proxy :: Proxy s)

instance KnownNat n => SingI (HAtomInt n) where
  sing :: Sing (HAtomInt n)
  sing = HAtomIntS . fromInteger $ natVal (Proxy :: Proxy n)

instance (SingI x, SingI y) => SingI (HCons x y) where
  sing :: Sing (HCons x y)
  sing =
    let x' = sing :: Sing x
        y' = sing :: Sing y
    in HConsS  x' y'


-- | 善子x花丸とそのリバのリバが等しい
type YohaMaru = 'HNil

-- 1つあるのっぽパンは食べたらなくなる
-- 1つあるのっぽパンにのっぽパンが1つ増えたら、食べても1つ残る

main :: IO ()
main = putStrLn "天界にてLispで定理証明…やっていくわよ！"
