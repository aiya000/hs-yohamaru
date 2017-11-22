{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy (Proxy(..))
import Data.Singletons (SingI(..), Sing, SingKind(..), SomeSing(..))
import Data.Type.Bool (If, type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol, symbolVal, Nat, natVal, KnownSymbol, KnownNat)
import Maru.Type
import qualified Maru.Type as MSym

{-
    善子「はぁ〜い こんにちは、リトルデーモンたち。
          定理証明界より舞い降りしフォーリンエンジェル、堕天使ヨハネ、降臨！」
    花丸「おらは一般的なプログラマーの花丸ずらー」

    善子「今回は、予め用意されたHaskell製のLisp処理系（hs-zuramaru）で用意されたデータ型を元にした題材で、
          すごく簡単な証明から…
          singleton typesと依存型を使った、証明済みの命題の実行時取り出しまでを進めていくわっ！」
    花丸「まるは調子にのった善子ちゃんを叩きつつ、補足していくよ！」
    善子「叩かないでよ！」

    善子「ていうか善子じゃなくてヨハネ！」
-}

-- | Maru.Type.SExpr.SExprと同じ形の型を定義
data HighSExpr = HCons HighSExpr HighSExpr
               | HAtomInt Nat
               | HAtomSymbol Symbol
               | HNil

{-
    善子「まずは依存型模倣のための基本、対象の型
          ……今回はSExpr型……
          と同じ形の型を定義したわ！」
    善子「SExpr型はこんな型よ」

        data SExpr = Cons SExpr SExpr
                   | Nil
                   | AtomInt Int
                   | AtomBool Bool
                   | AtomSymbol MaruSymbol
          deriving (Show, Eq)

    善子「AtomInt、AtomBool、AtomSymbolはそれぞれLispの整数値、真偽値、変数名/文字列を表しているの。
          そしてNilはラグナロクの始まりを、Consはレッドデビルの黒き翼を表して…」
    花丸「」ｽﾊﾟｺｰﾝｯ
    善子「いてっ！…
          え、えーと。Consはリストの構成子で、Nilはその終端を表しています」

        newtype MaruSymbol = MaruSymbol { unMaruSymbol :: Text }
          deriving (IsString, Semigroup, Monoid, Eq, Ord)

    花丸「MaruSymbolはTextの同型だね」

    善子「DataKinds拡張によって、
         HighSExpr型はHighSExprカインドに。
         その各値、例えばHNil値はHighSExprカインドのHNil型、
         つまり(HNil :: HighSExpr)型に持ち上げられるわ」
    善子「HAtomSymbolは…ふふ、(HAtomSymbol Symbol :: HighSExpr)型になるわね」

    花丸「Haskellでの依存型の概要はここ」

    - [定理証明系 Haskell - konn-san.com](http://konn-san.com/prog/2013-advent-calendar.html)

    花丸「活用についてはここ」

    - [実世界を扱う依存型プログラミングのたぶん基本～外界から安全な世界までの道 - ぼくのぬまち 出張版](http://notogawa.hatenablog.com/entry/2016/10/20/001812)

    花丸「応用はここを見るのがいいです」

    - [GHC.TypeLitsと型レベルFizzBuzz - Qiita](https://qiita.com/myuon_myon/items/dc6184f8e3d06ce3126c)
-}

data instance Sing (a :: HighSExpr) :: * where
  HAtomSymbolS :: MaruSymbol -> Sing ('HAtomSymbol s)
  HAtomIntS    :: Int -> Sing ('HAtomInt n)
  HNilS        :: Sing 'HNil
  HConsS       :: Sing x -> Sing y -> Sing ('HCons x y)

{-
    善子「漆黒、暗黒の道は…ここからよ。ふふふ」
    花丸「この変なかたちのdata instanceはなんずら？」

    善子「ずら丸はclosed type familyは知ってる？」
    花丸「type instanceのGADTsを使う方……
          各割り当てを全部where句の中で定義するから、追加で割り当てを定義できないやつ
          ……ずら？」
    善子「それね。
          今回のこれはclosed data family、単に、今ずら丸が言ったそれのdata版よ」
    善子「closedであることは、下で定義している`SingKind HighSExpr`インスタンスに必要なの」

    花丸「なるほどずら〜」
    善子「open/closed type/data familiesについてはここが詳しいわよ」

    - [型族が単射だと嬉しい理由 - Qiita](https://qiita.com/lotz/items/6c038698c8f04f57113a)

    花丸「なんでclosed type familiesとclosed data famileiesの構文は全然違うずら？」
    善子「仕様よ」
    花丸「仕様」
    善子「きっと天使たちの襲来のひずみね。GHCの人たちはおののいてしまったの」
    花丸「ずらあ……」
-}

instance SingI 'HNil where
  sing :: Sing 'HNil
  sing = HNilS

instance KnownSymbol s => SingI ('HAtomSymbol s) where
  sing :: Sing ('HAtomSymbol s)
  sing = HAtomSymbolS . MSym.pack $ symbolVal (Proxy :: Proxy s)

instance KnownNat n => SingI ('HAtomInt n) where
  sing :: Sing ('HAtomInt n)
  sing = HAtomIntS . fromInteger $ natVal (Proxy :: Proxy n)

instance (SingI x, SingI y) => SingI ('HCons x y) where
  sing :: Sing ('HCons x y)
  sing =
    let x' = sing :: Sing x
        y' = sing :: Sing y
    in HConsS  x' y'


instance SingKind HighSExpr where
  type DemoteRep HighSExpr = SExpr
  fromSing HNilS            = Nil
  fromSing (HAtomSymbolS s) = AtomSymbol s
  fromSing (HAtomIntS n)    = AtomInt n
  fromSing (HConsS x y)     = Cons (fromSing x) (fromSing y)
  toSing Nil            = SomeSing HNilS
  toSing (AtomSymbol s) = SomeSing $ HAtomSymbolS s
  toSing (AtomInt n)    = SomeSing $ HAtomIntS n
  toSing (Cons x y)     = case (toSing x, toSing y) of
                               (SomeSing x', SomeSing y') -> SomeSing $ HConsS x' y'

{-
    善子「術式展開、魔力装填。
          これが…ヨハネのちからよ！」
    花丸「落ち着くずら」ｽｯｯﾊﾟｺｰｰﾝｯ!
    善子「いてえっ」

    花丸「なんずらこれは？」
    善子「型の世界から値の世界の割り当てよ。
          さっきの`Sing (a :: HighSExpr)`のdata familyも、これの前準備だったの」
    花丸「割り当て？ 全然わからんずら」

    善子「一種の降霊術よ…、こ・う・れ・い・じゅ・つ♡
          例えば`SingI 'HNil`インスタンスは、
          型`Sing 'HNil`が値`HNilS`を割り当てることなの」
    善子「具体的には、以下を記述すると」

        x :: Sing 'HNil
        x = sing

    善子「xは`HNilS`という値に落ちてくるし、これは」

        y :: Sing ('HAtomSymbol "zura")
        y = sing

    善子「yは`HAtomSymbolS "zura"`という値に落ちてくるわ」

    花丸「それって単純に`x = HNilS`とか書くのと違うの？」
    善子「これは、値`sing`が静的に解決されることが重要で…
          具体的な内容については後で『証明済みの命題の実行時取り出し』って感じのところでやっていくけど、
          つまるところこれは、型が以下みたいに誤っていれば、コンパイルができなくなるのよ」

        z :: Sing ('HAtomSymbolS 10)
        z = sing

    花丸「なるほどずら」
-}

{-
    善子「ちょっと話題がそれちゃったから、話を元に戻すわね」
    善子「各SingIインスタンスが各値`x :: Sing (a :: HighSExpr)`を割り当てるのに加えて、
          `HighSExpr`カインドを`SExpr`型に、
          各型`a :: HighSExpr`を各値`x :: SExpr`に……
          例えば`'HNil`型を`Nil`値に……
          割り当てるのは、`SingKind HighSExpr`インスタンスが行うわ」
-}

{-
    善子「あとね、わたしもちょっとこれ書く時に混乱したから、
          InstanceSigs拡張のinstance sigunatureを書いたわ」
    善子「もっと書くと、こうね」

        sing :: (Sing HNil :: *)
        sing :: (Sing (HAtomSymbol s) :: *)
        sing :: (Sing (HAtomInt n) :: *)
        sing :: (Sing (HCons x y) :: *)

    善子「singは常に*になってくれるのが重要ね」

    花丸「KnownSymbolとKnownNatってなんずら？」
    善子「ああ、それは
            instance SingI (HAtomSymbol s)
          のsを(s :: Symbol)だと決定づける制約ね。
          HAtomIntのNatについても同じよ」
    花丸「instance SingI (HAtomSymbol (s :: Symbol))
          じゃだめずらか？」
    善子「それだと
            symbolVal :: KnownSymbol s => proxy s -> String
          が(Proxy :: Proxy s)のsをSymbolカインドに決定づけられないの。
          でも
            instance KnownSymbol s => SingI (HAtomSymbol (s :: Symbol))
          はありよ」
    花丸「わかりやすいずら〜」

    花丸「ていうかインスタンス宣言って、*カインドの型以外にも書けたずら？」
    善子「ええ、PolyKinds拡張のおかげで、こんな感じに書けるわ」

            {-# LANGUAGE DataKinds #-}
            {-# LANGUAGE PolyKinds #-}

            -- class AClass a -- こう書いてもいいけど、下の方がお作法がいいわね
            class AClass (a :: k)

            data Zura = Maru

            instance AClass Maru

    花丸「ずらあ…」

    善子「ふふ、ふふふ、ふふふふ…これで闇の使い魔を召喚する準備が整ったわね」
    花丸「依存型周りのことずらね」
-}

type instance 'HNil          == 'HNil          = 'True
type instance 'HAtomInt x    == 'HAtomInt y    = x == y
type instance 'HAtomSymbol x == 'HAtomSymbol y = x == y
type instance 'HCons x1 y1   == 'HCons x2 y2   = x1 == x2 && y1 == y2

-- | `assumption`ならば`result`
type family AsFarAs (assumption :: k) (result :: k) :: Maybe k where
  AsFarAs x y = If (x == y) ('Just y) 'Nothing

type family ZuraEats (xs :: HighSExpr) :: HighSExpr where
  ZuraEats ('HCons x xs) = xs
  ZuraEats _             = 'HAtomSymbol "おらののっぽパンがないずら！？"

{-
    善子「Lispライクに定理証明したいから、申し訳程度だけどDSLを作ったわ」
-}

type family ExtractProof (x :: Maybe HighSExpr) :: HighSExpr where
  ExtractProof ('Just x) = x
  ExtractProof _         = 'HAtomSymbol "証明が間違ってるずら"

{-
-}

-- | のっぽパン
type Noppo = 'HAtomSymbol "noppo-pan"

-- | 1つあるのっぽパンは食べたらなくなる
type XProof =
  (AsFarAs (ZuraEats ('HCons Noppo 'HNil)) 'HNil)

-- | 1つあるのっぽパンにのっぽパンが1つ増えたら、食べても1つ残る
type YProof =
  (AsFarAs (ZuraEats ('HCons Noppo ('HCons Noppo 'HNil)))
      ('HCons Noppo 'HNil))

x :: Sing (ExtractProof XProof)
x = sing

main :: IO ()
main = do
  putStrLn "天界にてLispで定理証明…やっていくわよ！"

{- その他参考ページ
    - [【ラブライブ！サンシャイン!! 1期】ヨハネこと津島善子の中二病セリフまとめ！大げさすぎてかわいい！ | まとめまとめ](http://matomame.jp/user/FrenchToast/b3b034e76e6bef49d27b?page=1)
    - [国木田花丸セリフbot](https://twitter.com/hanamarumarubot)
    - [津島善子 セリフbot](https://twitter.com/yohane445_bot)
    - [GHC.TypeLits](https://www.stackage.org/haddock/lts-9.12/base-4.9.1.0/GHC-TypeLits.html)
    - [Data.Singletons](https://www.stackage.org/haddock/lts-9.12/singletons-2.2/Data-Singletons.html)
-}

{- 関係ないけど面白いページ
    - [Data.Type.Equality](https://www.stackage.org/haddock/lts-9.2/base-4.9.1.0/Data-Type-Equality.html#t:-61--61-)
-}
