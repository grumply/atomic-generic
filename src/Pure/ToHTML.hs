{-# language DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Pure.ToHTML where

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-txt
import Pure.Data.Txt

-- from base
import Data.Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid
import GHC.Generics as G

class ToHTML a where
  toHTML :: a -> [View]
  default toHTML :: (Generic a, GToHTML (Rep a)) => a -> [View]
  toHTML = gtoHTML . G.from

instance ToHTML Bool where
  toHTML True = [ TextView Nothing "true" ]
  toHTML False = [ TextView Nothing "false" ]

instance ToHTML Int where
  toHTML i = [ TextView Nothing (toTxt i) ]

instance ToHTML Integer where
  toHTML i = [ TextView Nothing (toTxt i) ]

instance ToHTML Double where
  toHTML d = [ TextView Nothing (toTxt d) ]

instance ToHTML Txt where
  toHTML t = [ TextView Nothing t ]

instance ToHTML a => ToHTML [a] where
  toHTML = List.concatMap toHTML

instance ToHTML a => ToHTML (NonEmpty.NonEmpty a) where
  toHTML = toHTML . NonEmpty.toList

instance ToHTML a => ToHTML (Maybe a) where
  toHTML (Just a) = toHTML a
  toHTML Nothing = []

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False

instance (Equal a b ~ 'False, ToHTML a, ToHTML b) => ToHTML (Either a b) where
  toHTML (Left  a) = toHTML a
  toHTML (Right b) = toHTML b

instance (ToHTML a, ToHTML b) => ToHTML (a,b) where
  toHTML (a,b) = toHTML a <> toHTML b

instance (ToHTML a, ToHTML b, ToHTML c) => ToHTML (a,b,c) where
  toHTML (a,b,c) = toHTML a <> toHTML b <> toHTML c

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d) => ToHTML (a,b,c,d) where
  toHTML (a,b,c,d) = toHTML a <> toHTML b <> toHTML c <> toHTML d

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e) => ToHTML (a,b,c,d,e) where
  toHTML (a,b,c,d,e) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f) => ToHTML (a,b,c,d,e,f) where
  toHTML (a,b,c,d,e,f) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f, ToHTML g) => ToHTML (a,b,c,d,e,f,g) where
  toHTML (a,b,c,d,e,f,g) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f <> toHTML g

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f, ToHTML g, ToHTML h) => ToHTML (a,b,c,d,e,f,g,h) where
  toHTML (a,b,c,d,e,f,g,h) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f <> toHTML g <> toHTML h

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f, ToHTML g, ToHTML h, ToHTML i) => ToHTML (a,b,c,d,e,f,g,h,i) where
  toHTML (a,b,c,d,e,f,g,h,i) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f <> toHTML g <> toHTML h <> toHTML i

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f, ToHTML g, ToHTML h, ToHTML i, ToHTML j) => ToHTML (a,b,c,d,e,f,g,h,i,j) where
  toHTML (a,b,c,d,e,f,g,h,i,j) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f <> toHTML g <> toHTML h <> toHTML i <> toHTML j

instance (ToHTML a, ToHTML b, ToHTML c, ToHTML d, ToHTML e, ToHTML f, ToHTML g, ToHTML h, ToHTML i, ToHTML j, ToHTML k) => ToHTML (a,b,c,d,e,f,g,h,i,j,k) where
  toHTML (a,b,c,d,e,f,g,h,i,j,k) = toHTML a <> toHTML b <> toHTML c <> toHTML d <> toHTML e <> toHTML f <> toHTML g <> toHTML h <> toHTML i <> toHTML j <> toHTML k

class GToHTML a where
  gtoHTML :: a x -> [View]

instance (GToHTML f) => GToHTML (G.M1 D t f) where
  gtoHTML (G.M1 m) = gtoHTML m

instance (GToHTML f, Selector t) => GToHTML (G.M1 S t f) where
  gtoHTML (G.M1 m) = gtoHTML m

instance (GToHTML f, G.Constructor t) => GToHTML (G.M1 C t f) where
  gtoHTML m1@(G.M1 m) =
    [ SimpleHTML "div" <| Class (toTxt $ conName m1) |> gtoHTML m ]

instance GToHTML G.U1 where
  gtoHTML _ = []

instance (GToHTML a, GToHTML b) => GToHTML (a :*: b) where
  gtoHTML (a :*: b) = gtoHTML a <> gtoHTML b

instance (GToHTML a, GToHTML b) => GToHTML (a :+: b) where
  gtoHTML (L1 l) = gtoHTML l
  gtoHTML (R1 r) = gtoHTML r

instance ToHTML a => GToHTML (G.K1 i a) where
  gtoHTML (G.K1 a) = toHTML a
