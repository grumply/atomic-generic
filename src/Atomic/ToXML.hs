{-# language DefaultSignatures #-}
module Atomic.ToXML where

import Atomic.Base
import View

import Data.List as List
import Data.List.NonEmpty as NonEmpty

import GHC.Generics as G

class ToXML a where
  toXML :: a -> [View '[]]
  default toXML :: (Generic a, GToXML (Rep a)) => a -> [View '[]]
  toXML = gtoXML . from

instance ToXML Bool where
  toXML True = [ TextView Nothing "true" ]
  toXML False = [ TextView Nothing "false" ]

instance ToXML Int where
  toXML i = [ TextView Nothing (toTxt i) ]

instance ToXML Double where
  toXML d = [ TextView Nothing (toTxt d) ]

instance ToXML Txt where
  toXML t = [ TextView Nothing t ]

instance ToXML a => ToXML [a] where
  toXML = List.concatMap toXML

instance ToXML a => ToXML (NonEmpty.NonEmpty a) where
  toXML = toXML . NonEmpty.toList

instance ToXML a => ToXML (Maybe a) where
  toXML (Just a) = toXML a
  toXML Nothing = []

instance (ToXML a, ToXML b) => ToXML (a,b) where
  toXML (a,b) = toXML a <> toXML b

instance (ToXML a, ToXML b, ToXML c) => ToXML (a,b,c) where
  toXML (a,b,c) = toXML a <> toXML b <> toXML c

instance (ToXML a, ToXML b, ToXML c, ToXML d) => ToXML (a,b,c,d) where
  toXML (a,b,c,d) = toXML a <> toXML b <> toXML c <> toXML d

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e) => ToXML (a,b,c,d,e) where
  toXML (a,b,c,d,e) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f) => ToXML (a,b,c,d,e,f) where
  toXML (a,b,c,d,e,f) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f, ToXML g) => ToXML (a,b,c,d,e,f,g) where
  toXML (a,b,c,d,e,f,g) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f <> toXML g

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f, ToXML g, ToXML h) => ToXML (a,b,c,d,e,f,g,h) where
  toXML (a,b,c,d,e,f,g,h) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f <> toXML g <> toXML h

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f, ToXML g, ToXML h, ToXML i) => ToXML (a,b,c,d,e,f,g,h,i) where
  toXML (a,b,c,d,e,f,g,h,i) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f <> toXML g <> toXML h <> toXML i

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f, ToXML g, ToXML h, ToXML i, ToXML j) => ToXML (a,b,c,d,e,f,g,h,i,j) where
  toXML (a,b,c,d,e,f,g,h,i,j) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f <> toXML g <> toXML h <> toXML i <> toXML j

instance (ToXML a, ToXML b, ToXML c, ToXML d, ToXML e, ToXML f, ToXML g, ToXML h, ToXML i, ToXML j, ToXML k) => ToXML (a,b,c,d,e,f,g,h,i,j,k) where
  toXML (a,b,c,d,e,f,g,h,i,j,k) = toXML a <> toXML b <> toXML c <> toXML d <> toXML e <> toXML f <> toXML g <> toXML h <> toXML i <> toXML j <> toXML k

class GToXML a where
  gtoXML :: a x -> [View '[]]

instance (GToXML f) => GToXML (G.M1 D t f) where
  gtoXML (G.M1 m) = gtoXML m

instance (GToXML f) => GToXML (G.M1 S t f) where
  gtoXML (G.M1 m) = gtoXML m

instance (GToXML f, G.Constructor t) => GToXML (G.M1 C t f) where
  gtoXML m1@(G.M1 m) =
    [ HTMLView Nothing (toTxt $ conName m1) [] (gtoXML m) ]

-- instance GToXML G.U1 where
--   gtoXML _ = []

instance (GToXML a, GToXML b) => GToXML (a :*: b) where
  gtoXML (a :*: b) = gtoXML a <> gtoXML b

instance (GToXML a, GToXML b) => GToXML (a :+: b) where
  gtoXML (L1 l) = gtoXML l
  gtoXML (R1 r) = gtoXML r

instance ToXML a => GToXML (G.K1 i a) where
  gtoXML (G.K1 a) = toXML a
