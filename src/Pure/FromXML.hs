{-# language DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Pure.FromXML where

-- from pure-core
import Pure.Data.View

-- from pure-txt
import Pure.Data.Txt

-- from base
import Data.Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid
import GHC.Generics as G
import Text.Read

class FromXML a where
  parseXML :: [View] -> ([View],Maybe a)
  default parseXML :: (Generic a, GFromXML (Rep a)) => [View] -> ([View],Maybe a)
  parseXML vs = fmap (fmap G.to) $ gparseXML vs

instance FromXML Bool where
  parseXML vs@(TextView _ t : rest) =
    case t of
      "false" -> (rest,Just False)
      "true" -> (rest,Just True)
      "0" -> (rest,Just False)
      "1" -> (rest,Just True)
      _ -> (vs,Nothing)
  parseXML vs = (vs,Nothing)

instance FromXML Int where
  parseXML [] = ([],Nothing)
  parseXML vs@(TextView _ t : rest) =
    case readMaybe (fromTxt t) of
      Just i -> (rest,Just i)
      _ -> (vs,Nothing)

instance FromXML Integer where
  parseXML [] = ([],Nothing)
  parseXML vs@(TextView _ t : rest) =
    case readMaybe (fromTxt t) of
      Just i -> (rest,Just i)
      _      -> (vs,Nothing)

instance FromXML Double where
  parseXML [] = ([],Nothing)
  parseXML vs@(TextView _ t : rest) =
    case readMaybe (fromTxt t) of
      Just d -> (rest,Just d)
      _ -> (vs,Nothing)

instance FromXML Txt where
  parseXML (TextView _ t : rest) = (rest,Just t)
  parseXML vs = (vs,Nothing)

instance FromXML a => FromXML [a] where
  parseXML vs = go [] vs
    where
      go acc vs =
        case parseXML vs of
          (others,Just a) -> go (a:acc) others
          _ -> (vs,Just (List.reverse acc))

instance FromXML a => FromXML (NonEmpty.NonEmpty a) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (others,Just as@(_:_)) -> (others,Just (NonEmpty.fromList as))
      _ -> (vs,Nothing)

instance FromXML a => FromXML (Maybe a) where
  parseXML vs =
    case parseXML vs of
      (rest,Just a) -> (rest,Just (Just a))
      _ -> (vs,Just Nothing)

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False

instance (Equal a b ~ 'False, FromXML a, FromXML b) => FromXML (Either a b) where
  -- will fail to parse Right in case of Either a a
  parseXML vs =
    case parseXML vs of
      (rest,Just a) -> (rest,Just (Left a))
      _ -> case parseXML vs of
             (rest,Just b) -> (rest,Just (Right b))
             _             -> (vs,Nothing)

instance (FromXML a, FromXML b) => FromXML (a,b) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just a) ->
        case parseXML rest of
          (rest',Just b) -> (rest',Just (a,b))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c) => FromXML (a,b,c) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b)) ->
        case parseXML rest of
          (rest',Just c) -> (rest',Just (a,b,c))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d) => FromXML (a,b,c,d) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c)) ->
        case parseXML rest of
          (rest',Just d) -> (rest',Just (a,b,c,d))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e) => FromXML (a,b,c,d,e) where
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d)) ->
        case parseXML rest of
          (rest',Just e) -> (rest',Just (a,b,c,d,e))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f) => FromXML (a,b,c,d,e,f) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e)) ->
        case parseXML rest of
          (rest',Just f) -> (rest',Just (a,b,c,d,e,f))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f, FromXML g) => FromXML (a,b,c,d,e,f,g) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e,f)) ->
        case parseXML rest of
          (rest',Just g) -> (rest',Just (a,b,c,d,e,f,g))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f, FromXML g, FromXML h) => FromXML (a,b,c,d,e,f,g,h) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e,f,g)) ->
        case parseXML rest of
          (rest',Just h) -> (rest',Just (a,b,c,d,e,f,g,h))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f, FromXML g, FromXML h, FromXML i) => FromXML (a,b,c,d,e,f,g,h,i) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e,f,g,h)) ->
        case parseXML rest of
          (rest',Just i) -> (rest',Just (a,b,c,d,e,f,g,h,i))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f, FromXML g, FromXML h, FromXML i, FromXML j) => FromXML (a,b,c,d,e,f,g,h,i,j) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e,f,g,h,i)) ->
        case parseXML rest of
          (rest',Just j) -> (rest',Just (a,b,c,d,e,f,g,h,i,j))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

instance (FromXML a, FromXML b, FromXML c, FromXML d, FromXML e, FromXML f, FromXML g, FromXML h, FromXML i, FromXML j, FromXML k) => FromXML (a,b,c,d,e,f,g,h,i,j,k) where
  parseXML [] = ([],Nothing)
  parseXML vs =
    case parseXML vs of
      (rest,Just (a,b,c,d,e,f,g,h,i,j)) ->
        case parseXML rest of
          (rest',Just k) -> (rest',Just (a,b,c,d,e,f,g,h,i,j,k))
          _ -> (vs,Nothing)
      _ -> (vs,Nothing)

class GFromXML a where
  gparseXML :: [View] -> ([View],Maybe (a x))

instance (GFromXML a) => GFromXML (G.M1 D t a) where
  gparseXML vs = fmap (fmap G.M1) $ gparseXML vs

instance (GFromXML a) => GFromXML (G.M1 S t a) where
  gparseXML vs = fmap (fmap G.M1) $ gparseXML vs

instance (GFromXML a, G.Constructor t) => GFromXML (G.M1 C t a) where
  gparseXML inp@(HTMLView _ t _ children : rest ) =
    if t == toTxt (conName (undefined :: G.C1 t a x)) then
      case gparseXML children of
        (_,Just a) -> (rest,Just $ G.M1 a)
        _ -> (inp,Nothing)
    else
      (inp,Nothing)
  gparseXML vs = (vs,Nothing)

instance GFromXML G.U1 where
  gparseXML vs = (vs,Just (G.U1 :: G.U1 x))

instance (FromXML a) => GFromXML (G.K1 i a) where
  gparseXML vs = fmap (fmap G.K1) (parseXML vs)

instance (GFromXML a,GFromXML b) => GFromXML (a :*: b) where
  gparseXML vs =
    case gparseXML vs of
      (others,Just a) ->
        case gparseXML others of
          (others',Just b) -> (others',Just (a :*: b))
          (others',_) -> (others',Nothing)
      (_,_) -> (vs,Nothing)

instance (GFromXML a, GFromXML b) => GFromXML (a :+: b) where
  gparseXML vs =
    case gparseXML vs of
      (others,Just a) -> (others,Just (L1 a))
      (_,_) -> fmap (fmap R1) (gparseXML vs)
