{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, StandaloneDeriving, LambdaCase, OverloadedStrings #-}
module Lambda.Name where

import GHC.Generics (Generic)
import Data.Data
import Data.String
import Text.Printf
import Data.Text (Text, pack, unpack, replace)

-- Names are stored in NM form when we do program generation. NI is only used
-- when we seralize the Exp
data Name
  = NM { unNM :: !Text }
  | NI !Int
  deriving (Generic, Data, Eq, Ord, Show)

nMap :: (Text -> Text) -> Name -> Name
nMap f (NM n) = NM (f n)

instance Semigroup Name where
  (NM n1) <> (NM n2) = NM (n1 <> n2)

instance Monoid Name where
  mempty = NM mempty

instance IsString Name where
  fromString = NM . fromString

instance PrintfArg Name where
  formatArg = formatString . unpack . unNM

-- utils
unpackName :: Name -> String
unpackName (NM name) = unpack name

packName :: String -> Name
packName = NM . pack

showTS :: Show a => a -> Name
showTS = packName . show

-- module name handling
--  Qualified Name =  PACKAGE_ID + '_' + MODULE_NAME_THAT_CAN_CONTAIN_DOTS + '.' + DOT_ENCODED_NAME
mkPackageQualifiedName :: String -> String -> String -> Name
mkPackageQualifiedName pkg mod name = encodeUnderscore (packName pkg) <> "_" <> packName mod <> "." <> encodeDot (packName name)

decodePackageQualifiedName :: Name -> Maybe (String, String, String)
decodePackageQualifiedName n = do
  (pkg, mod_and_name) <- decodeUntil '_' $ unpackName n
  (rev_name, rev_mod) <- decodeUntil '.' $ reverse mod_and_name
  pure (pkg, reverse rev_mod, reverse rev_name)

decodeUntil :: Char -> String -> Maybe (String, String)
decodeUntil key = go [] where
  go locName (a : b : xs)
    | a == key
    , b == key
    = go (key : locName) xs
  go locName (a : xs)
    | a == key
    = Just (reverse locName, xs)
  go locName (x : xs) = go (x : locName) xs
  go locName [] = Nothing

-- dot mangling
encodeDot :: Name -> Name
encodeDot = nMap $ replace "." ".."

decodeDot :: Name -> Name
decodeDot = nMap $ replace ".." "."

-- underscore mangling
encodeUnderscore :: Name -> Name
encodeUnderscore = nMap $ replace "_" "__"

decodeUnderscore :: Name -> Name
decodeUnderscore = nMap $ replace "__" "_"
