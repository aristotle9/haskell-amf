module Data.AMF3.Types where

import Text.JSON.Types

data AMFObjectTrait = AMFObjectTrait {
    isExternalizable :: Bool,
    isDynamic :: Bool,
    getClassName :: String,
    getMemberNames :: [String]
} deriving (Show, Eq)

data AMFCache = AMFCache {
    getObjectCache :: [JSValue],
    getTraitCache :: [AMFObjectTrait],
    getStringCache :: [JSValue]
} deriving (Show, Eq)

emptyCache :: AMFCache
emptyCache = AMFCache [] [] []
