module Data.AMF3 (
    parseSOL
  , parseData
  , module Text.JSON
  ) where

import Control.Monad.State
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Control.Applicative ((<$>))
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Text.JSON
import Text.JSON.Types
import Data.AMF3.Types

type AMF a = StateT AMFCache Get a

cacheObject :: JSValue -> AMF ()
cacheObject obj = do
    cache@(AMFCache {getObjectCache = xs}) <- get
    put cache {getObjectCache=obj:xs}

refObject :: Int -> AMF JSValue
refObject n = do
    AMFCache {getObjectCache = xs} <- get
    let len = length xs
    if len <= n
        then
            fail $ "There is no object reffered by " ++ (show n)
        else
            return $ xs !! (len - n - 1)

cacheTrait :: AMFObjectTrait -> AMF ()
cacheTrait trait = do
    cache@(AMFCache {getTraitCache = xs}) <- get
    put cache {getTraitCache = trait : xs}

refTrait :: Int -> AMF AMFObjectTrait
refTrait n = do
    AMFCache {getTraitCache = xs} <- get
    let len = length xs
    if len <= n 
        then
            fail $ "There is no trait reffered by " ++ (show n)
        else
            return $ xs !! (len - n - 1)

cacheString :: JSValue -> AMF ()
cacheString s = do
    cache@(AMFCache {getStringCache = xs}) <- get
    put cache {getStringCache = s : xs}

refString :: Int -> AMF JSValue
refString n = do
    cache@(AMFCache {getStringCache = xs}) <- get
    let len = length xs
    if len <= n
        then
            fail $ "There is no string reffered by " ++ (show n)
        else 
            return $ xs !! (len - n - 1)

readPadding :: AMF ()
readPadding = do
    x <- fromIntegral <$> lift getWord8
    if x == 0 then return () else
        lift $ fail "There must be a null char for separating."

-- a usefull function
readUInt29 :: AMF Integer
readUInt29 = do
    let readUByte = toInteger <$> lift getWord8
    a0 <- readUByte
    if a0 < 128 then return $! a0 else do
        let a1 = (a0 .&. 127) `shiftL` 7
        a2 <- readUByte
        if a2 < 128 then return $! a1 .|. a2 else do
            let a3 = (a1 .|. a2 .&. 127) `shiftL` 7
            a4 <- readUByte
            if a4 < 128 then return $! a3 .|. a4 else do
                let a5 = (a3 .|. a4 .&. 127) `shiftL` 8
                a6 <- readUByte
                return $! a5 .|. a6

--read int
readInt :: AMF JSValue
readInt = do
    i <- readUInt29
    return $ JSRational False $ toRational $! (i `shiftL` 3) `shiftR` 3

--read double
readDouble :: AMF JSValue
readDouble = do
    num <- lift getFloat64host
    return $ JSRational False $ toRational num

--read a string
readString :: AMF JSValue
readString = do
    l <- readUInt29
    let ref = l .&. 1
    if ref == 0
        then
            refString $ fromIntegral $! l `shiftR` 1
        else do
            let str_len = fromIntegral $! l `shiftR` 1
            b_str <- lift $ getByteString str_len
            let str = (JSString . JSONString . toString) b_str
            cacheString str
            return str

--read a haskell string
readNormalString :: AMF String
readNormalString = do
    JSString (JSONString str) <- readString
    return str

--read an array
readArray :: AMF JSValue
readArray = do
    l <- readUInt29
    let ref = l .&. 1
    if ref == 0
        then
            refObject $ fromIntegral $! l `shiftR` 1
        else do
            let readAttr xs = do
                str <- readNormalString
                if str == ""
                    then
                        return $ reverse xs
                    else do
                        dt <- readData
                        readAttr ((str,dt):xs)
            pairs <- readAttr []
            let arr_len = l `shiftR` 1
            let readArr xs n len = do
                if n == len
                    then
                        return $ reverse xs
                    else do
                        dt <- readData
                        readArr ((show n, dt):xs) (n + 1) len
            arrs <- readArr [] 0 arr_len
            let ret =   if length pairs == 0
                            then
                                JSArray [dt | (_,dt) <- arrs]
                            else
                                JSObject $ JSONObject $ pairs ++ arrs
            cacheObject ret
            return ret

--read object
readObject :: AMF JSValue
readObject = do
    l <- readUInt29
    let ref = l .&. 1
    if ref == 0
        then
            refObject $ fromIntegral $! l `shiftR` 1
        else do
            let readTrait = do
                if (l .&. 3) == 1
                    then
                        refTrait $ fromIntegral $! l `shiftR` 2
                    else do
                        let external = (l .&. 4) == 4
                            dynamic = (l .&. 8) == 8
                            count = l `shiftR` 4
                        name <- readNormalString
                        let readName n = do
                            if n == 0
                                then
                                    return []
                                else do
                                    str <- readNormalString
                                    (str:) <$> readName (n - 1)
                        members <- readName count
                        let ret = AMFObjectTrait external dynamic name members
                        cacheTrait ret
                        return ret
            
            trait <- readTrait
            if isExternalizable trait
                then
                    fail "object externalizable encoding is not supported"
                else do
                    let readClassProperties [] = return []
                        readClassProperties (name : xs) = do
                            obj <- readData
                            ((name, obj):) <$> readClassProperties xs
                    classProperties <- readClassProperties $ getMemberNames trait
                    if isDynamic trait 
                        then do
                            let readDynamicProperties = do
                                name <- readNormalString
                                if name == ""
                                    then
                                        return []
                                    else do
                                        obj <- readData
                                        ((name, obj):) <$> readDynamicProperties
                            dynamicProperties <- readDynamicProperties
                            if length classProperties == 0
                                then
                                    return $ JSObject $ JSONObject dynamicProperties
                                else
                                    let properties = classProperties ++ dynamicProperties
                                    in return $ JSObject $ JSONObject properties
                        else
                            return $ JSObject $ JSONObject $ classProperties
--read data
readData :: AMF JSValue
readData = do
    flag <- fromIntegral <$> lift getWord8
    case flag of
        0 -> return JSNull
        1 -> return JSNull
        2 -> return $ JSBool False
        3 -> return $ JSBool True
        4 -> readInt
        5 -> readDouble
        6 -> readString
        9 -> readArray
        10 -> readObject
        n -> fail $ "cannot parse this data tag: " ++ (show n)

--read sol file header
readSOLHeader :: AMF (Int, String, Int)
readSOLHeader = do
    version <- fromIntegral <$> lift getWord16be
    l <- lift getWord32be
    signature <- lift $ getByteString 10
    l <- lift getWord16be
    name <- toString <$> (lift $ getByteString $ fromIntegral l)
    amfversion <- fromIntegral <$> lift getWord32be
    return (version, name, amfversion)

--read sol file
readSOL :: AMF JSValue
readSOL = do
    (_,_,3) <- readSOLHeader -- amf3

    let readPairs = do
        eof <- lift isEmpty
        if eof
            then
                return []
            else do
                name <- readNormalString
                value <- readData
                readPadding
                ((name, value):) <$> readPairs

    --ret <- reverse <$> readPairs
    ret <- readPairs
    return $ JSObject $ JSONObject ret

--decode sol file
parseSOL :: B.ByteString -> Either String JSValue
parseSOL = fst . (runGet $ (`evalStateT` emptyCache) readSOL)
--decode raw data
parseData :: B.ByteString -> Either String JSValue
parseData = fst . (runGet $ (`evalStateT` emptyCache) readData)
