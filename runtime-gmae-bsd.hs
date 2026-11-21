{-# LANGUAGE ForeignFunctionInterface #-}

module Lat
  ( LatString(..)
  , lat_print_int
  , lat_print_string
  , lat_read_int
  , lat_read_string
  , lat_new_string
  , lat_new_instance
  , lat_new_array
  , lat_cat_strings
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (callocBytes, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stdout, stdin)

data LatString = LatString
  { latStringLength :: CSize
  , latStringContents :: CString
  } deriving (Eq, Show)

instance Storable LatString where
  sizeOf _ = sizeOf (undefined :: CSize) + sizeOf (undefined :: CString)
  alignment _ = alignment (undefined :: CSize)
  peek p = LatString
    <$> peekByteOff p 0
    <*> peekByteOff p (sizeOf (undefined :: CSize))
  poke p (LatString len contents) = do
    pokeByteOff p 0 len
    pokeByteOff p (sizeOf (undefined :: CSize)) contents

terminate :: String -> IO a
terminate msg = do
  BS.hPut stdout (BS.pack msg)
  hFlush stdout
  exitWith (ExitFailure 1)

notNull :: Ptr a -> IO (Ptr a)
notNull p = do
  when (p == nullPtr) (terminate "internal error. null reference.\n")
  pure p

lat_print_int :: Int32 -> IO ()
lat_print_int x = BS.hPut stdout (BS.pack (show x ++ "\n")) `catch` ioHandler "printf"
  where
    ioHandler name (_ :: IOException) =
      terminate ("ERROR: Error in: " ++ name ++ "\n")

lat_print_string :: Ptr LatString -> IO ()
lat_print_string p = do
  str <- peek p
  contents <- peekCString (latStringContents str)
  BS.hPut stdout (BS.pack (contents ++ "\n")) `catch` ioHandler "printf"
  where
    ioHandler name (_ :: IOException) =
      terminate ("ERROR: Error in: " ++ name ++ "\n")

lat_read_int :: IO Int32
lat_read_int = do
  line <- BS.hGetLine stdin `catch` ioHandler
  pure (fromIntegral (parseInteger line))
  where
    ioHandler (_ :: IOException) = terminate "ERROR: Error in: getline\n"
    parseInteger bs =
      let text = dropWhile (`elem` (" \t\n\v\f\r" :: String)) (BS.unpack bs)
      in case text of
        ('+' : rest) -> digits rest
        ('-' : rest) -> negate (digits rest)
        rest -> digits rest
    digits = foldl (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0

lat_new_string :: CString -> CSize -> IO (Ptr LatString)
lat_new_string str len = do
  _ <- notNull str
  result <- mallocBytes (sizeOf (undefined :: LatString))
  when (result == nullPtr) (terminate "ERROR: malloc failed\n")
  poke result (LatString len str)
  pure result

lat_new_instance :: CSize -> IO (Ptr ())
lat_new_instance size
  | size < 8 =
      terminate ("internal error. invalid instance size " ++ show size
                 ++ ", must be at least 8 to hold the vtable pointer")
  | otherwise = do
      result <- callocBytes (fromIntegral size)
      when (result == nullPtr) (terminate "ERROR: calloc failed\n")
      pure result

lat_new_array :: Int32 -> CSize -> IO (Ptr ())
lat_new_array count size
  | size == 0 = terminate "internal error. invalid instance size 0"
  | count <= 0 = terminate "runtime error. non-positive array size.\n"
  | size < 4 && 4 `rem` size /= 0 =
      terminate "internal error. element size does not divide 4"
  | otherwise = do
      let sizeInt = fromIntegral size :: Int
          slots :: Int
          slots
            | sizeInt < 4 =
                fromIntegral count + 4 `div` sizeInt
            | otherwise = fromIntegral count + 1
          bytes = slots * sizeInt
      result <- callocBytes bytes
      when (result == nullPtr) (terminate "ERROR: calloc failed\n")
      poke (castPtr result) count
      pure result

lat_read_string :: IO (Ptr LatString)
lat_read_string = do
  line <- BS.hGetLine stdin `catch` ioHandler
  str <- newCString (BS.unpack line)
  lat_new_string str (fromIntegral (BS.length line))
  where
    ioHandler (_ :: IOException) = terminate "ERROR: Error in: getline\n"

lat_cat_strings :: Ptr LatString -> Ptr LatString -> IO (Ptr LatString)
lat_cat_strings ptr1 ptr2 = do
  str1 <- peek ptr1
  str2 <- peek ptr2
  contents1 <- peekCString (latStringContents str1)
  contents2 <- peekCString (latStringContents str2)
  let combined = contents1 ++ contents2
  str <- newCString combined
  lat_new_string str (fromIntegral (length combined))
