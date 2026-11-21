
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Lat
import Numeric (showHex)
import System.Environment (getArgs)
import System.IO (stdout)

out :: String -> IO ()
out = BS.hPut stdout . BS.pack

hex2 :: Word8 -> String
hex2 b =
  let s = showHex b ""
  in if length s < 2 then '0' : s else s

dump :: String -> Ptr a -> Int -> IO ()
dump label p n = do
  bytes <- peekArray n (castPtr p :: Ptr Word8)
  out (label ++ ":" ++ concatMap (\b -> " " ++ hex2 b) bytes ++ "\n")

showArray :: Int32 -> Int -> IO ()
showArray count size = do
  a <- lat_new_array count (fromIntegral size)
  len <- peek (castPtr a :: Ptr Int32)
  let slots = if size < 4
                then fromIntegral count + 4 `div` size
                else fromIntegral count + 1
  out ("array(" ++ show count ++ "," ++ show size ++ ") len=" ++ show len ++ "\n")
  dump "bytes" a (slots * size)
  free a

errorMode :: String -> IO ()
errorMode mode = do
  case mode of
    "err-instance"     -> lat_new_instance 4 >> pure ()
    "err-array-size"   -> lat_new_array 4 3
    "err-array-count"  -> lat_new_array 0 4
    "err-array-zero"   -> lat_new_array 4 0
    "err-eof-int"      -> lat_read_int >> pure ()
    "err-eof-str"      -> lat_read_string >> pure ()
    _                  -> pure ()
  out "

errorMode :: String -> IO ()
errorMode mode = do
  case mode of
    "err-instance"    -> lat_new_instance 4 >> pure ()
    "err-array-size"  -> lat_new_array 4 3 >> pure ()
    "err-array-count" -> lat_new_array 0 4 >> pure ()
    "err-array-zero"  -> lat_new_array 4 0 >> pure ()
    "err-eof-int"     -> lat_read_int >> pure ()
    "err-eof-str"     -> lat_read_string >> pure ()
    _                 -> pure ()
  out "unreachable\n"

fullDemo :: IO ()
fullDemo = do
  out "== print_int ==\n"
  lat_print_int 42
  lat_print_int (-7)
  lat_print_int 0

  out "== print_string ==\n"
  BS.useAsCString (BS.pack "hello") $ \p ->
    lat_new_string p 5 >>= lat_print_string
  BS.useAsCString (BS.pack "") $ \p ->
    lat_new_string p 0 >>= lat_print_string

  out "== read_int ==\n"
  forM_ [1 .. 5 :: Int] $ \_ -> lat_read_int >>= lat_print_int

  out "== read_string ==\n"
  forM_ [1 .. 2 :: Int] $ \_ -> do
    s <- lat_read_string
    str <- peek s
    out ("[len=" ++ show (latStringLength str) ++ "]")
    lat_print_string s

  out "== new_instance ==\n"
  inst16 <- lat_new_instance 16
  dump "inst16" inst16 16
  free inst16
  inst8 <- lat_new_instance 8
  dump "inst8" inst8 8
  free inst8

  out "== new_array ==\n"
  showArray 5 8
  showArray 3 1
  showArray 3 2
  showArray 4 4
  showArray 2 16

  out "== done ==\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    (mode : _) -> errorMode mode
    []         -> fullDemo
	
