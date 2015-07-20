-- {{{
{-# LANGUAGE OverloadedStrings, MultiWayIf, LambdaCase #-}
module Main where
---------------------Import-------------------------
import Prelude
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as BS -- BS.getContents
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap as HM
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Control.Applicative
import Data.Ratio                            -- x = 5%6
import Data.Maybe
import Text.Regex.Posix
import System.Random                         -- randomIO, randomRIO, randomRs, newStdGen
import Data.Int                              -- Int32, Int64
import System.IO
import Data.Bits                             -- (.&.), (.|.), shiftL...
import Text.Printf                           -- printf "%0.6f" (1.0)
import Control.Monad
import System.Directory
import System.FilePath
import Data.Aeson
import Control.Exception
import System.Process                       -- runCommand
-- }}}

main :: IO ()
main = do
  return ()

---------------User Defined Functions----------------- {{{
readInteger = getInteger
readInt = getInt

getInteger = head. readIntegerArray
getInt = head. readIntArray

getIntArray = readIntArray
getIntegerArray = readIntegerArray

readIntArray = readIntArray' BS.readInt
readIntegerArray = readIntArray' BS.readInteger

readIntArray' :: (Integral a) => (BS.ByteString->Maybe (a, BS.ByteString)) -> BS.ByteString -> [a]
readIntArray' parseInt str =
  case parseInt. BS.dropWhile isSpace $ str of
    Nothing -> []
    Just (n, str') -> n : readIntArray' parseInt str'
------------------------------------------------------ }}}
