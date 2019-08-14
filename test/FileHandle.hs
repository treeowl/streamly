module Main (main) where

import Foreign
import System.IO
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.Hspec as H

import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.Handle as FH

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
maxTestCount = 10

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

data DummyStorable =
    DummyStorable
        { storableInt :: Int
        , storableChar :: Char
        , storableBool :: Bool
        } deriving (Eq, Ord, Show)

genDummyStorable :: Gen DummyStorable
genDummyStorable = DummyStorable <$> arbitrary <*> arbitrary <*> arbitrary

instance Storable DummyStorable where
    sizeOf _ =
        sizeOf (undefined :: Int) + sizeOf (undefined :: Char) +
        sizeOf (undefined :: Bool)
    alignment = sizeOf
    peek ptr =
        DummyStorable <$> peek (castPtr ptr) <*>
        peekByteOff (castPtr ptr) (sizeOf (undefined :: Int)) <*>
        peekByteOff
            (castPtr ptr)
            (sizeOf (undefined :: Int) + sizeOf (undefined :: Char))
    poke ptr v = do
        poke (castPtr ptr) (storableInt v)
        pokeByteOff (castPtr ptr) (sizeOf (undefined :: Int)) (storableChar v)
        pokeByteOff
            (castPtr ptr)
            (sizeOf (undefined :: Int) + sizeOf (undefined :: Bool))
            (storableBool v)

testReadWrite :: FilePath -> Property
testReadWrite file =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len genDummyStorable) $ \list ->
            monadicIO $ do
                run $ withFile file WriteMode $ \outH -> do
                      S.runFold (FH.write outH) (S.fromList list)
                xs <- run $ withFile file ReadMode $ \inH ->
                   S.toList $ FH.read inH
                assert (xs == list)

infile :: String
infile = "./testfileXYZ"

main :: IO ()
main = hspec
  $ modifyMaxSuccess (const maxTestCount)
  $ do
  describe "File" $ do
      prop "read . write == id" $ testReadWrite infile
