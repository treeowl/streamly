import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.FileSystem.File as File

import Data.Char (ord)
import System.Environment (getArgs)

cat :: FilePath -> IO ()
cat src = File.writeArrays "/dev/stdout" $ File.readArraysOf (256*1024) src

cp :: FilePath -> FilePath -> IO ()
cp src dst = File.writeArrays dst $ File.readArraysOf (256*1024) src

append :: FilePath -> FilePath -> IO ()
append src dst = File.appendArrays dst $ File.readArraysOf (256*1024) src

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: FilePath -> IO ()
wcl src = print =<< (S.length
    $ FL.splitSuffixBy (== ord' '\n') FL.drain
    $ File.read src)

grepc :: String -> FilePath -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ FL.splitOn (A.fromList (map ord' pat)) FL.drain
    $ File.read src)

avgll :: FilePath -> IO ()
avgll src = print =<< (FL.foldl' avg
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ File.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

llhisto :: FilePath -> IO ()
llhisto src = print =<< (FL.foldl' (FL.classify FL.length)
    $ S.map bucket
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ File.read src)
    where
    bucket n = let i = n `div` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    src <- fmap head getArgs

    putStrLn "cat"    >> cat src              -- Unix cat program
    putStr "wcl "     >> wcl src              -- Unix wc -l program
    putStr "grepc "   >> grepc "aaaa" src     -- Unix grep -c program
    putStr "avgll "   >> avgll src            -- get average line length
    putStr "llhisto " >> llhisto src          -- get line length histogram
    putStr "cp "      >> cp src "dst-xyz.txt" -- Unix cp program
    putStr "append "  >> append src "dst-xyz.txt" -- Appending to file
