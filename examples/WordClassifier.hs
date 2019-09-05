-- compile with:
-- ghc -O2 -fspec-constr-recursive=10 -fmax-worker-args=16 word-classifier.hs
--
import qualified Data.Char as Char
import           Data.Foldable
import           Data.Function ((&))
import           Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import qualified Data.List as List
import qualified Data.Ord as Ord
import           Foreign.Storable (Storable(..))
import qualified Streamly.Data.String as Streamly
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal as Streamly
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as Streamly
import           System.Environment (getArgs)
import           System.IO (openFile, IOMode(..))

instance (Enum a, Storable a) => Hashable (A.Array a) where
    hash arr = runIdentity $ Streamly.runFold Streamly.rollingHash (A.read arr)
    hashWithSalt salt arr = runIdentity $
        Streamly.runFold (Streamly.rollingHashWithSalt salt) (A.read arr)

main :: IO ()
main =
    let
        increment m str = Map.insertWith (+) str (1 :: Int) m
        report = traverse_ print
    in do
        name <- fmap head getArgs
        src <- openFile name ReadMode
        FH.read src                  -- SerialT IO Word8
         & Streamly.decodeChar8      -- SerialT IO Char
         & Streamly.map Char.toLower -- SerialT IO Char
         & Streamly.words            -- SerialT IO (Array Char)
         & Streamly.filterM (Streamly.all Char.isAlpha . A.read)
         & Streamly.foldl' increment Map.empty
         & fmap (List.take 25 . List.sortOn (Ord.Down . snd) . Map.toList)
         >>= report                  -- IO ()