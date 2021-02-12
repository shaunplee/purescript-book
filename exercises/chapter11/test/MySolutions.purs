module Test.MySolutions where

import Control.Alternative ((<|>))
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Prelude
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (State, StateT, execState, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, runWriterT, tell)
import Data.Array (concat, many, replicate, some)
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate, traverse_)
import Data.Identity (Identity(..))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (drop, length, stripPrefix, toCharArray)
import Data.String.Common (joinWith)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)

-- Note to reader : Add your solutions to this file
sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

testParens :: String -> Boolean
testParens s = execState (go $ toCharArray s) 0 == 0
  where
  go :: Array Char -> State Int Unit
  go =
    traverse_ \c ->
      modify \count ->
        count
          + case c of
              '(' -> if count >= 0 then 1 else 0
              ')' -> -1
              otherwise -> 0

type Level
  = Int

type Doc
  = Reader Level String

line :: String -> Doc
line s = do
  level <- ask
  pure $ fold (replicate level " ") <> s

indent :: Doc -> Doc
indent d = local (2 + _) d

cat :: Array Doc -> Doc
cat ds = map (intercalate "\n") $ sequence ds

render :: Doc -> String
render d = runReader d 0

testDoc =
  render
    $ cat
        [ line "Here is some indented text:"
        , indent
            $ cat
                [ line "I am indented"
                , line "So am I"
                , indent $ line "I am even more indented"
                ]
        ]

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ (\n -> tell (Additive n))

collatz :: Int -> Tuple Int (Array Int)
collatz n = runWriter $ go 0 n
  where
  go :: Int -> Int -> Writer (Array Int) Int
  go cnt 1 = do
    tell [ 1 ]
    pure cnt

  go cnt m
    | even m = do
      tell [ m ]
      go (cnt + 1) (m / 2)
    | otherwise = do
      tell [ m ]
      go (cnt + 1) ((3 * m) + 1)

newtype DivideError a
  = DivideError (ExceptT String Identity a)

safeDivide :: Number -> Number -> (ExceptT String Identity) Number
safeDivide _ 0.0 = throwError "Divide by 0!"

safeDivide num den = pure $ num / den

type Errors
  = Array String

type Log
  = Array String

type Parser
  = StateT String (WriterT Log (ExceptT Errors Identity))

type Parser'
  = ExceptT Errors (StateT String (WriterT Log Identity))

string :: String -> Parser String
string needle = do
  s <- get
  tell [ "The state is " <> s ]
  case stripPrefix (Pattern needle) s of
    Just rest -> do
      put rest
      pure $ needle
    Nothing -> throwError [ "Could not parse" ]

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

runParser' :: forall a. Parser' a -> String -> (Tuple (Tuple (Either Errors a) String) Log)
runParser' p s = unwrap $ runWriterT $ runStateT (runExceptT p) s

type Doc'
  = WriterT Log (ReaderT Level Identity)

line' :: String -> Doc' Unit
line' s = do
  level <- ask
  tell $ [ fold (replicate level " ") <> s ]
  pure unit

indent' :: Doc' Unit -> Doc' Unit
indent' d = local (2 + _) d

render' :: Doc' Unit -> String
render' d = joinWith "\n" $ snd $ unwrap $ runReaderT (runWriterT d) 0

asThenBs :: Parser (Array String)
asThenBs =
  map concat
    $ sequence
        [ some (string "a")
        , many (string "b")
        ]

asAndBs :: Parser (Array String)
asAndBs = some (string "a" <|> string "b")
