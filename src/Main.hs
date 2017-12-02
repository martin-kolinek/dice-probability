{-# LANGUAGE TupleSections #-}
module Main where

import Data.Set (Set)
import Control.Monad (mapM, join)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import Data.Char (toLower)
import qualified Data.List.Zipper as Z
import qualified Numeric.Probability.Distribution as Prob
import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as P

type DieProb = Prob.T Double

data Goal = Scroll | Monster | Skull | SpyingGlass Int

data DieValue = ScrollValue | MonsterValue | SkullValue | SpyingGlassValue Int | JokerValue

data Outcome = Solved | NotSolved deriving (Eq, Ord)

data DieType = Green | Yellow | Red deriving (Eq, Ord, Enum)

dieValues Green = [ScrollValue, MonsterValue, SkullValue, SpyingGlassValue 1, SpyingGlassValue 2, SpyingGlassValue 3]
dieValues Yellow = [ScrollValue, SpyingGlassValue 4, SkullValue, SpyingGlassValue 1, SpyingGlassValue 2, SpyingGlassValue 3]
dieValues Red = [ScrollValue, SpyingGlassValue 4, SkullValue, JokerValue, SpyingGlassValue 2, SpyingGlassValue 3]

type Throw = [(DieType, DieValue)]

type Goals = [Goal]

throw :: [DieType] -> DieProb Throw
throw dieTypes = mapM throwDie dieTypes
  where throwDie dieType = (dieType,) <$> Prob.uniform (dieValues dieType)

trySatisfy :: Throw -> Goals -> Maybe [DieType]
trySatisfy throw [] = Just []
trySatisfy [] _ = Nothing
trySatisfy ((dieType, value):remainingThrow) goals = useValue <> skipValue
  where goalZipper = Z.fromList goals
        tryUse zp accum = case solves value (Z.cursor zp) of
          DoesNotSolve -> accum
          Solves -> accum <> ((dieType :) <$> (trySatisfy remainingThrow $ Z.toList $ Z.delete zp))
          PartiallySolves rest -> accum <> ((dieType :) <$> (trySatisfy remainingThrow $ Z.toList $ Z.replace rest zp))
        useValue = Z.foldrz tryUse Nothing goalZipper
        skipValue = trySatisfy remainingThrow goals

data Solution = Solves | DoesNotSolve | PartiallySolves Goal

solves :: DieValue -> Goal -> Solution
solves JokerValue (SpyingGlass x) = if x <= 4 then Solves else PartiallySolves (SpyingGlass (x - 4))
solves JokerValue _ = Solves
solves (SpyingGlassValue x) (SpyingGlass y) = if y <= x then Solves else PartiallySolves (SpyingGlass (y - x))
solves ScrollValue Scroll = Solves
solves MonsterValue Monster = Solves
solves SkullValue Skull = Solves
solves _ _ = DoesNotSolve

satisfy :: Goals -> [DieType] -> DieProb Outcome
satisfy goals dieTypes = Prob.norm $ do
  thr <- throw dieTypes
  return $ case trySatisfy thr goals of
    Nothing -> NotSolved
    Just _ -> Solved

multiplier :: P.Parser t -> P.Parser [t]
multiplier pars = do
  num <- P.option 1 $ P.decimal
  res <- pars
  return $ replicate (fromIntegral num) res

diceParser :: P.Parser [DieType]
diceParser = do
  res <- P.many (multiplier dieParser)
  P.eof
  return (join res)
  where dieParser = const Red <$> P.char 'r' P.<|>
                    const Yellow <$> P.char 'y' P.<|>
                    const Green <$> P.char 'g'

getDice = do
  putStrLn "Dice (_R_ed, _Y_ellow, _G_reen):"
  str <- getLine
  case P.parse diceParser "" (toLower <$> str) of
    Left err -> putStrLn (P.parseErrorPretty err) >> getDice
    Right d -> return d

goalsParser :: P.Parser Goals
goalsParser = do
  res <- P.many $ multiplier $ goalParser
  P.eof
  return (join res)
  where goalParser = const Skull <$> P.char 's' P.<|>
                     const Monster <$> P.char 'm' P.<|>
                     const Scroll <$> P.char 'c' P.<|>
                     spyingGlassParser
        spyingGlassParser = do
          P.char 'p'
          num <- P.decimal
          return $ SpyingGlass (fromIntegral num)

getGoals = do
  putStrLn "Goals (_S_kull, _M_onster, S_c_roll, S_p_ying glass):"
  str <- getLine
  case P.parse goalsParser "" (toLower <$> str) of
    Left err -> putStrLn (P.parseErrorPretty err) >> getGoals
    Right g -> return g

main :: IO ()
main = do
  dice <- getDice
  goals <- getGoals
  let probability = 100 * ((== Solved) Prob.?? satisfy goals dice)
  putStrLn $ "Success probability: " ++ show probability
