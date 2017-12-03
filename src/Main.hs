{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Set (Set)
import Control.Monad (mapM, join, forM)
import Data.List (sort, foldl', group)
import Data.Monoid ((<>))
import Data.Char (toLower)
import qualified Data.List.Zipper as Z
import qualified Numeric.Probability.Distribution as Prob
import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as P
import qualified Math.Combinatorics.Exact.Binomial as C
import Debug.Trace

mapZipper :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapZipper f z = Z.foldrz (\iterator accum -> f iterator : accum) [] z

type DieProb = Prob.T Double

data Goal = Scroll | Monster | Skull | SpyingGlass Int

data DieValue = ScrollValue | MonsterValue | SkullValue | SpyingGlassValue !Int | JokerValue deriving (Eq, Ord, Show)

type Outcome = Bool

data DieType = Green | Yellow | Red deriving (Eq, Ord, Enum, Show)

dieValues Green = sort [ScrollValue, MonsterValue, SkullValue, SpyingGlassValue 1, SpyingGlassValue 2, SpyingGlassValue 3]
dieValues Yellow = sort [ScrollValue, SpyingGlassValue 4, SkullValue, SpyingGlassValue 1, SpyingGlassValue 2, SpyingGlassValue 3]
dieValues Red = sort [ScrollValue, SpyingGlassValue 4, SkullValue, JokerValue, SpyingGlassValue 2, SpyingGlassValue 3]

type Throw = [(DieType, DieValue)]

type Goals = [Goal]

throwSingleType :: DieType -> Int -> DieProb Throw
throwSingleType tp totalNumberOfDice = Prob.fromFreqs $ getValuesWithProbability <$> countCombinations (length values) totalNumberOfDice
  where values = dieValues tp
        countCombinations 1 numberOfDice = [[numberOfDice]]
        countCombinations possibleValues numberOfDice = [(numberOfDice - count) : other | count <- [0..numberOfDice], other <- countCombinations (possibleValues - 1) count]
        getCountProbability (remainingDice, totalProbability) count =
          (remainingDice - count,
           (1.0 / (fromIntegral $ length values) ^^ count) * (fromIntegral $ C.choose remainingDice count) * totalProbability)
        getProbabilityForCounts counts = snd $ foldl' getCountProbability (totalNumberOfDice, 1) counts
        getValuesForCounts counts = join $ zipWith replicate counts ((tp,) <$> values)
        getValuesWithProbability counts = (getValuesForCounts counts, getProbabilityForCounts counts)

throw :: [DieType] -> DieProb Throw
throw dieTypes = do
  let sorted = sort dieTypes
      grouped = group sorted
  groupThrows <- forM grouped $ \grp -> do
    throwSingleType (head grp) (length grp)
  return $ join groupThrows

trySatisfy :: Throw -> Goals -> Maybe [DieType]
trySatisfy throw [] = Just []
trySatisfy [] _ = Nothing
trySatisfy ((dieType, value):remainingThrow) goals = useValue <> skipValue
  where goalZipper = Z.fromList goals
        tryUse zp = case solves value (Z.cursor zp) of
          DoesNotSolve -> Nothing
          Solves -> ((dieType :) <$> (trySatisfy remainingThrow $ Z.toList $ Z.delete zp))
          PartiallySolves rest -> ((dieType :) <$> (trySatisfy remainingThrow $ Z.toList $ Z.replace rest zp))
        useValue = mconcat $ mapZipper tryUse goalZipper
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
  let sortedDice = sort dieTypes
  thr <- throw sortedDice
  case trySatisfy thr goals of
    Just _ -> return True
    Nothing -> Prob.norm $ do
      let groupZipper = Z.fromList $ group sortedDice
          removeElem z = join $ Z.toList $ Z.replace (drop 1 (Z.cursor z)) z
          possibilities = mapZipper removeElem groupZipper
      outcomes <- mapM (satisfy goals) possibilities
      return $ or outcomes

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
  let probability = 100 * (id Prob.?? satisfy goals dice)
  putStrLn $ "Success probability: " ++ show probability ++ "%"
