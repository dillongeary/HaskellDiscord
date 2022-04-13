{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib (discordMain) where
import           Control.Monad
import Control.Monad.Reader
import Data.Text ( pack, replace, split, unpack, Text )
import qualified Data.Text.IO as TIO
import System.Random
import Text.Regex.TDFA ( (=~) )

import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import qualified Control.Monad.IO.Class
import Text.Read (Lexeme(String), prec)
import qualified Control.Monad as Prelude

data Outcomes a = Rolls [a] | Modifier a deriving (Ord,Eq, Show)

outcomesSum :: [Outcomes Int] -> Int
outcomesSum [] = 0
outcomesSum (Rolls x:xs) = sum x + outcomesSum xs
outcomesSum (Modifier x:xs) = x + outcomesSum xs

list2tuple :: [a] -> (a,a)
list2tuple [a,b] = (a,b)

-- | Replies "pong" to every message that starts with "ping"
discordMain :: IO ()
discordMain = do
    userFacingError <- runDiscord $ def
             { discordToken = "Bot ODY0NjI2NDY2MzM0NDQxNDcy.YO4MJQ.NwgnD4GtsvTdFGw106a9ZH1ce7Y"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isDie m && not (fromBot m)) $ do --void $ restCall $ R.CreateMessage (messageChannelId m) "message read"
                                                              output <- rollDice m $ replace "-" "+-" (messageContent m)
                                                              void $ restCall $ R.CreateMessage (messageChannelId m) output
    _ -> return ()

rollDice :: (Control.Monad.IO.Class.MonadIO m) => Message -> Text -> m Text
rollDice m xs = do gen <- newStdGen
                   let rollsOutcome = getRolls gen (Prelude.map list2tuple rolls)
                   --rollsOutcome <- mapM (foo . list2tuple) rolls
                   let output = rollsOutcome ++ modifiers
                   --return $ pack $ show rollsOutcome
                   return $ pack $ prettyPrint m output
  where modifiers :: [Outcomes Int]
        modifiers = Prelude.map (Modifier . \[x] -> x) $ Prelude.filter (\x -> Prelude.length x == 1) dices
        rolls = Prelude.filter (\x -> Prelude.length x == 2) dices
        dices :: [[Int]]
        dices = Prelude.map (Prelude.map (smartRead . unpack) . Data.Text.split (\x -> x == 'D' || x == 'd')) (Data.Text.split (== '+') xs)

foo :: (Random (m a), Monad m, Num a, MonadIO ((->) (Integer, a))) => (Int,a) -> m (Outcomes a)
foo (a,s) = fmap Rolls (replicateM a (randomIO (1,s)))


getRolls :: (Random a, RandomGen t, Num a) => t -> [(Int,a)] -> [Outcomes a]
getRolls g [] = []
getRolls g ((a,s):xs) = Rolls o : getRolls ng xs
  where (o,ng) = doRolls (Prelude.replicate a s) g []
        doRolls [] g os = (os,g)
        doRolls (x:xs) g os = doRolls xs bar (foo:os)
          where (foo,bar) = randomR (1,x) g

smartRead :: String -> Int
smartRead [] = 1
smartRead xs = read xs

--TODO = Replace You with user Mention

prettyPrint :: Message -> [Outcomes Int] -> String
prettyPrint m os = "<@" ++ username ++ "> rolled " ++ stuff os
  where username = show $ fromEnum $ userId $ messageAuthor m
        stuff :: [Outcomes Int] -> String
        stuff [Modifier x] = "**" ++ show x ++ "**"
        stuff [Rolls [x]] = "**" ++ show x ++ "**"
        stuff xs = "**" ++ total ++ "**   (" ++ Prelude.foldl1 (\x y -> x ++ ", " ++ y) ( breakdown xs ) ++ ")"
          where total = show $ outcomesSum xs
                breakdown :: [Outcomes Int] -> [String]
                breakdown [] = []
                breakdown (Modifier x : xs) = show x : breakdown xs
                breakdown (Rolls x : xs) = extraBreakdown 0 x : breakdown xs
                  where extraBreakdown :: Int -> [Int] -> String
                        extraBreakdown t [] = error "aaaaaaa"
                        extraBreakdown t [x] = show x ++ " = " ++ show (t+x)
                        extraBreakdown t (x:xs) = show x ++ " + " ++ extraBreakdown (t+x) xs


{- prettyPrint :: Message -> Int -> ([Int],[Int]) -> String
prettyPrint m total ([r],[]) = "<@" ++ username ++ "> rolled **" ++ show total ++ "**"
  where username = show $ fromEnum $ userId $ messageAuthor m
prettyPrint m total (rolls,modifiers) = "<@" ++ username ++ "> rolled **" ++ show total ++ "**   (" ++ formatRolls rolls 0 ++ formatModifiers modifiers ++ ")"
  where formatRolls :: [Int] -> Int -> String
        formatRolls [] _ = ""
        formatRolls [x] 0 = show x
        formatRolls [x] i = show x ++ " = " ++ show (i + x)
        formatRolls (x:xs) i = show x ++ " + " ++ formatRolls xs (i+x)
        formatModifiers :: [Int] -> String
        formatModifiers [] = ""
        formatModifiers xs = Prelude.concatMap (\x -> ", " ++ show x) xs
        username = show $ fromEnum $ userId $ messageAuthor m -}

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isDie :: Message -> Bool
isDie x = replace " " "" (messageContent x) =~ diceRegex :: Bool
  where diceRegex :: String
        diceRegex = "^(([1-9][0-9]?)?[d|D][1-9][0-9]*|[-]?[0-9]+)([+]([1-9][0-9]?)?[d|D][1-9][0-9]*|[+|-]?[0-9]+)*$"