{-# LANGUAGE LambdaCase #-}
module Day1 where

import Data.List (foldl')

main :: IO ()
main = do
    depths <- map read . lines <$> getContents
    putStrLn $ "Part 1: " <> show (countDepthIncreases depths)
    putStrLn $ "Part 2: " <> show (countWindowIncreases depths)

countDepthIncreases :: [Int] -> Int
countDepthIncreases =
    countIncreases

countWindowIncreases :: [Int] -> Int
countWindowIncreases = countIncreases . makeWindows
    where
        makeWindows :: [Int] -> [Int]
        makeWindows = \case
            x : y : z : rest ->
                (x + y + z) : makeWindows (y : z : rest)
            _ ->
                []


countIncreases :: [Int] -> Int
countIncreases =
    snd . foldl'
        (\(mbLastDepth, count) nextDepth ->
            case mbLastDepth of
                Nothing -> (Just nextDepth, count)
                Just lastDepth
                    | nextDepth > lastDepth -> (Just nextDepth, count + 1)
                    | otherwise -> (Just nextDepth, count)
        )
        (Nothing, 0)
