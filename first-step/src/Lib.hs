module Lib
    ( someFunc
    , myListLen
    ) where

import Control.Applicative(liftA2)

someFunc :: IO ()
someFunc = testLift
-- someFunc = do
--     putStrLn "Plz input ur name:"
--     inpStr <- getLine
--     putStrLn $ "Welcome to my zone, " ++ inpStr ++ "!"
--     len2 <- return (show (listLen2 inpStr))
--     putStrLn $ "ur name has " ++ len2 ++ " letters!"
--     putStrLn (printMyType (TYPE_2 1 "kinnon"))

myListLen :: [Int] -> Int
myListLen (x:xs) = 1 + myListLen xs
myListLen [] = 0

listLen2 :: [a] -> Int
listLen2 = foldr step 0 where
    step :: a -> Int -> Int
    step _ sum = sum + 1

data MyType a = TYPE_NONE
              | TYPE_1 a
              | TYPE_2 Int a

printMyType :: Show a => MyType a -> String
printMyType TYPE_NONE = "TYPE_NONE"
printMyType (TYPE_1 a) = "TYPE_1" ++ show a
printMyType (TYPE_2 a b) = "TYPE_2 " ++ show a ++ " " ++ show b

testGuard :: Int -> Int
testGuard n
    | isEven n = n `div` 2
    | otherwise      = 3 * n + 1

isEven:: Int -> Bool
isEven n
    | n `mod` 2 == 0 = True
    | otherwise      = False

customSeq :: Monad m => [m a] -> m [a]
customSeq [] = return []
-- customSeq (ma:mas) = ma >>= \a -> customSeq mas >>= \as -> return (a:as)
customSeq (ma:mas) = do
    a <- ma
    b <- customSeq mas
    return (a:b)

data Player = Player {
    getName::String,
    getSex::Sex,
    getProfessional::Professional
} deriving(Show, Eq)

data Sex = Male | Female deriving(Show, Eq)
data Professional = Coder
                  | Doctor
                  | Teacher
                    deriving(Show, Eq)

testLift :: IO()
testLift = do
    putStrLn $ show player1
    print $ show player2
    print $ show player3 where
        name1, name2, name3 :: Maybe String
        name1 = Just "Gouzi"
        name2 = Just "Duantou"
        name3 = Nothing
        
        sex1, sex2, sex3 :: Maybe Sex
        sex1 = Just Male
        sex2 = Nothing
        sex3 = Just Female
        
        p1, p2, p3 :: Maybe Professional
        p1 = Just Coder
        p2 = Just Doctor
        p3 = Just Teacher
        
        player1 = Player <$> name1 <*> sex1 <*> p1
        player2 = Player <$> name2 <*> sex2 <*> p2
        player3 = Player <$> name3 <*> sex3 <*> p3




