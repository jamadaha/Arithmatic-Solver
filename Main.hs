import Text.ParserCombinators.Parsec;
import Text.Read
import Control.Exception
import Data.Data

data MyException = SyntaxError | OtherError
    deriving(Show, Typeable);

instance Exception MyException;

data Symbol = Error | Num | Add | Subtract
    deriving(Eq, Show);

indexOf :: [Char] -> Char -> Int -> Int;
indexOf input c index = do
    if null input
        then -1;
        else if head input == c
            then index;
            else indexOf (drop 1 input) c (index + 1);

-- Gets substring until char or end of string
getSubString :: String -> Char -> String;
getSubString input c = do
    let charIndex = indexOf input c 0;
    if charIndex == -1
        then input
        else take charIndex input

isSymbol :: String -> Bool;
isSymbol str = do
    case readMaybe str :: Maybe Int of
        Just x -> True
        Nothing ->
            (str == "+" ||
            str == "-")

toSymbol :: String -> Symbol;
toSymbol str = do
    case readMaybe str :: Maybe Int of
        Just x -> Num
        Nothing -> 
            if str == "+"
                then Add
                else if str == "-"
                    then Subtract
                    else Error

lexAnal :: String -> [Symbol] -> [Symbol]
lexAnal input symbols = do
    let substr = getSubString input ' ';
    let validSymbol = isSymbol substr;
    if null input
        then reverse symbols
        else if validSymbol
            then lexAnal (drop (length substr + 1) input) (toSymbol substr: symbols)
            else throw SyntaxError; 
            

main = do
    print (lexAnal "12 + 47 - 9" []);

-- "12 +"
-- "12"


