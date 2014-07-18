import Data.JustParse
import Data.JustParse.Char hiding (print)
import Data.JustParse.Combinator

parser2 = manyTill (manyTill anyChar eof) eof

main = do
    let r = (runParser parser2 "a")
    print r
    print (finalize r)
    print (finalize (finalize r))
