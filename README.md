# JustParse

A simple and comprehensive Haskell parsing library

### Differences and similarities from `Parsec` and `Attoparsec`

##### Similarities to `Parsec`
* Allows for parsing arbitrary Streams 
* Makes extensive use of combinators

##### Similarities to `Attoparsec`
* Allows for return partial results
* Is not a monad transformer 

##### Differences from both
* Returns a list of all possible parses
* Allows for conversion of a regular expression to a parser 

### Non-greedy parsing

The last item in that list is the most important. In both `Parsec` and 
`Attoparsec`, parsers such as "many" are greedy. That is, they will consume 
as much input as possible. This is makes writing a parser equivalent to the 
regular expression `a[ab]*a` a bit tricky. We would be tempted to write:

    p = do
        a <- char 'a'
        b <- many (oneOf "ab")
        c <- char 'a'
        return (a,b,c)

The problem is that the `many (oneOf "ab")` parser is greedy, and will 
consume the final `char 'a'` term that we try to bind to `c`, resulting in a failed parse. We could write this using a combination of `try`, 
`notFollowedBy`, and `lookAhead` parsers, but it doesn't capture the same 
elegance of "parse an 'a', then some 'a's or 'b's, then an 'a'". 

JustParse removes this problem with its ability to match all possible 
parses. That same parser in JustParse (with `many` changed to `many_`), 
applied to the input `abaaba` would return:

    ('a', "b", 'a')
    ('a', "ba", 'a')
    ('a', "baab", 'a')
    Partial

The Partial result represents the branch of the parse tree in which the 
"many\_" term consumes all available input. Supplying it with something 
like `a` would yield an additional result of `('a', "baaba", 'a')` (and 
another `Partial`), since it will resume parsing.

For compatability reasons, the parsers `many`, `sepBy`, etc. operate as 
they do in `Parsec` and `Attoparsec`. To use the ones that return all 
possible parses, merely append an underscore, such as `many_` and `sepBy_`. 
For general purpose parse branching, one may use the `branch` function, or 
its infix name of `<||>`.

### Regex convenience

JustParse provides the `regex` parser. This parser is of the type 
`Stream s Char => Parser s Match`. A `Match` object contains all of the 
text matched within it, and a list of `Match` objects which represent any 
subgroups (which may themselves contain subgroups, etc). These regular 
expressions are truly regular in that they do not have backreferences (for 
now). If one only wants the entirety of the matched text, the `regex'` 
parser will do that. Example:

    p = regex' "ab+cd?"

is equivalent to the standard parser:

    p = do
        a <- char 'a'
        b <- many1 (char 'b')
        c <- char 'c'
        d <- option "" (string "d")
        return (a:b++c:d)
       
So for small `String` parsers, or for use in larger parsers, the `regex` or 
`regex'` parsers prove very convenient.
