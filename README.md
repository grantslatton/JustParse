JustParse
=========

A simple and comprehensive Haskell parsing library

### Differences and similarities from Parsec and Attoparsec

* Allows for partial parsing (like Attoparsec)
* Allows for parsing arbitrary Streams (like Parsec)
* Is not a monad transformer (like Attoparsec, unlike Parsec)
* Returns a list of all possible parses (unlike Attoparsec and Parsec)

### Non-greedy parsing

The last item in that list is the most important. In both Parsec and AttoParsec, 
parsers such as "many" are greedy. That is, they will consume as much input as
as possible. This is makes writing a parser equivalent to the regular expression
"[A-z][A-z0-9]\*[A-z]" a bit tricky. We would be tempted to write:

    p = do
        a <- alpha
        b <- many alphaNum
        c <- alpha
        return (a,b,c)

The problem is that the "many alphaNum" parser is greedy, and will consume the 
final "alpha" term that we try to bind to c, resulting in a failed parse.

JustParse removes this problem with its ability to match all possible parses. That
same parser in JustParse, applied to the input "ab2c3d" would return:

    ('a', "", 'b')
    ('a', "b2", 'c')
    ('a', "b2c3", 'd')
    Partial

The Partial result represents the branch of the parse tree in which the "many" term
consumes all available input. Supplying it with something like "z~" would yield an
additional result of ('a', "b2c3d", 'z'), since it will fail (and thus stop parsing)
upon encountering the '~' character (which is not alphanumeric).
