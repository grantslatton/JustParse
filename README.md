# JustParse

A simple and comprehensive Haskell parsing library

### Differences and similarities from `Parsec` and `attoparsec`

##### Similarities to `Parsec`
* Allows for parsing arbitrary Streams 
* Makes extensive use of combinators
* Returns relatively verbose errors
* Allows for custom renaming of parsers

##### Similarities to `attoparsec`
* Allows for return partial results
* Is not a monad transformer 

##### Differences from both
* Returns a list of all possible parses
* Allows for conversion of a regular expression to a parser 

### Non-greedy parsing

The last item in that list is the most important. In both `Parsec` and `attoparsec`, 
parsers such as "many" are greedy. That is, they will consume as much input as
as possible. This is makes writing a parser equivalent to the regular expression
`[A-z][A-z0-9]\*[A-z]` a bit tricky. We would be tempted to write:

    p = do
        a <- alpha
        b <- many alphaNum
        c <- alpha
        return (a,b,c)

The problem is that the `many alphaNum` parser is greedy, and will consume the 
final `alpha` term that we try to bind to `c`, resulting in a failed parse. We could
write this using a combination of `try`, `notFollowedBy`, and `lookAhead` parsers, 
but it doesn't capture the same elegance of "parse a letter, then some letters and 
digits, then a letter". 

JustParse removes this problem with its ability to match all possible parses. That
same parser in JustParse, applied to the input `ab2c3d` would return:

    ('a', "", 'b')
    ('a', "b2", 'c')
    ('a', "b2c3", 'd')
    Partial

The Partial result represents the branch of the parse tree in which the "many" term
consumes all available input. Supplying it with something like `z~` would yield an
additional result of `('a', "b2c3d", 'z')`, since it will fail (and thus stop parsing)
upon encountering the `~` character (which is not alphanumeric).

If this behavior is undesirable (e.g. for performance reasons), or unneeded, a 
parser can be wrapped in the `greedy` parser (e.g. `greedy (many alpha)`) to force
behavior similar to `Parsec` or `attoparsec`.

### Regex convenience

JustParse provides the `regex` parser. This parser is of the type 
`Stream s Char => Parser s Match`. A `Match` object contains all of the text matched 
within it, and a list of `Match` objects which represent any subgroups (which may 
themselves contain subgroups, etc). These regular expressions are truly regular in
that they do not have backreferences (for now). If one only wants the entirety of
the matched text, the `regex'` parser will do that. Example:

    p = regex' "ab+cd?"

is equivalent to the standard parser:

    p = do
        a <- char 'a'
        b <- many1 (char 'b')
        c <- char 'c'
        d <- option "" (string "d")
        return (a:b++c:d)
       
So for small `String` parsers, or for use in larger parsers, the `regex` or `regex'`
parsers prove very useful.
