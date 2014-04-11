# Manual Proofs of Mathematical Laws

### Left Identity Law for the Parser Monad
    NTS: return a >>= f ~ f a

    1.  return a >>= f                                  -- Begin
    2.  Parser $ \s -> [Done a s] >>= f                 -- Definition of return
    3.  Parser $ \s -> [Done a s] >=> g                 -- Definition of bind
    4.  Parser $ \x -> (\s -> [Done a s]) x >>= g       -- Definition of (>=>)
    5.  Parser $ \x -> [Done a x] >>= g                 -- Apply x to the lambda
    6.  Parser $ \x -> concat (map g [Done a x])        -- Definition of bind for lists
    7.  Parser $ \x -> concat [g (Done a x)]            -- Inline map
    8.  Parser $ \x -> g (Done a x)                     -- concat of a 1-item list is the item
    9.  Parser $ \x -> parse (f a) x                    -- Definition of g for a Done
    10. Parser $ parse (f a)                            -- Eta-reduce
    11. f a                                             -- Definition of Parser 
    Q.E.D.

### Right Identity Law for the Parser Monad
    NTS: m >>= return ~ m

    1.  Parser $ p >=> g                                -- Begin
    2.  Parser $ \x -> p x >>= g                        -- Definition of (>=>)
    3.  Parser $ \x -> concat (map g (p x))             -- Definition of (>>=) for lists

        CASE 1: (p x) !! n ~ Done a s
        1. g (Done a s)                                     -- Begin
        2. parse (return a) s                               -- Definition of g
        3. parse (Parser $ \s -> [Done a s]) s              -- Definition of return
        4. (\s -> [Done a s]) s                             -- Apply record
        5. [Done a s]                                       -- Beta-reduce

        CASE 2: (p x) !! n ~ Partial p
        1. g (Partial p)                                    -- Begin
        2. [Partial $ p >=> g]                              -- Definition of g
        ?!?!?!?!?!?!?!?!?!?!?!?!?
        I think this calls for Structural Induction?

    4. Parser $ \x -> p x                               -- When f ~ return, g r = [r], hence concat (map g (p x)) ~ id
    5. Parser p                                         -- Eta-reduce
    6. m                                                -- Definition of m
    Q.E.D.

