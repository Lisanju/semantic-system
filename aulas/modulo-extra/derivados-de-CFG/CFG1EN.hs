module CFG1EN where

-- We begin encoding of languages and derivations
-- The simplest way to embed a language is with strings
-- Terminals and non-terminals are uniformly strings.

-- Definitions (or, `bookmarks') and CFG-like derivations

john   = "John"
mary   = "Mary"
like   = "likes"

r2 tv np = tv ++ " " ++ np
r1 np vp = np ++ " " ++ vp

sentence = r1 john (r2 like mary)
