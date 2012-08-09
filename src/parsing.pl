:- module(parsing,
    [
        word/3,
        alnum/3,
        alnums/3,
        skip_white/2
    ]).

/** <module> Low-level DCGs for parsing.
 */

:- include(docopt(common)).

word(Word) -->
    alnums(Cs),
    { core:atom_chars(Word, Cs) }.

alnum(C) -->
    [C],
    { core:char_type(C, alnum) }.

alnums([C|Cs]) -->
    alnum(C),
    !,
    alnums(Cs).
alnums([]) --> [].

skip_white -->
    [C],
    { core:char_type(C, white) },
    !,
    skip_white.
skip_white --> [].
