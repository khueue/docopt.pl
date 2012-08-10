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
    csyms(Cs),
    { core:atom_chars(Word, Cs) }.

csym(C) -->
    [C],
    { core:char_type(C, csym) }.

csyms([C|Cs]) -->
    csym(C),
    !,
    csyms(Cs).
csyms([]) --> [].

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
