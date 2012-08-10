:- module(docopt,
    [
        version/1,
        docopt/2
    ]).

/** <module> Docopt parser.
 *
 *  This module contains predicates for the docopt command line argument
 *  parser. See <http://docopt.org/>.
*/

:- include(docopt(common)).

:- use_module(docopt(parsing), []).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor and patch version
%   numbers of this library.

version([0,0,1]).

%%  docopt(+Doc, ?Options) is semidet.
%
%   True if Doc is an atom containing a help message and Options is the
%   corresponding command line arguments given to the program, according to
%   the docopt specification.

docopt(Doc, Options) :-
    program_arguments(ProgramArgs),
    core:atom_chars(Doc, DocChars),
    docopt(DocChars, ProgramArgs, Options).

%%  program_arguments(?ProgramArgs) is semidet.
%
%   True if ProgramArgs is the list of arguments given to the program
%   excluding system arguments ('swipl -O' etc.). System and program
%   arguments are separated by two dashes.

program_arguments(ProgramArgs) :-
    core:current_prolog_flag(argv, AllArgs),
    core:append(_SystemArgs, [--|ProgramArgs], AllArgs).

%%  docopt(+Doc, +Args, ?Options) is semidet.
%
%   True if Options is the collection of arguments compiled from the
%   usage atom Doc and the arguments Args.

docopt('', _Args, []) :- !.
docopt(Doc, _Args, Options) :-
    core:atom_chars(Doc, _DocChars),
    core:downcase_atom(Doc, DocLower),
    core:atom_chars(DocLower, DocCharsLower),
    core:phrase(docopt:parse_usage(_ProgramName), DocCharsLower, _Rest),
    Options = [].

parse_usage(ProgramName) -->
    [u,s,a,g,e,:],
    parsing:skip_white,
    parsing:word(ProgramName).

%%  args_empty(?Args) is semidet.
%
%   True if Args is an empty argument collection.

args_empty([]).

%%  args_get(+Args, +Key, ?Value) is semidet.
%
%   True if Value is the value associated with Key in Args.
%   Fails if Key is not found or does not match Value.

args_get([K-V|_], K, V) :- !.
args_get([_|Pairs], K, V) :-
    args_get(Pairs, K, V).

%%  args_put(+Args, +Key, +Value, ?NewArgs) is semidet.
%
%   True if NewArgs is Args with the addition or update of the
%   association Key-Value.

args_put([], K, V, [K-V]).
args_put([K-_|Pairs], K, V, [K-V|Pairs]) :- !.
args_put([Other|Pairs], K, V, [Other|Pairs1]) :-
    args_put(Pairs, K, V, Pairs1).
