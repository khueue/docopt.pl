:- module(docopt,
    [
        version/1,
        docopt/2
    ]).

/** <module> Docopt parser.
 *
 *  This module contains predicates implementing the docopt command line
 *  argument parser.
 */

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([0,0,1]).

%%  docopt(+Doc, ?Options) is semidet.
%
%   True if Doc is an atom containing a help message and Options is the
%   corresponding command line arguments given to the program, according to
%   the docopt specification.

docopt('', []) :- !.
% docopt(Doc, Options).
