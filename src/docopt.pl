:- module(docopt,
    [
        version/1,
        docopt/2
    ]).

% :- include(misc(common)).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([0,0,1]).

%%  docopt(+DocAtom, -Args) is semidet.
%
%   XXX

docopt('', []).
