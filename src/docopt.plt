:- begin_tests('docopt:docopt/2').

test('empty doc gives no options', [true(Actual == Expected)]) :-
    Expected = [],
    docopt:docopt('', Actual).

:- end_tests('docopt:docopt/2').
