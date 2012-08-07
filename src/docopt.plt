:- include(docopt(common)).

:- begin_tests('docopt:docopt/3').

test('empty doc gives no options', [true(Got == Expected)]) :-
    Usage = '',
    Args = [],
    Expected = [],
    docopt:docopt(Usage, Args, Got).

test('minimal doc', [true(Got == Expected)]) :-
    Usage = 'Usage: the_program',
    Args = [],
    Expected = [],
    docopt:docopt(Usage, Args, Got).

test('minimal doc xxx', [true(Got == Expected)]) :-
    Usage = 'Usage: the_program <arg1>',
    Args = [42],
    Expected = [],
    docopt:docopt(Usage, Args, Got).

/*
test('xxx 1', [true(Got == Expected)]) :-
    Usage =
        'Usage: my_program [-hso FILE] [--quiet | --verbose] [INPUT ...]

        -h --help    show this
        -s --sorted  sorted output
        -o FILE      specify output file [default: ./test.txt]
        --quiet      print less text
        --verbose    print more text',
    Args = [ship, 'Guardian', move, 100, 150, '--speed=15'],
    Expected = [xxx],
    docopt:docopt(Usage, Args, Got).
*/

:- end_tests('docopt:docopt/3').
