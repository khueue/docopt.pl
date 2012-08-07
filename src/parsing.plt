:- include(docopt(common)).

:- begin_tests('parsing:word/3').

test('word/3', [true(Got == Expected)]) :-
    Expected = hello,
    phrase(parsing:word(Got), [h,e,l,l,o], _).

:- end_tests('parsing:word/3').
