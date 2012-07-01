% Acts as an interface to the system. Sets up load paths and provides
% a predicate for running the test suite.

setup_globals :-
    % For optimized compiles, tests are by default ignored.
    set_test_options([load(always)]),
    % For maximum compatibility.
    set_prolog_flag(language, iso).
    % Try to make everything as UTF-8 as possible.
    % set_prolog_flag(encoding, utf8). % When using streams, global setting.
    % Hunting implicit dependencies is easier without autoload.
    % set_prolog_flag(autoload, false),
    % Displays how modules and such are located.
    % set_prolog_flag(verbose_file_search, true).

setup_load_paths :-
    prolog_load_context(directory, Root), % Available only during compilation.
    setup_path(Root, '/src', docopt).

setup_path(PathPrefix, PathSuffix, Name) :-
    atom_concat(PathPrefix, PathSuffix, Path),
    asserta(user:file_search_path(Name, Path)).

:- setup_globals.
:- setup_load_paths.

test :-
    load_project_modules,
    load_project_tests,
    run_test_suite.

repl :-
    load_project_modules,
    use_module(library(test_wizard), []),
    set_prolog_flag(log_query_file, 'querylog.pl').

cov :-
    load_project_modules,
    load_project_tests,
    run_test_suite_with_coverage.

load_project_modules :-
    use_module(library(pldoc), []), % Load first to enable comment processing.
    use_module(docopt(docopt), []).

load_project_tests :-
    plunit:load_test_files([]).

run_test_suite :-
    core:format('~n% Run tests ...~n'),
    plunit:run_tests.

% TODO: Coverage doesn't seem to work at all.
run_test_suite_with_coverage :-
    core:format('~n% Run tests ...~n'),
    plunit:show_coverage(plunit:run_tests).
