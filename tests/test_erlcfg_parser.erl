-module(test_erlcfg_parser).
-include_lib("eunit/include/eunit.hrl").

parse_bool_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {bool, 1, true}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, true, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_integer_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, 51, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_float_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {float, 1, -51.0e-10}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, -51.0e-10, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {atom, 1, foo51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, foo51, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_quoted_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {quoted_atom, 1, '.foo@51'}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, '.foo@51', noop}}, []]},
    ?assertEqual(Expected, Result).

parse_string_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {string, 1, <<"A String">>}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, <<"A String">>, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_variable_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {variable, 1, foo.bar}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {val, {get, foo.bar, noop}, noop}}, []]},
    ?assertEqual(Expected, Result).

parse_single_block_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'{', 1}, {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, {'}', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [[{block, foo, noop}, [{set, foo, {val, moo, noop}}, []], {endblock, noop, noop}],[]]},
    ?assertEqual(Expected, Result).

parse_empty_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'(', 1}, {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, nil}, []]},
    ?assertEqual(Expected, Result).

parse_single_value_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'(', 1}, {atom, 1, bar}, {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {cons, {val, bar, noop}, nil}}, []]},
    ?assertEqual(Expected, Result).

parse_multiple_value_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, 
        {'(', 1}, 
            {atom, 1, bar}, 
            {',', 1},
            {integer, 1, 5},
            {',', 1},
            {float, 1, 0.59},
            {',', 1},
            {string, 1, <<"A String">>},
        {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {cons, {val, bar, noop}, {cons, {val, 5, noop}, {cons, {val, 0.59, noop}, {cons, {val, <<"A String">>, noop}, nil}}}}}, []]},
    ?assertEqual(Expected, Result).

parse_single_nested_blocks_assignment_test() ->
    Tokens = [
        {atom, 1, foo}, {'=', 1}, {'{', 1}, 
            {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
            {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {atom, 1, foo2}, {'=', 1}, {'{', 1},
                {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
                {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {'}', 1}, {';', 1},
        {'}', 1}, {';', 1}
    ],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok,
        [
            [
                {block, foo, noop}, 
                [
                    {set, foo, {val, moo, noop}},
                    [
                        {set, foo1, {val, moo1, noop}},
                        [
                            [
                                {block, foo2, noop}, 
                                [
                                    {set, foo, {val, moo, noop}},
                                    [
                                        {set, foo1, {val, moo1, noop}},
                                        []
                                    ]
                                
                                ], 
                                {endblock, noop, noop}
                            ],
                            []
                        ]
                    ]
                ],
                {endblock, noop, noop}
            ], 
            []
        ]
    },
    ?assertEqual(Expected, Result).

parse_mixed_assignment_test() ->
    Tokens = [
        {atom, 1, foo}, {'=', 1}, {bool, 1, false}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {float, 1, -51.0e-10}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {atom, 1, foo51}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {string, 1, <<"A String">>}, {';', 1},
        {atom, 1, foo}, {'=', 1}, {variable, 1, foo.bar}, {';', 1}
    ],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, 
        [{set, foo, {val, false, noop}},
            [{set, foo, {val, 51, noop}},
                [{set, foo, {val, -51.0e-10, noop}}, 
                    [{set, foo, {val, foo51, noop}}, 
                        [{set, foo, {val, <<"A String">>, noop}}, 
                            [{set, foo, {val, {get, foo.bar, noop}, noop}}, []]
                        ]
                    ]
                ]
            ]
        ]
    },
    ?assertEqual(Expected, Result).


