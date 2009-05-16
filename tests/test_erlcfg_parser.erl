-module(test_erlcfg_parser).
-include_lib("eunit/include/eunit.hrl").
-include("erlcfg.hrl").

parse_bool_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {bool, 1, true}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, true, nil}]},
    ?assertEqual(Expected, Result).

parse_integer_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {integer, 1, 51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, 51, nil}]},
    ?assertEqual(Expected, Result).

parse_float_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {float, 1, -51.0e-10}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, -51.0e-10, nil}]},
    ?assertEqual(Expected, Result).

parse_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {atom, 1, foo51}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, foo51, nil}]},
    ?assertEqual(Expected, Result).

parse_quoted_atom_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {quoted_atom, 1, '.foo@51'}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, '.foo@51', nil}]},
    ?assertEqual(Expected, Result).

parse_string_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {string, 1, <<"A String">>}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, <<"A String">>, nil}]},
    ?assertEqual(Expected, Result).

parse_variable_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {variable, 1, foo.bar}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {get, foo.bar}, nil}]},
    ?assertEqual(Expected, Result).

parse_single_block_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'{', 1}, {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, {'}', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [[{block, foo, noop}, [{set, foo, moo, nil}], {endblock, noop, noop}]]},
    ?assertEqual(Expected, Result).

parse_empty_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'(', 1}, {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, nil, nil}]},
    ?assertEqual(Expected, Result).

parse_single_value_list_assignment_test() ->
    Tokens = [{atom, 1, foo}, {'=', 1}, {'(', 1}, {atom, 1, bar}, {')', 1}, {';', 1}],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok, [{set, foo, {cons, bar, nil}, nil}]},
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
    Expected = {ok, [{set, foo, {cons, bar, {cons, 5, {cons, 0.59, {cons, <<"A String">>, nil}}}}, nil}]},
    ?assertEqual(Expected, Result).

parse_single_nested_blocks_assignment_test() ->
    Tokens = [
        {atom, 1, foo}, {'{', 1}, 
            {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
            {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {atom, 1, foo2}, {'{', 1},
                {atom, 1, foo}, {'=', 1}, {atom, 1, moo}, {';', 1}, 
                {atom, 1, foo1}, {'=', 1}, {atom, 1, moo1}, {';', 1}, 
            {'}', 1},
        {'}', 1}
    ],
    Result = erlcfg_parser:parse(Tokens),
    Expected = {ok,
        [
            [
                {block, foo, noop}, 
                [
                    {set, foo, moo, nil},
                    [
                        {set, foo1, moo1, nil},
                        [
                            [
                                {block, foo2, noop}, 
                                [
                                    {set, foo, moo, nil},
                                    [
                                        {set, foo1, moo1, nil}
                                    ]
                                
                                ], 
                                {endblock, noop, noop}
                            ]
                        ]
                    ]
                ],
                {endblock, noop, noop}
            ]
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
        [{set, foo, false, nil},
            [{set, foo, 51, nil},
                [{set, foo, -51.0e-10, nil}, 
                    [{set, foo, foo51, nil}, 
                        [{set, foo, <<"A String">>, nil}, 
                            [{set, foo, {get, foo.bar}, nil}]
                        ]
                    ]
                ]
            ]
        ]
    },
    ?assertEqual(Expected, Result).
