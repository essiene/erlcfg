-module(test_erlcfg_ast).
-include_lib("eunit/include/eunit.hrl").



erlcfg_traverse_single_set_test() ->
    Ast = [{set, one, 3}, []],
    Expected = {c, '', [
            {d, one, 3}
        ]
    },
    ?assertEqual(Expected, erlcfg_ast:traverse(Ast)).

erlcfg_traverse_single_get_test() ->
    Ast = [{get, one, nil}, []],
    Expected = {not_found, one},
    ?assertThrow(Expected, erlcfg_ast:traverse(Ast)).

erlcfg_traverse_single_val_test() ->
    Ast = [{val, -1.5e-15, nil}, []],
    Expected = {c, '', []},
    ?assertEqual(Expected, erlcfg_ast:traverse(Ast)).
