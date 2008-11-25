-module(test_erlcfg_ast).
-include_lib("eunit/include/eunit.hrl").



erlcfg_traverse_single_set_test() ->
    Ast = [{set, one, 3}, []],
    Expected = {c, '', [
            {d, one, 3}
        ]
    },
    ?assertEqual(Expected, erlcfg_ast:traverse(Ast)).
