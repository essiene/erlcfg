-record(listof, {type}).

-record(typedef, {
        name,
        options}).

-record(block, {
        name,
        child}).

-record(declaration, {
        type,
        name}).

-record(cons, {
        head,
        tail=nil}).

-record(validator, {
        type,
        test
    }).

-define(DEFAULT_TYPE_MAP, [
        {integer, #validator{type=integer, test=fun is_integer/1}},
        {float, #validator{type=float, test=fun is_float/1}},
        {atom, #validator{type=atom, test=fun is_atom/1}},
        {string, #validator{type=string, test=fun is_binary/1}},
        {boolean, #validator{type=boolean, test=fun is_boolean/1}}
    ]).
