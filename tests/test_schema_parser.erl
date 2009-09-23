-module(test_schema_parser).
-include_lib("eunit/include/eunit.hrl").
-include("schema.hrl").

parse_int_declaration_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, int, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).

parse_int_declaration_with_default_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {'=', 1}, {integer, 1, 2}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, int, foo, 2}]},
    ?assertEqual(Expected, Result).

parse_float_declaration_test() ->
    Tokens = [{datatype, 1, float}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, float, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).

parse_bool_declaration_test() ->
    Tokens = [{datatype, 1, bool}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, bool, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).

parse_atom_declaration_test() ->
    Tokens = [{datatype, 1, atom}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, atom, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).

parse_string_declaration_test() ->
    Tokens = [{datatype, 1, string}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, string, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).

parse_typedef_test() ->
    Tokens = [{keyword_type, 1, type}, 
        {atom, 1, newtype},
        {'=', 1}, 
            {atom, 1, foo}, 
                {'|', 1}, 
            {atom, 1, bar}, 
         {';', 1}
     ],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{typedef, newtype, {cons, foo, {cons, bar, nil}}
            }
    ]},
    ?assertEqual(Expected, Result).

parse_empty_block_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, []}]},
    ?assertEqual(Expected, Result).

parse_block_with_declarations_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, [
                    {declaration, string, foo, ?ERLCFG_SCHEMA_NIL},
                    {declaration, int, bar, ?ERLCFG_SCHEMA_NIL}
                ]}]},
    ?assertEqual(Expected, Result).

parse_block_with_nested_block_test() ->
    Tokens = [
        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {atom, 1, block1},
            {'{', 1},
                {datatype, 1, string}, {atom, 1, foo}, {';', 1},
                {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {'}', 1},
        {'}', 1}
    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{block, block1, [
                    {declaration, string, foo, ?ERLCFG_SCHEMA_NIL},
                    {declaration, int, bar, ?ERLCFG_SCHEMA_NIL},
                    {block, block1, [
                        {declaration, string, foo, ?ERLCFG_SCHEMA_NIL},
                        {declaration, int, bar, ?ERLCFG_SCHEMA_NIL}
                    ]}
                ]}]},
    ?assertEqual(Expected, Result).

parse_complex_with_typedefs_and_blocks_test() ->
    Tokens = [
        {keyword_type, 1, type}, 
            {atom, 1, newtype1},
            {'=', 1}, 
                {atom, 1, foo}, 
                    {'|', 1}, 
                {atom, 1, bar}, 
             {';', 1},

        {atom, 1, block1},
        {'{', 1},
            {datatype, 1, string}, {atom, 1, foo}, {';', 1},
            {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {atom, 1, block1},
            {'{', 1},
                {datatype, 1, string}, {atom, 1, foo}, {';', 1},
                {datatype, 1, int}, {atom, 1, bar}, {';', 1},
            {'}', 1},
        {'}', 1},

        {keyword_type, 1, type}, 
            {atom, 1, newtype2},
            {'=', 1}, 
                {atom, 1, moo}, 
                    {'|', 1}, 
                {atom, 1, meh}, 
             {';', 1}

    ],

    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [
            {typedef, newtype1, {cons, foo, {cons, bar, nil}}},
            {block, block1, [
                    {declaration, string, foo, ?ERLCFG_SCHEMA_NIL},
                    {declaration, int, bar, ?ERLCFG_SCHEMA_NIL},
                    {block, block1, [
                        {declaration, string, foo, ?ERLCFG_SCHEMA_NIL},
                        {declaration, int, bar, ?ERLCFG_SCHEMA_NIL}
                    ]}
             ]},
            {typedef, newtype2, {cons, moo, {cons, meh, nil}}}
    ]},
    ?assertEqual(Expected, Result).

parse_list_of_int_declaration_test() ->
    Tokens = [{'[', 1}, {datatype, 1, string}, {']', 1}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, {listof, string}, foo, ?ERLCFG_SCHEMA_NIL}]},
    ?assertEqual(Expected, Result).


parse_list_of_int_declaration_with_default_test() ->
    Tokens = [{'[', 1}, {datatype, 1, string}, {']', 1}, {atom, 1, foo}, {'=',
            1}, {'(', 1}, {string, 1, <<"2">>}, {',', 1}, {string, 1, <<"3">>}, {')', 1}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, {listof, string}, foo, {cons, <<"2">>, {cons, <<"3">>, nil}}}]},
    ?assertEqual(Expected, Result).
