-module(test_schema_parser).
-include_lib("eunit/include/eunit.hrl").
-include("schema.hrl").

parse_int_declaration_test() ->
    Tokens = [{datatype, 1, int}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, int, foo}]},
    ?assertEqual(Expected, Result).

parse_float_declaration_test() ->
    Tokens = [{datatype, 1, float}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, float, foo}]},
    ?assertEqual(Expected, Result).

parse_bool_declaration_test() ->
    Tokens = [{datatype, 1, bool}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, bool, foo}]},
    ?assertEqual(Expected, Result).

parse_atom_declaration_test() ->
    Tokens = [{datatype, 1, atom}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, atom, foo}]},
    ?assertEqual(Expected, Result).

parse_string_declaration_test() ->
    Tokens = [{datatype, 1, string}, {atom, 1, foo}, {';', 1}],
    Result = erlcfg_schema_parser:parse(Tokens),
    Expected = {ok, [{declaration, string, foo}]},
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
                    {declaration, string, foo},
                    {declaration, int, bar}
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
                    {declaration, string, foo},
                    {declaration, int, bar},
                    {block, block1, [
                        {declaration, string, foo},
                        {declaration, int, bar}
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
                    {declaration, string, foo},
                    {declaration, int, bar},
                    {block, block1, [
                        {declaration, string, foo},
                        {declaration, int, bar}
                    ]}
             ]},
            {typedef, newtype2, {cons, moo, {cons, meh, nil}}}
    ]},
    ?assertEqual(Expected, Result).
