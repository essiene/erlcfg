{application, erlcfg,
 [{description, "Config file format and library for Erlang"},
  {vsn, "0.1"},
  {modules, 
      [
          erlcfg_lexer,
          erlcfg_parser,
          node_addr,
          erlcfg_node,
          erlcfg_ast,
          erlcfg_interp,
          erlcfg_data,
          erlcfg
      ]
  },
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
