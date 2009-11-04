-record(block, {
        name,
        children}).

-record(set, {
        key,
        value}).

-record(get, {
        address}).

-record(list, {
        data}).

-record(cons, {
        head,
        tail=nil}).

-record(interp, {
        node,
        value=nil,
        schema_table=nil}).
        

-record(directive, {
        name,
        value}).
