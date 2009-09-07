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
