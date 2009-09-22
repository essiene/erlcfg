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
        scope=''}).
