-record(block, {
        name,
        child,
        next=nil}).

-record(set, {
        key,
        value,
        next=nil}).

-record(get, {
        address}).

-record(cons, {
        head,
        tail=nil}).

-record(interp, {
        node,
        value=nil,
        scope=''}).
