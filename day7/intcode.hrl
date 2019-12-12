-define(DEBUG(X, Y), ok).
%% -define(DEBUG(X, Y), io:format(X, Y)).

-record(state, {
                memory,
                base,
                operators,
                input,
                output
               }).

%% All arrays (`contents`) will hold 1024 values.
-record(mem, {
              start,
              contents
             }).
