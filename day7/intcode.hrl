-define(DEBUG(X, Y), ok).
%% -define(DEBUG(X, Y), io:format(X, Y)).

-record(state, {
                memory,
                base,
                operators,
                input,
                output,
                ip
               }).
