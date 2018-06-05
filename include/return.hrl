-define(ok, ok).
-define(ok(Result), {ok, Result}).

-define(err(Reason), return:error(Reason)).
-define(err(Reason, ErrorParams), return:error(Reason, ErrorParams)).

-define(stacktrace, {stacktrace, erlang:get_stacktrace()}).
-define(module, {module, ?MODULE}).
-define(function, {function, ?FUNCTION_NAME}).
-define(line, {line, ?LINE}).