# EReturn
The main idea behind this simple API is providing safe function result.

# Usage
Every Function should yields a value with type `return:result/0`:  
```erlang

-type result() :: ok() | ok_result() | error().
-type  ok() :: 'ok'.
-type  ok_result() :: {'ok', term()}.
-type  error() :: {'error', {reason(), error_parameters()}}.
-type   reason() :: atom().
-type   error_parameters() :: [] | [error_parameter()].
-type    error_parameter() :: {atom(), term()}.

```

# Example
Suppose i have a module named `api.erl`:  
```erlang
-module(api).
-export([func_1/0
        ,func_2/0
        ,func_3/0
        ,func_4/1
        ,func_5/1]).

%% Include return's header file and use its macros:
-include("/path/to/return/include/return.hrl").

-spec func_1() -> return:ok().
func_1() ->
	...
	?ok. %% means 'ok'

-spec func_2() -> return:ok_result().
func_2() ->
	...
	?ok(foo). %% {'ok', foo}

-spec func_3() -> return:ok() | return:error().
func_3() ->
	case ... of
		true ->
			?ok;
		false ->
			?err(reason_of_error) %% {'error', {reason_of_error, []}}
	end.

-spec func_4() -> return:ok_result() | return:error().
func_4(Arg) ->
	case ... of
		Value when ... ->
			?ok(Value); %% {'ok', Value}
		_ ->
			?err(reason_of_error, [{argument, Arg}]) %% {'error', {reason_of_error, [{argument, Arg}]}}
	end.

-spec func_5() -> return:result().
func_5(Arg) ->
	try ... of
		Value when ... ->
			?ok(Value);
		_ ->
			?ok
	catch
		_:Reason ->
			?err(crash, [{reason, Reason}, ?stacktrace, ?module, ?function, {arguments, [Arg]}]) 
			%% {error, {crash, [{reason, Reason}
		    %%                 ,{stacktrace, erlang:get_stacktrace()}
			%%                 ,{module, ?MODULE}
			%%                 ,{function, ?FUNCTION_NAME}
			%%                 ,{arguments, [Arg]}]}}
	end.
```

Note that `reason()` **must** be `atom()` and `error_parameters()` **must** be a proplist, otherwise it exits with reason like `{parameter_type, ...}`.

#### Hex
[**`18.6.5`**](https://hex.pm/packages/ereturn)
