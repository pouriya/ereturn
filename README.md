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

-type boolean_result() :: boolean_true() | boolean_false().
-type  boolean_true() :: 'true'.
-type  boolean_false() :: 'false'.
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

-spec func_1() -> return:ok().
func_1() ->
	...
	ret:ok(). %% means 'ok'

-spec func_2() -> return:ok_result().
func_2() ->
	...
	ret:ok(foo). %% {'ok', foo}

-spec func_3() -> return:ok() | return:error().
func_3() ->
	case ... of
		true ->
			ret:ok();
		false ->
			ret:err(reason_of_error) %% {'error', {reason_of_error, []}}
	end.

-spec func_4(term()) -> return:ok_result() | return:error().
func_4(Arg) ->
	case ... of
		Value when ... ->
			ret:ok(Value); %% {'ok', Value}
		_ ->
			ret:err(reason_of_error, [{argument, Arg}]) 
			%% {'error', {reason_of_error, [{argument, Arg}]}}
	end.

-spec func_5(term()) -> return:result().
func_5(Arg) ->
	try ... of
		Value when ... ->
			ret:ok(Value);
		_ ->
			ret:ok()
	catch
		_:Reason ->
			ret:err(crash, [ret:r(Reason), ret:st(), ret:mfa(?MODULE, ?FUNCTION_NAME, [Arg])]) 
			%% {error, {crash, [{reason, Reason}
			%%                 ,{stacktrace, erlang:get_stacktrace()}
			%%                 ,{module, ?MODULE}
			%%                 ,{function, ?FUNCTION_NAME}
			%%                 ,{arguments, [Arg]}]}}
	end.
```

Note that `reason()` **must** be `atom()` and `error_parameters()` **must** be a proplist, otherwise it exits with reason like `{parameter_type, ...}`.

#### Hex
[**`18.6.14`**](https://hex.pm/packages/ereturn)
