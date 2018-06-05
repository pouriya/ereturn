-module(return_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include_lib("eunit/include/eunit.hrl").
-include("return.hrl").

%% -------------------------------------------------------------------------------------------------
%% ct callbacks:


all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-8)].


init_per_suite(Config) ->
    application:start(sasl),
    Config.


end_per_suite(Config) ->
    application:stop(sasl),
    Config.


init_per_testcase(_, Config) ->
    erlang:process_flag(trap_exit, true),
    Config.


end_per_testcase(_, _) ->
    ok.

%% -------------------------------------------------------------------------------------------------
%% Test cases:

'1'(_) ->
    ?assertEqual(ok, ?ok),
    ?assertEqual({ok, foo}, ?ok(foo)),

    ?assertEqual({error, {foo, []}}, ?err(foo)),
    ?assertEqual({error, {foo, [{bar, baz}]}}, ?err(foo, [{bar, baz}])),
    ?assertEqual({error, {foo, [{bar, baz}]}}, ?err(foo, [{bar, baz}])),

    Result = ?stacktrace,
    ?assertMatch({stacktrace, _}, ?stacktrace),
    {_, ST} = Result,
    ?assert(erlang:is_list(ST)),

    ?assertExit({reason_type, [{value, 1}, {stacktrace, _}]}, ?err(1)),
    ?assertExit({parameters_type, [{value, 1}, {stacktrace, _}]}, ?err(foo, 1)),
    ?assertExit({parameter, [{value, 1}, {stacktrace, _}]}, ?err(foo, [1])),
    ?assertExit({parameter, [{value, 1}, {stacktrace, _}]}, ?err(foo, [1|1])),
    ?assertExit({parameter_type, [{value, "bar"}, {stacktrace, _}]}, ?err(foo, [{"bar", baz}])),
    ?assertExit({parameter_type, [{value, "bar"}, {stacktrace, _}]}, ?err(foo, [{a, b},{"bar", baz}, {c, d}])).