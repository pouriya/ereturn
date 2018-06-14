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
    ?assertEqual(ok, ret:ok()),
    ?assertEqual({ok, foo}, ret:ok(foo)),

    ?assert(ret:true()),
    ?assertNot(ret:false()),

    ?assertEqual({error, {oops, []}}, ret:err(oops)),
    ?assertEqual({error, {oops, []}}, ret:err(oops, [])),

    ?assertEqual({error, {oops, [{ok, value}]}}, ret:err(oops, [{ok, value}])),
    ?assertEqual({error, {oops, [{ok, "value"}]}}, ret:err(oops, [{ok, "value"}])),

    ?assertEqual({error, {oops, [{foo, bar}, {baz, qux}]}}, ret:err(oops, [{foo, bar}, {baz, qux}])),

    ?assertError({reason_type, _}, ret:err(<<>>)),
    ?assertError({reason_type, _}, ret:err(<<>>, [])),
    ?assertError({parameter, _}, ret:err(atom, [oo])),
    ?assertError({parameters_type, _}, ret:err(atom, 1)),
    ?assertError({parameter_tag_type, _}, ret:err(atom, [{1, 0}])),

    ?assertEqual({foo, []}, ret:reason(foo)),
    ?assertEqual({foo, [{bar, baz}]}, ret:reason(foo, [{bar, baz}])),

    ?assertEqual({module, foo}, ret:m(foo)),
    ?assertEqual({function, foo}, ret:f(foo)),
    ?assertEqual({arguments, [foo]}, ret:a([foo])),
    ?assertEqual({arguments, []}, ret:a([])),

    ?assertEqual([{module, foo}, {function, bar}, {arguments, [baz]}], ret:mfa(foo, bar, [baz])),

    ?assertEqual({value, ""}, ret:v("")),
    ?assertEqual({reason, 1.2}, ret:r(1.2)),
    ?assertEqual({foo, bar}, ret:ep(foo, bar)).













