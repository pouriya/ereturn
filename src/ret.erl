%%% ------------------------------------------------------------------------------------------------
%%% "return" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.6.5
%% @doc
%%           Providing safe function result.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(ret).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([ok/0
        ,ok/1
        ,true/0
        ,false/0
        ,err/1
        ,err/2
        ,reason/1
        ,reason/2
        ,m/1
        ,f/1
        ,mf/2
        ,a/1
        ,mfa/3
        ,st/0
        ,r/1
        ,v/1
        ,ep/2]).

%% -------------------------------------------------------------------------------------------------
%% Types:

-opaque result() :: ok() | ok_result() | error().
-opaque  ok() :: 'ok'.
-opaque  ok_result() :: {'ok', term()}.
-opaque  error() :: {'error', {reason(), error_parameters()}}.
-opaque   reason() :: atom().
-opaque   error_parameters() :: [] | [error_parameter()].
-opaque    error_parameter() :: {atom(), term()}.

-opaque boolean_result() :: boolean_true() | boolean_false().
-opaque  boolean_true() :: 'true'.
-opaque  boolean_false() :: 'false'.


-export_type([result/0
             ,ok/0
             ,ok_result/0
             ,error/0
             ,reason/0
             ,error_parameters/0
             ,error_parameter/0
             ,boolean_result/0
             ,boolean_true/0
             ,boolean_false/0]).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
ok() ->
    ok().
ok() ->
    ok.


-spec
ok(term()) ->
    ok_result().
ok(Result) ->
    {ok, Result}.


-spec
true() ->
    boolean_true().
true() ->
    true.


-spec
false() ->
    boolean_false().
false() ->
    false.


-spec
err(reason()) ->
    error().
err(Reason) when erlang:is_atom(Reason) ->
    {error, {Reason, []}};
err(Other) ->
    erlang:error({reason_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).


-spec
err(reason(), error_parameters()) ->
    error().
err(Reason, ErrorParams) when erlang:is_atom(Reason) ->
    _ = check_error_parameters(ErrorParams),
    {error, {Reason, ErrorParams}};
err(Other, _) ->
    erlang:error({reason_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).


-spec
reason(reason()) ->
    {reason(), []}.
reason(Reason) when erlang:is_atom(Reason) ->
    {Reason, []};
reason(Other) ->
    erlang:error({reason_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).


-spec
reason(reason(), error_parameters()) ->
    {reason(), error_parameters()}.
reason(Reason, ErrorParams) when erlang:is_atom(Reason) ->
    _ = check_error_parameters(ErrorParams),
    {Reason, ErrorParams};
reason(Other, _) ->
    erlang:error({reason_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).


-spec
m(module()) ->
    error_parameter().
m(Mod) when erlang:is_atom(Mod) ->
    {module, Mod};
m(Other) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, module}]}).


-spec
f(atom()) ->
    error_parameter().
f(FuncName) when erlang:is_atom(FuncName) ->
    {function, FuncName};
f(Other) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, function}]}).


-spec
mf(module(), atom()) ->
    error_parameters().
mf(Mod, FuncName) when erlang:is_atom(Mod) andalso erlang:is_atom(FuncName) ->
    [{module, Mod}, {function, FuncName}];
mf(Other, FuncName) when erlang:is_atom(FuncName) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, module}]});
mf(Mod, Other) when erlang:is_atom(Mod) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, function}]});
mf(Other1, Other2) ->
    erlang:error({parameter_value_type, [{value, Other1}
                                        ,{value, Other2}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, module}
                                        ,{parameter, function}]}).


-spec
a([] | list()) ->
    error_parameter().
a(Args) when erlang:is_list(Args) ->
    {arguments, Args};
a(Other) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, arguments}]}).


-spec
mfa(module(), atom(), [] | list()) ->
    error_parameters().
mfa(Mod, FuncName, Args) when erlang:is_atom(Mod) andalso
                              erlang:is_atom(FuncName) andalso
                              erlang:is_list(Args) ->
    [{module, Mod}, {function, FuncName}, {arguments, Args}];
mfa(Other, FuncName, Args) when erlang:is_atom(FuncName) andalso erlang:is_list(Args) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, module}]});
mfa(Mod, Other, Args) when erlang:is_atom(Mod) andalso erlang:is_list(Args) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, function}]});
mfa(Mod, FuncName, Other)  when erlang:is_atom(Mod) andalso erlang:is_atom(FuncName) ->
    erlang:error({parameter_value_type, [{value, Other}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, arguments}]});
mfa(Other1, Other2, Other3) ->
    erlang:error({parameter_value_type, [{value, Other1}
                                        ,{value, Other2}
                                        ,{value, Other3}
                                        ,{stacktrace, erlang:get_stacktrace()}
                                        ,{parameter, module}
                                        ,{parameter, function}
                                        ,{parameter, arguments}]}).


-spec
st() ->
    error_parameter().
st() ->
    {stacktrace, erlang:get_stacktrace()}.


-spec
r(term()) ->
    error_parameter().
r(Reason) ->
    {reason, Reason}.


-spec
v(term()) ->
    error_parameter().
v(Value) ->
    {value, Value}.


-spec
ep(atom(), term()) ->
    error_parameter().
ep(Tag, Value) when erlang:is_atom(Tag) ->
    {Tag, Value};
ep(Other, _) ->
    erlang:error({parameter_tag_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).


%% -------------------------------------------------------------------------------------------------
%% Internals:

check_error_parameters([{Param, _}|Rest]) when erlang:is_atom(Param) ->
    check_error_parameters(Rest);
check_error_parameters([]) ->
    ok;
check_error_parameters([{Param, _}|_]) ->
    erlang:error({parameter_tag_type, [{value, Param}, {stacktrace, erlang:get_stacktrace()}]});
check_error_parameters([Other|_]) ->
    erlang:error({parameter, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]});
check_error_parameters(Other) -> % improper list
    erlang:error({parameters_type, [{value, Other}, {stacktrace, erlang:get_stacktrace()}]}).