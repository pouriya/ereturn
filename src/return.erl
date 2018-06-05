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
-module(return).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([error/1
        ,error/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("return.hrl").

%% -------------------------------------------------------------------------------------------------
%% Types:

-type result() :: ok() | ok_result() | error().
-type  ok() :: 'ok'.
-type  ok_result() :: {'ok', term()}.
-type  error() :: {'error', {reason(), error_parameters()}}.
-type   reason() :: atom().
-type   error_parameters() :: [] | [error_parameter()].
-type    error_parameter() :: {atom(), term()}.

-export_type([result/0
             ,ok/0
             ,ok_result/0
             ,error/0
             ,reason/0
             ,error_parameters/0
             ,error_parameter/0]).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
error(reason()) ->
    error().
error(Reason) when erlang:is_atom(Reason) ->
    {error, {Reason, []}};
error(Reason) ->
    erlang:exit({reason_type, [{value, Reason}, ?stacktrace]}).


-spec
error(reason(), error_parameters()) ->
    error().
error(Reason, ErrorParams) when erlang:is_atom(Reason) andalso erlang:is_list(ErrorParams) ->
    case check_error_parameters(ErrorParams) of
        ok ->
            {error, {Reason, ErrorParams}};
        Err ->
            erlang:exit(erlang:element(2, Err))
    end;
error(Reason, ErrorParams) when erlang:is_atom(Reason) ->
    erlang:exit({parameters_type, [{value, ErrorParams}, ?stacktrace]});
error(Reason, _) ->
    erlang:exit({reason_type, [{value, Reason}, ?stacktrace]}).

%% -------------------------------------------------------------------------------------------------
%% Internals:

check_error_parameters([{Param, _}|Rest]) when erlang:is_atom(Param) ->
    check_error_parameters(Rest);
check_error_parameters([]) ->
    ok;
check_error_parameters([{Param, _}|_]) ->
    {error, {parameter_type, [{value, Param}, ?stacktrace]}};
check_error_parameters([Other|_]) ->
    {error, {parameter, [{value, Other}, ?stacktrace]}};
check_error_parameters(Other) -> % improper list
    {error, {parameters_type, [{value, Other}, ?stacktrace]}}.