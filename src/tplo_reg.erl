%% Copyright (c) 2016, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(tplo_reg).
-behaviour(gen_server).

%% API
-export([
         start_link/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("kernel/include/file.hrl").
-include_lib("yolf/include/yolf.hrl").
-include("tplo.hrl").

-record(st, {dir, cur}).

%%% API
start_link(Cfg) ->
    ?LOG_WORKER(?MODULE),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Cfg], []).


%%% gen_server callbacks
init([#cfg{dir = Dir}]) ->
    ?LOG_WORKER_INIT(?MODULE),
    SplitFun = fun(X) -> binary:split(X, [<<".">>, <<"-">>], [global]) end,
    lists:filtermap(),
    _All = [{expand(SplitFun(X), X), X} || X <- ycmd:ls_dir(Dir)],
    {ok, #st{dir = Dir}}.


handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================

expand(List, Fn) ->
    case catch expand1(List) of
        {ok, Tuple} -> Tuple;
        _ -> throw({bad_file_name, Fn})
    end.

expand1([A, B, <<"begin">>]) -> expand2({A, B, b});
expand1([A, B])              -> expand2({A, B});
expand1([A, B, <<"end">>])   -> expand2({A, B, e});

expand1([A, <<"d">>, <<"begin">>]) -> expand2({A, d, b});
expand1([A, <<"d">>, B])           -> expand2({A, d, B});
expand1([A, <<"d">>, <<"end">>])   -> expand2({A, d, e});

expand1([A, <<"d">>, B, C, <<"begin">>]) -> expand2({A, d, B, C, b});
expand1([A, <<"d">>, B, C])              -> expand2({A, d, B, C});
expand1([A, <<"d">>, B, C, <<"d">>])     -> expand2({A, d, B, C, d});
expand1([A, <<"d">>, B, C, <<"end">>])   -> expand2({A, d, B, C, e}).

expand2({A, B}) -> {ok, {ex3(A), ex3(B)}};
expand2({A, B, C}) -> {ok, {ex3(A), ex3(B), ex3(C)}};
expand2({A, B, C, D}) -> {ok, {ex3(A), ex3(B), ex3(C), ex3(D)}};
expand2({A, B, C, D, E}) -> {ok, {ex3(A), ex3(B), ex3(C), ex3(D), ex3(E)}}.

ex3(X) when is_atom(X) -> X;
ex3(X) -> binary_to_integer(X).

%%------------------------------------------------------------------------------

