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

-module(tplo_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("tplo.hrl").

start(_StartType, _StartArgs) ->
    ylog:init(),
    ylog:tin(<<"Triplero: Log started.">>),
    case get_cfg() of
        {error, _} = Err -> Err;
        Cfg -> start_sup(Cfg)
    end.

get_cfg() ->
    try
        setup()
    catch
        throw:Term -> {error, Term};
        error:Term -> {error, Term}
    end.

setup() ->
    {ok, Dir} = application:get_env(triplero, dir),
    Cfg = #cfg{dir = yolf:to_binary(Dir)},
    ylog:in(<<"Read config:">>, endl, Cfg, endl),

    case ycmd:ensure_dir(Dir) of
        ok -> Cfg;
        {error, _} = Err -> bad_triplero_dir(Dir, Err)
    end.

bad_triplero_dir(Dir, Err) ->
    ylog:in(<<"Option 'dir' for 'triplero' specifies directory: '">>, Dir,
            <<"', which doesn't exist and couldn't be created; error: ">>,
            Err, <<". Exiting.">>),
    throw(bad_triplero_dir).

start_sup(Cfg) ->
    case tplo_sup:start_link(Cfg) of
        {ok, Pid} ->
            tplo_event_logger:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ylog:tin(<<"Triplero: Stopping log.">>),
    ylog:stop(),
    ok.
