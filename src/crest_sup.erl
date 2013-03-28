%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2011-2012 Kevin A. Smith All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(crest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Ip} = application:get_env(crest, ip),
    {ok, Port} = application:get_env(crest, port),
    {ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)),
                                                 "..", "priv", "dispatch.conf"])),
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],

    Children = case application:get_env(crest, active) of
                   true ->
                       [child_spec(webmachine_mochiweb, [WebConfig], web),
                        child_spec(crest_channels, [], supervisor),
                        child_spec(crest_barriers, [], supervisor)];
                   _ ->
                       []
                end,

    {ok, {{one_for_one, 5, 10}, Children}}.


%% Internal functions
child_spec(Module, Args, supervisor) ->
    {Module, {Module, start_link, Args},
     permanent, infinity, supervisor, [Module]};
%% child_spec(Module, Args, worker) ->
%%     {Module, {Module, start_link, Args},
%%      permanent, 5000, worker, [Module]};
child_spec(Module, Args, web) ->
    {Module, {Module, start, Args},
     permanent, 5000, worker, [Module]}.
