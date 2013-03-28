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

-module(crest_channels).

-include("crest.hrl").
-include("crest_channel.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0,
         exists/1,
         get/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec exists(crest_entity_name()) -> boolean().
exists(Name) ->
    case gproc:lookup_global_name(?CREST_CHANNEL(Name)) of
        undefined ->
            false;
        Pid when is_pid(Pid) ->
            true
    end.

-spec get(crest_entity_name(), pos_integer()) -> created | exists.
get(Name, Duration) when is_binary(Name),
                         Duration > 0 ->
    case gproc:lookup_global_name(?CREST_CHANNEL(Name)) of
        undefined ->
            case create_channel(Name, Duration) of
                {ok, _Pid} ->
                    created;
                {error, already_allocated} ->
                    ?MODULE:get(Name, Duration)
            end;
        _Pid ->
            exists
    end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 1, 1},
          [{crest_barrier, {crest_channel, start_link, []},
            temporary, brutal_kill, worker, [crest_barrier]}]}}.

create_channel(Name, Duration) ->
    supervisor:start_child({global, ?SERVER}, [Name, Duration]).
