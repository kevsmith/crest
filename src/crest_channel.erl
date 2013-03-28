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

-module(crest_channel).

-behaviour(gen_server).

-include("crest.hrl").

%% API
-export([start_link/2,
         read/2,
         write/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name,
                store,
                duration,
                tref}).

start_link(Name, Duration) ->
    gen_server:start_link([Name, Duration], []).

read(ChannelRef, Actor) ->
    call(ChannelRef, {read, Actor}).

write(ChannelRef, Msg) ->
    call(ChannelRef, {write, Msg}).

init([Name, Duration]) ->
    case catch gproc:add_local_name(?CREST_CHANNEL(Name)) of
        true ->
            {ok, Tref} = timer:send_after(Duration, teardown),
            {ok, Store} = crest_channel_file:open(Name),
            {ok, #state{name=Name, duration=Duration, tref=Tref, store=Store}};
        {'EXIT', {badarg, _}} ->
            {error, already_allocated}
    end.

handle_call({read, Actor}, _From, #state{store=Store}=State) ->
    Reply = crest_channel_file:read(Store, Actor),
    {reply, Reply, State};
handle_call({write, Msg}, _From, #state{store=Store}=State) ->
    Reply = crest_channel_file:write(Store, Msg),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
call(ChannelRef, Msg) when is_binary(ChannelRef) ->
    case gproc:lookup_local_name(?CREST_CHANNEL(ChannelRef)) of
        undefined ->
            exit({{not_found, ChannelRef}, [{?MODULE, call, [ChannelRef, Msg]}]});

        Pid ->
            call(Pid, Msg, infinity)
    end.

call(Channel, Msg, Timeout) when is_pid(Channel) ->
    gen_server:call(Channel, Msg, Timeout).
