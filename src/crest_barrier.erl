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

-module(crest_barrier).

-behaviour(gen_server).

-include("crest.hrl").
-include("crest_barrier.hrl").

-record('DOWN', {mref :: reference(),
                 type :: atom(),
                 object :: pid(),
                 info :: term()}).

%% API
-export([start_link/3]).

-export([await/2,
         destroy/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {count,
                clients,
                recycle}).

-spec start_link(binary(), pos_integer(), boolean()) -> {ok, pid()} | {error, term()}.
start_link(Name, Count, Recycle) ->
    gen_server:start_link(?MODULE, [Name, Count, Recycle], []).

-spec await(crest_entity_ref(), timeout()) -> no_return().
await(BarrierRef, Timeout) ->
    case catch call(BarrierRef, {await, self()}, Timeout) of
        {'EXIT', _} ->
            {error, broken};
        Result ->
            Result
    end.

destroy(BarrierRef) ->
    call(BarrierRef, destroy, 5000).

init([Name, Count, Recycle]) ->
    case catch gproc:add_global_name(?CREST_BARRIER(Name)) of
        true ->
            {ok, #state{count=Count, clients=[], recycle=Recycle}};
        {'EXIT', {badarg, _}} ->
            {error, already_allocated}
    end.

handle_call({await, Caller}, From, #state{clients=Clients}=State) ->
    case lists:keyfind(Caller, 1, Clients) of
        false ->
            maybe_release(Caller, From, State);
        _ ->
            {noreply, State}
    end;
handle_call(destroy, _From, #state{clients=Clients}=State) ->
    [break_client(Client) || Client <- Clients],
    {stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{object=Pid}, #state{clients=Clients, recycle=Recycle,
                                        count=Count}=State) ->
    case lists:keytake(Pid, 1, Clients) of
        false ->
            {noreply, State};
        {value, _, Clients1} ->
            [break_client(Client) || Client <- Clients1],
            if
                Recycle == true ->
                    {noreply, State#state{clients=[], count=length(Clients) + Count}};
                true ->
                    {stop, normal, State}
            end
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("~p terminating: ~p~n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
-spec call(BarrierRef::crest_entity_ref(), Msg::crest_barrier_msg(), Timeout::pos_integer()) -> term() | no_return().
call(BarrierRef, Msg, Timeout) when is_binary(BarrierRef) ->
    case gproc:lookup_global_name(?CREST_BARRIER(BarrierRef)) of
        undefined ->
            exit({{not_found, BarrierRef}, [{?MODULE, call, [BarrierRef, Msg]}]});

        Pid ->
            gen_server:call(Pid, Msg, Timeout)
    end;
call(BarrierRef, Msg, Timeout) when is_pid(BarrierRef) ->
    gen_server:call(BarrierRef, Msg, Timeout).

maybe_release(_Caller, _From, #state{clients=Clients, count=Count,
                     recycle=Recycle}=State) when Count == 1 ->
    [release_client(Client) || Client <- Clients],
    if
        Recycle == true ->
            {reply, ok, State#state{clients=[], count=length(Clients) + 1}};
        true ->
            {stop, normal, ok, State}
    end;
maybe_release(Caller, From, #state{clients=Clients, count=Count}=State) ->
    MRef = erlang:monitor(process, Caller),
    {noreply, State#state{clients=[{Caller, From, MRef}|Clients], count=Count - 1}}.

release_client({_Pid, From, MRef}) ->
    erlang:demonitor(MRef, [flush]),
    gen_server:reply(From, ok).

break_client({_, From, MRef}) ->
    gen_server:reply(From, {error, broken}),
    erlang:demonitor(MRef, [flush]).
