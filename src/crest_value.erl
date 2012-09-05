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

-module(crest_value).

-behaviour(gen_server).

-include("crest.hrl").

-type crest_value_msg() :: {lock, pid()} | {unlock, pid()} | {write, pid(), write_term()} |
                           destroy | read | value_type.

%% API
-export([start_link/2,
         destroy/1]).

%% Lock management
-export([lock/1,
         lock/2,
         unlock/1,
         unlock/2]).

%% Read/write
-export([read/1,
         write/2,
         type/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {value :: crest_value(),
                name  :: crest_entity_name(),
                lock_monitor :: reference() | undefined,
                lock_owner :: pid() | undefined}).

-record('DOWN', {mref :: reference(),
                 type :: atom(),
                 object :: pid(),
                 info :: term()}).

-spec start_link(crest_entity_name(), StartingValue::crest_value()) -> {ok, pid()} | {error, term()}.
start_link(Name, StartingValue) when is_binary(StartingValue);
                                     is_number(StartingValue) ->
    gen_server:start_link(?MODULE, [Name, StartingValue], []).

-spec destroy(ValueRef::crest_entity_ref()) -> ok.
destroy(ValueRef) ->
    call(ValueRef, destroy).

-spec lock(ValueRef::crest_entity_ref()) -> boolean().
lock(ValueRef) ->
    lock(ValueRef, self()).

-spec lock(ValueRef::crest_entity_ref(), Locker::pid()) -> boolean().
lock(ValueRef, Locker) ->
    call(ValueRef, {lock, Locker}).

-spec unlock(ValueRef::crest_entity_ref()) -> boolean().
unlock(ValueRef) ->
    unlock(ValueRef, self()).

-spec unlock(ValueRef::crest_entity_ref(), Unlocker::pid()) -> boolean().
unlock(ValueRef, Unlocker) ->
    call(ValueRef, {unlock, Unlocker}).

-spec read(ValueRef::crest_entity_ref()) -> crest_value().
read(ValueRef) ->
    call(ValueRef, read).

-spec write(ValueRef::crest_entity_ref(), write_term()) -> {ok, {OldValue::crest_value(),
                                                                 NewValue :: crest_value()}} | write_error() |
                                                           no_return().
write(ValueRef, Value) ->
    validate_write_term(Value),
    call(ValueRef, {write, self(), Value}).

-spec type(ValueRef::crest_entity_ref()) -> crest_value_type().
type(ValueRef) ->
    call(ValueRef, value_type).

init([Name, StartingValue]) ->
    case catch gproc:add_local_name(?CREST_VALUE(Name)) of
        true ->
            {ok, #state{name=Name, value=StartingValue}};
        {'EXIT', {badarg, _}} ->
            {error, already_allocated}
    end.

handle_call({lock, Locker}, _From, #state{lock_owner=undefined}=State) ->
    {reply, true, State#state{lock_owner=Locker,
                              lock_monitor=erlang:monitor(process, Locker)}};
handle_call({lock, Locker}, _From, #state{lock_owner=Locker}=State) ->
    {reply, true, State};
handle_call({unlock, Unlocker}, _From, #state{lock_owner=Unlocker, lock_monitor=MRef}=State) ->
    erlang:demonitor(MRef, [flush]),
    {reply, true, State#state{lock_owner=undefined, lock_monitor=undefined}};
handle_call({unlock, _Unlocker}, _From, State) ->
    {reply, false, State};
handle_call({lock, _Locker}, _From, State) ->
    {reply, false, State};
handle_call(read, _From, #state{value=Value}=State) ->
    {reply, Value, State};
handle_call({write, Writer, Write}, _From, #state{lock_owner=Owner,
                                                  value=OldValue}=State) when Owner == Writer;
                                                                              Owner == undefined ->
    {Reply, State2} = case perform_write(Write, State) of
                          {ok, NewValue, State1} ->
                              {{ok, {OldValue, NewValue}}, State1};
                          Error ->
                              {Error, State}
                      end,
    {reply, Reply, State2};
handle_call({write, _Writer, _Write}, _From, State) ->
    {reply, {error, locked}, State};
handle_call(value_type, _From, #state{value=Value}=State) ->
    {reply, type_of(Value), State};
handle_call(destroy, _From, #state{lock_owner=undefined}=State) ->
    {stop, normal, ok, State};
handle_call(destroy, _From, State) ->
    {reply, {error, locked}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{object=Crasher}, #state{lock_owner=Crasher}=State) ->
    {noreply, State#state{lock_owner=undefined,
                          lock_monitor=undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("~p terminating: ~p~n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

-spec perform_write(write_term(), #state{}) -> {ok, NewValue::crest_value(), #state{}} |
                                               {type_error(), #state{}}.
perform_write({write, NewValue}, State) ->
    {ok, NewValue, State#state{value=NewValue}};
perform_write({write, NewValue, Precondition}, #state{name=Name, value=Value}=State) ->
    case Precondition(Name, Value) of
        true ->
            {ok, NewValue, State#state{value=NewValue}};
        Error ->
            Error
    end;
perform_write({incr, Amt}, #state{value=Value}=State) when is_number(Value),
                                                           is_number(Amt) ->
    NewValue = Value + Amt,
    {ok, NewValue, State#state{value=NewValue}};
perform_write({incr, Amt, Precondition}, #state{name=Name, value=Value}=State) when is_number(Value),
                                                                                    is_number(Amt) ->
    case Precondition(Name, Value) of
        true ->
            NewValue = Value + Amt,
            {ok, NewValue, State#state{value=NewValue}};
        Error ->
            Error
    end;
perform_write({decr, Amt}, #state{value=Value}=State) when is_number(Value),
                                                           is_number(Amt) ->
    NewValue = Value - Amt,
    {ok, NewValue, State#state{value=NewValue}};
perform_write({decr, Amt, Precondition}, #state{name=Name, value=Value}=State) when is_number(Value),
                                                                                    is_number(Amt) ->
    case Precondition(Name, Value) of
        true ->
            NewValue = Value - Amt,
            {ok, NewValue, State#state{value=NewValue}};
        Error ->
            Error
    end;
perform_write({Op, _Amt}, State) when Op == incr;
                                      Op == decr ->
    {{error, wrong_type}, State}.

validate_write_term({write, Value}) when is_binary(Value);
                                         is_number(Value) ->
    ok;
validate_write_term({write, Value, Precondition}) when (is_binary(Value) orelse
                                                       is_number(Value)) andalso
                                                       is_function(Precondition) ->
    ok;
validate_write_term({decr, Amt}) when is_number(Amt) ->
    ok;
validate_write_term({decr, Amt, Precondition}) when is_number(Amt),
                                                    is_function(Precondition) ->
    ok;

validate_write_term({incr, Amt}) when is_number(Amt) ->
    ok;
validate_write_term({incr, Amt, Precondition}) when is_number(Amt),
                                                    is_function(Precondition) ->
    ok;
validate_write_term(Term) ->
    lager:error("Bad write term: ~p~n", [Term]),
    error(badarg).

-spec type_of(crest_value()) -> crest_value_type().
type_of(Value) when is_float(Value) ->
    float;
type_of(Value) when is_integer(Value) ->
    integer;
type_of(Value) when is_binary(Value) ->
    binary.

-spec call(ValueRef::crest_entity_ref(), Msg::crest_value_msg()) -> term() | no_return().
call(ValueRef, Msg) when is_binary(ValueRef) ->
    case gproc:lookup_local_name(?CREST_VALUE(ValueRef)) of
        undefined ->
            exit({{not_found, ValueRef}, [{?MODULE, call, [ValueRef, Msg]}]});

        Pid ->
            gen_server:call(Pid, Msg)
    end;
call(ValueRef, Msg) when is_pid(ValueRef) ->
    gen_server:call(ValueRef, Msg).
