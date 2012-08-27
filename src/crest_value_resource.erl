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

-module(crest_value_resource).

-export([init/1,
         resource_exists/2,
         malformed_request/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         allow_missing_post/2,
         process_post/2,
         delete_resource/2]).

-export([to_json/2,
         from_json/2]).

-record(state, {valueref,
                pbody}).

-include("crest.hrl").
-include("crest_value.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {{trace, "/tmp"}, #state{}}.

allowed_methods(Req, State) ->
    {['PUT', 'GET', 'POST', 'DELETE'], Req, State}.

resource_exists(Req, State) ->
    ValueRef = list_to_binary(wrq:path_info(valueref, Req)),
    {crest_values:exists(ValueRef), Req, State#state{valueref=ValueRef}}.

malformed_request(Req, State) ->
    malformed_request(wrq:method(Req), Req, State).

malformed_request(Action, Req, State) ->
    case validate_body(Action, Req) of
        error ->
            {true, Req, State};
        Body ->
            {false, Req, State#state{pbody=Body}}
    end.

allow_missing_post(Req, State) ->
    {true, Req, State}.

process_post(Req, #state{valueref=ValueRef, pbody=PBody}=State) ->
    case wrq:get_req_header("Accept", Req) of
        "application/json" ->
            {value, {_, Value}, _} = lists:keytake(<<"value">>, 1, PBody),
            case crest_values:get(ValueRef, Value) of
                created ->
                    {true, Req, State};
                exists ->
                    {{halt, 409}, Req, State}
            end;
        _ ->
            {{halt, 406}, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

from_json(Req, #state{valueref=ValueRef, pbody=Body}=State) ->
    case translate_write(ValueRef, Body) of
        {Conditions, Write} when is_tuple(Write) ->
            case check_conditions(Conditions) of
                true ->
                    do_write(ValueRef, Write, Req, State);
                {false, Name, {Current, ProposedCurent}} ->
                    failed_precondition(Name, Current, ProposedCurent, Req, State)
            end;
        Write ->
            do_write(ValueRef, Write, Req, State)
    end.

-spec check_conditions([] | [tuple()]) -> true | {false, crest_entity_name(), {crest_value(), crest_value()}}.
check_conditions([]) ->
    true;
check_conditions([Condition|T]) ->
    {value, {_, Name}, _} = lists:keytake(<<"name">>, 1, Condition),
    {value, {_, Proposed}, _} = lists:keytake(<<"value">>, 1, Condition),
    case crest_value:read(Name) of
        Proposed ->
            check_conditions(T);
        Value ->
            {false, Name, {Value, Proposed}}
    end.

do_write(ValueRef, Write, Req, State) ->
    case crest_value:write(ValueRef, Write) of
        {ok, {Old, New}} ->
            Resp = jiffy:encode({[{<<"old">>, Old}, {<<"new">>, New}]}),
            Req1 = wrq:set_resp_body(Resp, Req),
            {true, Req1, State};
        {error, precondition_failed, {Proposed, Current}} ->
            failed_precondition(ValueRef, Current, Proposed, Req, State)
    end.

failed_precondition(Name, Current, ProposedCurrent, Req, State) ->
    Resp = jiffy:encode({[{<<"name">>, Name}, {<<"current">>, Current}, {<<"proposed_current">>, ProposedCurrent}]}),
    Req1 = wrq:set_resp_body(Resp, Req),
    {{halt, 412}, Req1, State}.

to_json(Req, #state{valueref=ValueRef}=State) ->
    Value = crest_value:read(ValueRef),
    Response = jiffy:encode({[{<<"value">>, Value}]}),
    {Response, Req, State}.

delete_resource(Req, #state{valueref=ValueRef}=State) ->
    ok = crest_value:destroy(ValueRef),
    {true, Req, State}.

validate_body(Action, Req) when Action == 'POST';
                                Action == 'PUT' ->
    case wrq:req_body(Req) of
        "" ->
            error;
        Body ->
            case catch jiffy:decode(Body) of
                {error, Reason} ->
                    lager:error("Error decoding JSON: ~p  ~p~n", [Body, Reason]),
                    error;
                {DecodedBody} ->
                    case lists:keytake(<<"value">>, 1, DecodedBody) of
                        false ->
                            error;
                        _ ->
                            DecodedBody
                    end
            end
    end;
validate_body(_Action, Req) ->
    wrq:req_body(Req).

translate_write(ValueRef, Body) ->
    Action = translate_action(lists:keytake(<<"action">>, 1, Body)),
    {value, {_, Value}, _} = lists:keytake(<<"value">>, 1, Body),
    case lists:keytake(<<"preconditions">>, 1, Body) of
        false ->
            {Action, Value};
        {value, {_, Conditions0}, _} ->
            Conditions = [C || {C} <- Conditions0],
            process_self_reference(ValueRef, Action, Value, Conditions, [])
    end.

process_self_reference(_Name, Action, Value, [], Accum) ->
    {lists:reverse(Accum), {Action, Value}};
process_self_reference(Name, Action, Value, [Condition|T], Accum) ->
    case lists:keytake(<<"value">>, 1, Condition) of
        false ->
            process_self_reference(Name, Action, Value, T, [Condition|Accum]);
        {value, {_, OldValue}, _} ->
            {Accum ++ T, {Action, OldValue, Value}}
    end.

translate_action(false) ->
    write;
translate_action({value, {_, <<"incr">>}, _}) ->
    incr;
translate_action({value, {_, <<"decr">>}, _}) ->
    decr.
