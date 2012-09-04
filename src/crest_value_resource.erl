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
                expects,
                action}).

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

malformed_request(Method, Req, State) ->
    case set_expectations(Req, State) of
        {true, State1} ->
            case validate_body(Method, Req) of
                error ->
                    {true, Req, State1};
                WriteAction ->
                    {false, Req, State1#state{action=WriteAction}}
            end;
        {false, State1} ->
            {true, Req, State1}
    end.

allow_missing_post(Req, State) ->
    {true, Req, State}.

process_post(Req, #state{valueref=ValueRef, action={write, Value}, expects=Expects}=State) ->
    case wrq:get_req_header("Accept", Req) of
        "application/json" ->
            case Expects() of
                true ->
                    case crest_values:get(ValueRef, Value) of
                        created ->
                            {true, Req, State};
                        exists ->
                            {{halt, 409}, Req, State}
                    end;
                {false, FailedVar, Current, Op, Proposed} ->
                    failed_precondition(FailedVar, Current, Op, Proposed, Req, State)
            end;
        _ ->
            {{halt, 406}, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

from_json(Req, #state{valueref=ValueRef, action=Action, expects=Expects}=State) ->
    case Expects() of
        true ->
            do_write(ValueRef, Action, Req, State);
        {false, Name, Current, Op, Proposed} ->
            failed_precondition(Name, Current, Op, Proposed, Req, State)
    end.

do_write(ValueRef, Write, Req, State) ->
    case crest_value:write(ValueRef, Write) of
        {ok, {Old, New}} ->
            Resp = jiffy:encode({[{<<"old">>, Old}, {<<"new">>, New}]}),
            Req1 = wrq:set_resp_body(Resp, Req),
            {true, Req1, State};
        {error, precondition_failed, {Proposed, Current}} ->
            failed_precondition(ValueRef, Current, '==', Proposed, Req, State)
    end.

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
                    translate_write(DecodedBody)
            end
    end;
validate_body(_Action, Req) ->
    wrq:req_body(Req).

translate_write(Body) ->
    Action = translate_action(lists:keytake(<<"action">>, 1, Body)),
    {value, {_, Value}, _} = lists:keytake(<<"value">>, 1, Body),
    {Action, Value}.


translate_action(false) ->
    write;
translate_action({value, {_, <<"incr">>}, _}) ->
    incr;
translate_action({value, {_, <<"decr">>}, _}) ->
    decr.

failed_precondition(Name, Current, Op, ProposedCurrent, Req, State) ->
    Resp = jiffy:encode({[{<<"name">>, Name}, {<<"op">>, translate_op(Op)}, {<<"current">>, Current},
                          {<<"proposed_current">>, ProposedCurrent}]}),
    Req1 = wrq:set_resp_body(Resp, Req),
    {{halt, 412}, Req1, State}.

set_expectations(Req, State) ->
    case wrq:get_req_header(?EXPECTS_HEADER, Req) of
        undefined ->
            {true, State#state{expects=fun always_true/0}};
        Header ->
            case crest_parser:string(Header) of
                {ok, Expects} ->
                    {true, State#state{expects=Expects}};
                _Error ->
                    {false, State}
            end
    end.

always_true() ->
    true.

translate_op(Op) ->
    list_to_binary(atom_to_list(Op)).

