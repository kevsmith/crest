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

-module(crest_barrier_resource).

-export([init/1,
         resource_exists/2,
         malformed_request/2,
         allowed_methods/2,
         content_types_provided/2,
         allow_missing_post/2,
         process_post/2,
         delete_resource/2]).

-export([to_json/2]).

-record(state, {barrierref,
                pbody}).

-define(DEFAULT_TIMEOUT, 10000).

-include("crest.hrl").
-include_lib("webmachine/include/webmachine.hrl").


init(_) ->
    {{trace, "/tmp"}, #state{}}.

allowed_methods(Req, State) ->
    {['POST', 'GET', 'DELETE'], Req, State}.

resource_exists(Req, State) ->
    BarrierRef = list_to_binary(wrq:path_info(barrierref, Req)),
    {crest_barriers:exists(BarrierRef), Req, State#state{barrierref=BarrierRef}}.

malformed_request(Req, State) ->
    malformed_request(wrq:method(Req), Req, State).

malformed_request('GET', Req, State) ->
    {false, Req, State};
malformed_request('POST', Req, State) ->
    Body = wrq:req_body(Req),
    case jiffy:decode(Body) of
        {error, Reason} ->
            lager:error("Error decoding JSON: ~p  ~p~n", [Body, Reason]),
            {true, Req, State};
        {DecodedBody} ->
            case lists:keytake(<<"count">>, 1, DecodedBody) of
                false ->
                    {true, Req, State};
                {value, _, _} ->
                    {false, Req, State#state{pbody=DecodedBody}}
            end
    end.

allow_missing_post(Req, State) ->
    {true, Req, State}.

process_post(Req, #state{barrierref=BarrierRef, pbody=Body}=State) ->
    {value, {<<"count">>, Count}, _} = lists:keytake(<<"count">>, 1, Body),
    Recycle = case lists:keytake(<<"recycle">>, 1, Body) of
                  false ->
                      false;
                  {value, {<<"recycle">>, V}, _} when V == true;
                                                      V == false ->
                      V
              end,
    case crest_barriers:get(BarrierRef, Count, Recycle) of
        created ->
            {true, Req, State};
        exists ->
            {{halt, 409}, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

delete_resource(Req, State) ->
    {true, Req, State}.

to_json(Req, #state{barrierref=BarrierRef}=State) ->
    Timeout = read_timeout(Req),
    case catch crest_barrier:await(BarrierRef, Timeout) of
        ok ->
            {{halt, 204}, Req, State};
        {error, broken} ->
            {{halt, 408}, Req, State}
    end.

read_timeout(Req) ->
    case wrq:get_qs_value("t", Req) of
        undefined ->
            ?DEFAULT_TIMEOUT;
        V ->
            list_to_integer(V)
    end.
