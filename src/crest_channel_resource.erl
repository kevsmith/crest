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

-module(crest_channel_resource).

-export([init/1,
         resource_exists/2,
         malformed_request/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         allow_missing_post/2,
         process_post/2,
         delete_resource/2]).

-export([to_json/2,
         from_json/2]).

-record(state, {channelref,
                actor,
                pbody}).

-define(DEFAULT_DURATION, 300000).

-include("crest.hrl").
-include_lib("webmachine/include/webmachine.hrl").


init(_) ->
    {{trace, "/tmp"}, #state{}}.

allowed_methods(Req, State) ->
    {['POST', 'PUT', 'GET', 'DELETE'], Req, State}.

resource_exists(Req, State) ->
    ChannelRef = list_to_binary(wrq:path_info(channelref, Req)),
    {crest_channels:exists(ChannelRef), Req, State#state{channelref=ChannelRef}}.

malformed_request(Req, State) ->
    malformed_request(wrq:method(Req), Req, State).

malformed_request('GET', Req, State) ->
    case wrq:path_info(actor, Req) of
        undefined ->
            {true, Req, State};
        Actor ->
            {false, Req, State#state{actor=list_to_binary(Actor)}}
    end;
malformed_request('PUT', Req, State) ->
    Body = wrq:req_body(Req),
    case jiffy:decode(Body) of
        {error, Reason} ->
            lager:error("Error decoding JSON: ~p  ~p~n", [Body, Reason]),
            {true, Req, State};
        {DecodedBody} ->
            case proplists:get_value(<<"message">>, DecodedBody) of
                undefined ->
                    {true, Req, State};
                _ ->
                    {false, Req, State#state{pbody=DecodedBody}}
            end
    end;
malformed_request('POST', Req, State) ->
    Body = wrq:req_body(Req),
    case jiffy:decode(Body) of
        {error, Reason} ->
            lager:error("Error decoding JSON: ~p  ~p~n", [Body, Reason]),
            {true, Req, State};
        {DecodedBody} ->
            {false, Req, State#state{pbody=DecodedBody}}
    end.

allow_missing_post(Req, State) ->
    {true, Req, State}.

process_post(Req, #state{channelref=ChannelRef, pbody=Body}=State) ->
    Duration = proplists:get_value(<<"duration">>, Body, ?DEFAULT_DURATION),
    case crest_channels:get(ChannelRef, Duration) of
        created ->
            {true, Req, State};
        exists ->
            {{halt, 409}, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

delete_resource(Req, State) ->
    {true, Req, State}.

to_json(Req, #state{channelref=ChannelRef, actor=Actor}=State) ->
    case crest_channel:read(ChannelRef, Actor) of
        none ->
            {jiffy:encode([{<<"count">>, 0}]), Req, State};
        {ok, Messages} ->
            {jiffy:encode([{<<"count">>, length(Messages)},
                           {<<"messages">>, Messages}]), Req, State}
    end.

from_json(Req, #state{channelref=ChannelRef, pbody=Body}=State) ->
    Message = proplists:get_value(<<"message">>, Body),
    crest_channel:write(ChannelRef, Message),
    {true, Req, State}.
