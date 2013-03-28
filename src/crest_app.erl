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

-module(crest_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         manual_start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

manual_start() ->
    start_deps([crypto,
                harasser,
                lager,
                mochiweb,
                gproc,
                webmachine,
                crest]),
    lager:set_loglevel(lager_console_backend, debug),
    wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp").

start(_StartType, _StartArgs) ->
    crest_sup:start_link().

stop(_State) ->
    ok.

%% Internal functions
start_deps([]) ->
    ok;
start_deps([Dep|T]) ->
    case application:load(Dep) of
        V when V == ok;
               V == {error, {already_loaded, Dep}} ->
            ok;
        V ->
            error(V)
    end,
    case application:get_key(Dep, applications) of
        undefined ->
            ok;
        {ok, Apps} ->
            start_deps(Apps)
    end,
    case application:start(Dep) of
        ok ->
            start_deps(T);
        {error, {already_started, Dep}} ->
            start_deps(T);
        Error ->
            error(Error)
    end.
