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

-module(crest_channel_file).

-define(CURRENT, <<"/current">>).
-define(SUB_CURRENT(Sub), list_to_binary([<<"/">>, Sub, <<"/current">>])).
-define(MSG_ID(Id), list_to_binary([<<"<</messages/">>, integer_to_list(Id)])).

-record(db, {db,
             path}).

-export([open/1,
         close/1,
         unsub/2,
         read/2,
         write/2]).

open(Name) when is_binary(Name) ->
    open(binary_to_list(Name));
open(Name) ->
    Dir = application:get_env(crest, channel_dir, "/tmp"),
    Path = filename:absname(filename:join([Dir, Name])),
    ok = filelib:ensure_dir(Path),
    case eleveldb:open(Path, [{create_if_missing, true},
                              {error_if_exists, true},
                              {compression, true}]) of
        {ok, Db} ->
            init_channel(#db{db=Db, path=Path});
        Error ->
            Error
    end.

unsub(#db{db=Db}, Sub) ->
    eleveldb:delete(Db, ?SUB_CURRENT(Sub)).

read(#db{db=Db}, Sub) ->
    read_messages(Db, Sub).

write(#db{db=Db}, Msg) ->
    write_message(Db, Msg).

close(#db{db=Db, path=Path}) ->
    eleveldb:close(Db),
    os:cmd(io_lib:format("rm -rf ~s", [Path])),
    ok.

%% Internal functions
init_channel(#db{db=Db}=DbRef) ->
    ok = eleveldb:put(Db, ?CURRENT, erlang:term_to_binary(0), []),
    {ok, DbRef}.

get_current_version(Db) ->
    {ok, Current} = eleveldb:get(Db, ?CURRENT, []),
    erlang:binary_to_term(Current).

get_current_version(Db, Sub) ->
    case eleveldb:get(Db, ?SUB_CURRENT(Sub), []) of
        not_found ->
            0;
        {ok, Current} ->
            erlang:binary_to_term(Current)
    end.

set_current_version(Db, V) ->
    ok = eleveldb:put(Db, ?CURRENT, erlang:term_to_binary(V), []).

set_current_version(Db, Sub, V) ->
    ok = eleveldb:put(Db, ?SUB_CURRENT(Sub), erlang:term_to_binary(V), []).

write_message(Db, Msg) ->
    Current1 = get_current_version(Db) + 1,
    MsgId = ?MSG_ID(Current1),
    ok = eleveldb:put(Db, MsgId, erlang:term_to_binary(Msg), []),
    ok = set_current_version(Db, Current1),
    ok.

read_messages(Db, Sub) ->
    SubVersion = get_current_version(Db, Sub),
    case get_current_version(Db) of
        SubVersion ->
            none;
        V when V > SubVersion ->
            Messages = read_range(Db, SubVersion, V),
            set_current_version(Db, Sub, V),
            {ok, Messages}
    end.

read_range(Db, Start, End) ->
    {ok, Iter} = eleveldb:iterator(Db, []),
    {ok, _, _} = eleveldb:iterator_move(Iter, ?MSG_ID(Start)),
    {ok, Values} = read_range1(Iter, End - Start, []),
    ok = eleveldb:iterator_close(Iter),
    {ok, Values}.

read_range1(_Iter, 0, Accum) ->
    {ok, lists:reverse(Accum)};
read_range1(Iter, Count, Accum) ->
    io:format("Iter: ~p~n", [Iter]),
    {ok, _, Msg} = eleveldb:iterator_move(Iter, next),
    read_range1(Iter, Count - 1, [Msg|Accum]).
