Nonterminals
stmt value.

Terminals
name integer float string comparator.

Rootsymbol stmt.

value -> integer : '$1'.
value -> float   : '$1'.
value -> string  : '$1'.
value -> name    : '$1'.

stmt -> name comparator value: build_evaluator('$1', '$2', '$3').

Erlang code.
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

build_evaluator({name, _, Name}, {comparator, _, Comp}, Value) ->
    ValEval = build_value_evaluator(Value),
    fun() -> erlang:Comp(crest_value:read(Name), ValEval()) end.

build_value_evaluator({float, _, V}) ->
    fun() -> V end;
build_value_evaluator({integer, _, V}) ->
    fun() -> V end;
build_value_evaluator({string, _, V}) ->
    fun() -> V end;
build_value_evaluator({name, _, Name}) ->
    fun() -> crest_value:read(Name) end.
