Nonterminals
stmts stmt value.

Terminals
name integer float string comparator eos.

Rootsymbol stmts.

value -> integer : '$1'.
value -> float   : '$1'.
value -> string  : '$1'.
value -> name    : '$1'.

stmts -> stmts eos stmt: '$3' ++ '$1'.
stmts -> stmt: '$1'.
stmt -> name comparator value: build_evaluator('$1', '$2', '$3').
stmt -> name: build_evaluator('$1').

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

-export([string/1]).

string(Text) ->
    {ok, Tokens, _} = crest_lexer:string(Text),
    case parse(Tokens) of
        {error, {Line, crest_parser, Error}} ->
            lager:error("Parse error on ~p: ~s~n", [Line, Error]);
        {ok, Funs} ->
            {ok, fun() -> eval_all(Funs) end}
    end.

eval_all(F) when is_function(F) ->
    F();
eval_all([H]) when is_function(H) ->
    H();
eval_all([H|T]) when is_function(H) ->
    case H() of
        true ->
            eval_all(T);
        False ->
            False
    end.

build_evaluator({name, _, Name}) ->
    fun() -> evaluate_criteria(Name) end.

build_evaluator({name, _, Name}, {comparator, _, Comp}, Value) ->
    ValEval = build_value_evaluator(Value),
    [fun() -> evaluate_criteria(Name, ValEval, Comp) end].

build_value_evaluator({float, _, V}) ->
    fun() -> V end;
build_value_evaluator({integer, _, V}) ->
    fun() -> V end;
build_value_evaluator({string, _, V}) ->
    fun() -> V end;
build_value_evaluator({name, _, Name}) ->
    fun() -> crest_value:read(Name) end.


evaluate_criteria(Name) ->
    case crest_values:exists(Name) of
        false ->
            {false, Name, false, exists, true};
        true ->
            true
    end.

evaluate_criteria(Name, ProposedEval, Comp) ->
    Current = crest_value:read(Name),
    Proposed = ProposedEval(),
    case erlang:Comp(Current, Proposed) of
        false ->
            {false, Name, Current, Comp, Proposed};
        true ->
            true
    end.
