Nonterminals
stmt exprs expr value.

Terminals
name integer float string comparator bool.

Rootsymbol stmt.

value -> integer : '$1'.
value -> float   : '$1'.
value -> string  : '$1'.
value -> name    : '$1'.

stmt  -> exprs: '$1'.
exprs -> expr bool exprs: build_bool_evaluator('$2', '$1', '$3').
exprs -> expr: '$1'.
expr  -> name comparator value: build_evaluator('$1', '$2', '$3').
expr  -> name: build_evaluator('$1').

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

-include("crest.hrl").

-export([string/1]).

string(Text) ->
    {ok, Tokens, _} = crest_lexer:string(Text),
    case parse(Tokens) of
        {error, {Line, crest_parser, Error}} ->
            lager:error("Parse error on ~p: ~s~n", [Line, Error]),
            {error, failed_parse};
        {ok, Funs} ->
            {ok, fun(ContextName, CurrentValue) -> eval_all(ContextName, CurrentValue, Funs) end}
    end.

eval_all(N, CV, F) when is_function(F) ->
    F(N, CV);
eval_all(N, CV, [H]) when is_function(H) ->
    H(N, CV);
eval_all(N, CV, [H|T]) when is_function(H) ->
    case H(N, CV) of
        true ->
            eval_all(N, CV, T);
        False ->
            False
    end.

build_evaluator({name, _, Name}) ->
    fun() -> evaluate_criteria(Name) end.

build_evaluator({name, _, Name}, {comparator, _, Comp}, Value) ->
    ValEval = build_value_evaluator(Value),
    [fun(N, CV) -> evaluate_criteria(N, CV, Name, ValEval, Comp) end].

build_bool_evaluator({bool, _, 'and'}, First, Second) ->
    fun(N, CV) -> eval_all(N, CV, First) andalso eval_all(N, CV, Second) end;
build_bool_evaluator({bool, _, 'or'}, First, Second) ->
    fun(N, CV) -> eval_all(N, CV, First) orelse eval_all(N, CV, Second) end.

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

evaluate_criteria(Name, Current, Name, ProposedEval, Comp) ->
    Proposed = ProposedEval(),
    case erlang:Comp(Current, Proposed) of
        false ->
            {false, Name, Current, Comp, Proposed};
        true ->
            true
    end;
evaluate_criteria(_N, _CV, Name, ProposedEval, Comp) ->
    Current = crest_value:read(Name),
    Proposed = ProposedEval(),
    case erlang:Comp(Current, Proposed) of
        false ->
            {false, Name, Current, Comp, Proposed};
        true ->
            true
    end.
