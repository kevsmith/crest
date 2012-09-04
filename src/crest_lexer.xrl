Definitions.

D      = [0-9]
L      = [a-zA-Z0-9\._-]
N      = [a-zA-Z]([a-zA-Z0-9\._-])*
QUOTE  = \"
WS     = ([\000-\s]|%.*)
COMP   = (<|>|=|>=|=<|!=).

Rules.
{D}+         :  {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
\-{D}+    :  {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+   :  {token, {float, TokenLine, list_to_float(TokenChars)}}.
'{L}+'       :  S = strip(TokenChars, TokenLen),
                {token, {string, TokenLine, list_to_binary(S)}}.
;            :  {token, {eos, TokenLine, TokenChars}}.
{N}+         :  {token, {name, TokenLine, list_to_binary(TokenChars)}}.
{WS}+        :  skip_token.
{COMP}       :  emit_comparator(TokenLine, strip_ws(TokenChars)).

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

strip_ws(TokenChars) ->
    string:strip(TokenChars, both, 32).
strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

emit_comparator(TokenLine, "<") ->
    {token, {comparator, TokenLine, '<'}};
emit_comparator(TokenLine, ">") ->
    {token, {comparator, TokenLine, '>'}};
emit_comparator(TokenLine, "==") ->
    {token, {comparator, TokenLine, '=='}};
emit_comparator(TokenLine, "!=") ->
    {token, {comparator, TokenLine, '/='}};
emit_comparator(TokenLine, ">=") ->
    {token, {comparator, TokenLine, '>='}};
emit_comparator(TokenLine, "=<") ->
    {token, {comparator, TokenLine, '=<'}}.
