%% -*- coding: utf-8; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%-------------------------------------------------------------------
%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%% @copyright (C) 2013, Nikolay Mavrenkov
%% @doc
%% 
%% @end
%%-------------------------------------------------------------------
%% Copyright 2013 Nikolay Mavrenkov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-------------------------------------------------------------------
%% Created :  4 Oct 2013 by Nikolay Mavrenkov <koluch@koluch.ru>

-module(json4erl).
-export([parse_file/1,parse_file/2, parse/1, parse/2]).

-define(DEFAULT_OPTIONS, [{encoding,utf8}]).

%% Interface

parse_file(FileName) -> parse_file(FileName, ?DEFAULT_OPTIONS).
parse_file(FileName, Options) ->
    {ok, Binary} = file:read_file(FileName),
    parse(Binary, Options).


parse(Anything) -> 
    parse(Anything, ?DEFAULT_OPTIONS).

parse(String, Options) when is_list(String) -> 
    parse(list_to_binary(String), Options);
parse(Binary, Options) when is_binary(Binary) -> 
    call_encoding_module(Binary,  proplists:get_value(encoding, Options)).

%% Internal functions
call_encoding_module(Binary, utf8) ->
    {Parsed, {_RestBin, _Line, _Column}} = json4erl_parser_utf8:parse({0,0}, Binary, next_el, nil),
    Parsed;

call_encoding_module(Binary, utf16) ->
    {Parsed, {_RestBin, _Line, _Column}} = json4erl_parser_utf16:parse({0,0}, Binary, next_el, nil),
    Parsed;

call_encoding_module(Binary, utf16_little) ->
    {Parsed, {_RestBin, _Line, _Column}} = json4erl_parser_utf16_little:parse({0,0}, Binary, next_el, nil),
    Parsed;

call_encoding_module(Binary, utf32) ->
    {Parsed, {_RestBin, _Line, _Column}} = json4erl_parser_utf32:parse({0,0}, Binary, next_el, nil),
    Parsed;

call_encoding_module(Binary, utf32_little) ->
    {Parsed, {_RestBin, _Line, _Column}} = json4erl_parser_utf32_little:parse({0,0}, Binary, next_el, nil),
    Parsed.



