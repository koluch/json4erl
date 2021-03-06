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
-export([parse/4]).
-ifndef(ENCODING).
-define(ENCODING, utf8).
-endif.


parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, next_el, Acc) ->
    case Ch of
        S when S=:=$ ;S=:=$\t -> parse({Line,Column+1}, Rest, next_el, Acc); % skip spaces, tabs and \r char
        16#FEFF -> parse({Line,Column}, Rest,next_el, []);              % skip BOM
        $\n -> parse({Line+1,0}, Rest, next_el, Acc);                   % increase line and zero columns for new lines
        $\r -> parse({Line,0}, Rest, next_el, Acc);                   % increase line and zero columns for new lines
        $" -> parse({Line,Column+1}, Rest, read_string, []);
        Dig when is_integer(Dig), Dig >= $0, Dig =< $9 -> parse({Line,Column}, Bin, read_number, []);
        $[ -> parse({Line,Column+1}, Rest, read_array_begin, []);
        ${ -> parse({Line,Column+1}, Rest, read_object, []);
        B when B =:= $t; B =:= $f -> parse({Line,Column+1}, Rest, read_boolean, [Ch]);
        _ -> throw({"Next char is not a first char of any element (string, number, array, object or boolean)!", {[Ch], {line, Line+1}, {column, Column}}})
    end;


%% String
parse({Line,Column}, <<Ch/?ENCODING, Rest/binary>>, read_string, Acc) ->
    case Ch of
        $" -> {create_element(string, lists:reverse(Acc)), {Rest, Line, Column+1}};
        $\r -> parse({Line,Column}, Rest, read_string, [Ch|Acc]);
        $\n -> parse({Line+1,Column}, Rest, read_string, [Ch|Acc]);
        _ -> parse({Line,Column+1}, Rest, read_string, [Ch|Acc])
    end;

%% Boolean
parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_boolean, Acc) ->
    case Ch of
        _ when Ch =:= $t; Ch =:= $r; Ch =:= $u; Ch =:= $e;
               Ch =:= $f; Ch =:= $a; Ch =:= $l; Ch =:= $s ->
            parse({Line,Column+1}, Rest, read_boolean, [Ch|Acc]);
        _ ->
            Val = lists:reverse(Acc),
            if Val =:= "true" orelse Val =:= "false" ->
                    {create_element(boolean, list_to_atom(Val)), {Bin, Line, Column}};
            true ->
                throw({"Next char is not a char of true/false boolean values!", {Val, {line, Line+1}, {column, Column}}})
            end
    end;

%% Number
parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_number, Acc) ->
    case Ch of
        $. -> parse({Line,Column+1}, Rest, read_number, [Ch|Acc]);
        Dig when is_integer(Dig),Dig >= $0, Dig =< $9 -> parse({Line,Column+1}, Rest, read_number, [Ch|Acc]);
        _ ->
            Rev = lists:reverse(Acc),
            {Num,[]} = case string:to_float(Rev) of
                           {error, no_float} -> string:to_integer(Rev);
                           Float -> Float
                       end,
            {create_element(number, Num), {Bin, Line, Column}}
    end;


%% Array
parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_array_begin, Acc) ->
    case Ch of
        $] -> {create_element(array, lists:reverse(Acc)), {Rest, Line, Column}};
        _ ->
            {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
            parse({NewLine, NewColumn+1}, NewRest, read_array, [El])
    end;

parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_array, Acc) ->
    case Ch of
        S when S=:=$ ;S=:=$\t -> parse({Line, Column+1}, Rest, read_array, Acc);
        $\r -> parse({Line, Column}, Rest, read_array, Acc);
        $\n -> parse({Line+1, Column}, Rest, read_array, Acc);

        $] ->
            {create_element(array, lists:reverse(Acc)), {Rest, Line, Column}};
	    $, ->
            {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Rest, next_el, []),
            parse({NewLine, NewColumn+1}, NewRest, read_array, [El|Acc]);
	    _ ->
            {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
            parse({NewLine, NewColumn+1}, NewRest, read_array, [El|Acc])
    end;

%% Objects
parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_object, Acc) ->
    case Ch of
        S when S=:=$ ;S=:=$\t -> parse({Line, Column+1}, Rest, read_object, Acc);
        $\n -> parse({Line+1, Column}, Rest, read_object, Acc);
        $\r -> parse({Line, Column}, Rest, read_object, Acc);
        $" ->
            {El,{NewRest, NewLine, NewColumn}} = parse({Line, Column}, Bin, read_object_pair, Acc),
            parse({NewLine, NewColumn+1}, NewRest, read_object, [El|Acc]);
        $} ->
            {create_element(object, lists:reverse(Acc)), {Rest, Line, Column}};
        $, ->
            {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Rest, read_object_pair, []),
            parse({NewLine, NewColumn+1}, NewRest, read_object, [El|Acc]);
        Ch -> throw({"Next char is not a first char of object key name!", {[Ch], {line, Line+1}, {column, Column}}})
    end;

parse({Line,Column}, Bin = <<Ch/?ENCODING, Rest/binary>>, read_object_pair, Acc) ->
    case Ch of
        S when S=:=$ ;S=:=$\t -> parse({Line,Column+1}, Rest, read_object_pair, Acc);
        $\n -> parse({Line+1,Column}, Rest, read_object_pair, Acc);
        $\r -> parse({Line, Column}, Rest, read_object_pair, Acc);
        $" ->
            {Key,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
            <<$:/?ENCODING,ValueBin/binary>> = NewRest,
            {Value,NewContext} = parse({NewLine,NewColumn}, ValueBin, next_el, []),
            {{create_element(object_key, Key),Value},NewContext};
        Ch -> throw({"Next char is not a first char of object key name!", {[Ch], {line, Line+1}, {column, Column}}})
    end.

create_element(object, Data) -> {object, Data};
create_element(object_key, {string,Data}) -> Data;
create_element(array, Data) -> {array, Data};
create_element(number, Data) -> {number, Data};
create_element(string, Data) -> {string, Data};
create_element(boolean, Data) -> {boolean, Data}.
