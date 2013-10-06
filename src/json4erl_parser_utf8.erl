%%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%%% @copyright (C) 2013, Nikolay Mavrenkov
%%% @doc
%%%
%%% @end
%%% Created :  4 Oct 2013 by Nikolay Mavrenkov <koluch@koluch.ru>

-module(json4erl_parser_utf8).
-export([parse/4]).

parse_binary(Binary, utf8) ->
    json4erl_parser_utf8:parse({0,0}, Binary, next_el, nil).

parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, next_el, Acc) ->
    case Ch of
	S when S=:=$ ;S=:=$\t -> parse({Line,Column+1}, Rest, next_el, Acc); %% skip spaces and tabs
	$\n -> parse({Line+1,0}, Rest, next_el, Acc);                        %% increate line and zero columns for new lines
	$" -> parse({Line,Column+1}, Rest, read_string, []);
	Dig when is_integer(Dig), Dig >= $0, Dig =< $9 -> parse({Line,Column}, Bin, read_number, []);
	$[ -> parse({Line,Column+1}, Rest, read_array_begin, []);
	${ -> parse({Line,Column+1}, Rest, read_object, []);
	Ch -> throw({"Next char is not a first char of any element (string, number, array or object)!", {[Ch], {line, Line+1}, {column, Column}}})
    end;


%% String
parse({Line,Column}, <<Ch/utf8, Rest/binary>>, read_string, Acc) ->
    case Ch of
	$" -> {lists:reverse(Acc), {Rest, Line, Column+1}};
	$\n -> parse({Line+1,Column}, Rest, read_string, [Ch|Acc]);
	_ -> parse({Line,Column+1}, Rest, read_string, [Ch|Acc])
    end;

%% Number
parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, read_number, Acc) ->
    case Ch of
	$. -> parse({Line,Column+1}, Rest, read_number, [Ch|Acc]);
	Dig when is_integer(Dig),Dig >= $0, Dig =< $9 -> parse({Line,Column+1}, Rest, read_number, [Ch|Acc]);
	_ -> 
	    Rev = lists:reverse(Acc),
	    {Num,[]} = case string:to_float(Rev) of
		      {error, no_float} -> string:to_integer(Rev);
		      Float -> Float
		  end,
	    {Num, {Bin, Line, Column}}
    end;


%% Array
parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, read_array_begin, Acc) ->
    case Ch of
	$] -> {lists:reverse(Acc), {Rest, Line, Column}};
	_ -> 
	    {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
	    parse({NewLine, NewColumn+1}, NewRest, read_array, [El])
    end;

parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, read_array, Acc) ->
    case Ch of
	S when S=:=$ ;S=:=$\t -> parse({Line+1, Column}, Rest, read_array, Acc);
	$] -> 
	    {lists:reverse(Acc), {Rest, Line, Column}};
	$, -> 
	    {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Rest, next_el, []),
	    parse({NewLine, NewColumn+1}, NewRest, read_array, [El|Acc]);
	_ -> 
	    {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
	    parse({NewLine, NewColumn+1}, NewRest, read_array, [El|Acc])
    end;

%% Objects
parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, read_object, Acc) ->
    case Ch of
	S when S=:=$ ;S=:=$\t -> parse({Line, Column+1}, Rest, read_object, Acc);
	$\n -> parse({Line+1, 0}, Rest, read_object, Acc);
	$" -> 
	    {El,{NewRest, NewLine, NewColumn}} = parse({Line, Column}, Bin, read_object_pair, Acc),
	    parse({NewLine, NewColumn+1}, NewRest, read_object, [El|Acc]);
	$} -> 
	    {lists:reverse(Acc), {Rest, Line, Column}};
	$, -> 
	    {El,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Rest, read_object_pair, []),
	    parse({NewLine, NewColumn+1}, NewRest, read_object, [El|Acc]);
	Ch -> throw({"Next char is not a first char of object key name!", {[Ch], {line, Line+1}, {column, Column}}})
    end;

parse({Line,Column}, Bin = <<Ch/utf8, Rest/binary>>, read_object_pair, Acc) ->
    case Ch of
	S when S=:=$ ;S=:=$\t -> parse({Line,Column+1}, Rest, read_object_pair, Acc);
	$\n -> parse({Line+1,0}, Rest, read_object_pair, Acc);
	$" -> 
	    {Key,{NewRest, NewLine, NewColumn}} = parse({Line,Column}, Bin, next_el, []),
	    <<$:/utf8,ValueBin/binary>> = NewRest,
	    {Value,NewContext} = parse({NewLine,NewColumn}, ValueBin, next_el, []),
	    {{Key,Value},NewContext};
	Ch -> throw({"Next char is not a first char of object key name!", {[Ch], {line, Line+1}, {column, Column}}})
    end.