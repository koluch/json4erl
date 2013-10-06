%% -*- coding: utf-8 -*-
%%% @author Nikolay Mavrenkov <koluch@koluch.ru>
%%% @copyright (C) 2013, Nikolay Mavrenkov
%%% @doc
%%%
%%% @end
%%% Created :  6 Oct 2013 by Nikolay Mavrenkov <koluch@koluch.ru>

-module(json4erl_tests).

-include_lib("eunit/include/eunit.hrl").

parse_file_test() ->
    FileDir = code:priv_dir(json4erl),
    Options = [{encoding, utf8}],
    ?assertEqual("АБВ, russian", json4erl:parse_file(FileDir ++ "/test/test1.json",Options)),
    ?assertEqual(["some string","another"], json4erl:parse_file(FileDir ++ "/test/test2.json",Options)),
    ?assertEqual(3133.7423, json4erl:parse_file(FileDir ++ "/test/test3.json", Options)),
    ?assertEqual([3133.7423, "Федорино \nгоре", ["subarray",1,2,3]], json4erl:parse_file(FileDir ++ "/test/test4.json", Options)),
    ?assertEqual([{"number",123},
		  {"string","string value 2"},
		  {"sub", [
			   {"sub1","sub value1"},
			   {"sub 1.5",["первое","второе","и третье"]},
			   {"sub2",42.0}
			  ]},
		  {"array",[1,2,3]}],
		 json4erl:parse_file(FileDir ++ "/test/test5.json", Options)).


