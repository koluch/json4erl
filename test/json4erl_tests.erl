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
    ?assertEqual({string, "АБВ, russian"}, json4erl:parse_file(FileDir ++ "/test/test1.json",Options)),

    ?assertEqual({array, [{string, "some string"},
			  {string, "another"}]}, json4erl:parse_file(FileDir ++ "/test/test2.json",Options)),

    ?assertEqual({number, 3133.7423}, json4erl:parse_file(FileDir ++ "/test/test3.json", Options)),

    ?assertEqual({array, [{number, 3133.7423},
			  {string, "Федорино \nгоре"}, 
			  {array, [{string,"subarray"},
				   {number,1},
				   {number,2},
				   {number,3}]}]},
		  json4erl:parse_file(FileDir ++ "/test/test4.json", Options)),

    ?assertEqual({object, [{"number",{number, 123}},
			   {"string",{string, "string value 2"}},
			   {"sub",{object, [{"sub1",{string, "sub value1"}},
					    {"sub 1.5",{array, [{string, "первое"},
									 {string, "второе"},
									 {string, "и третье"}]}},
					    {"sub2", {number, 42.0}}]}},
			   {"array",{array,[{number,1},
					    {number,2},
					    {number,3}]}}]},
		 json4erl:parse_file(FileDir ++ "/test/test5.json", Options)),

    %% Test utf16
    Options2 = [{encoding, utf16_little}],

    ?assertEqual({array, [{number, 3133.7423},
			  {string, "Федорино \nгоре"}, 
			  {array, [{string,"subarray"},
				   {number,1},
				   {number,2},
				   {number,3}]}]},
		  json4erl:parse_file(FileDir ++ "/test/test4_utf16.json", Options2)).


