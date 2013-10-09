json4erl
===========
[![Build Status](https://travis-ci.org/koluch/json4erl.png?branch=master)](https://travis-ci.org/koluch/json4erl)

Simple JSON library for Erlang. Support parsing from JSON to Erlang data structure. Support UTF8, UTF16 and UTF32 encodings.

Usage example:

```erlang
> json4erl:parse(<<"[1,2,3]">>).
{array,[{number,1},{number,2},{number,3}]}
> json4erl:parse(<<"{\"answer\":42, \"options\":[\"forty two\",\"don't know...\"]}">>).
{object,[{"answer",{number,42}},
         {"options",
          {array,[{string,"forty two"},{string,"I don't know..."}]}}]}
```

