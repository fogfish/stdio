%%
%%   Copyright 2012 - 2018 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(stdio_file_stream_SUITE).

-export([all/0]).
-export([
   forward/1,
   reverse/1,
   lifecycle_existing_file/1,
   lifecycle_new_file/1,
   snapshot_forward/1,
   snapshot_reverse/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
%%
forward(_) ->
   File = "/tmp/stdio-stream-forward",
   Expect = <<"0123456789">>,
   ok = file:write_file(File, Expect),
   {ok, Stream} = stdio:fstream(File, [{read_ahead, 2}]),
   [<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>] = stream:list(
      stream:takewhile(fun(X) -> X /= undefined end, Stream)
   ),
   stdio:fclose(Stream).

%%
%%
reverse(_) ->
   File = "/tmp/stdio-stream-reverse",
   Expect = <<"0123456789">>,
   ok = file:write_file(File, Expect),
   {ok, Stream} = stdio:fstream(File, [{read_ahead, 2}, reverse]),
   [<<"89">>, <<"67">>, <<"45">>, <<"23">>, <<"01">>] = stream:list(
      stream:takewhile(fun(X) -> X /= undefined end, Stream)
   ),
   stdio:fclose(Stream).

%%
%%
lifecycle_existing_file(_) ->
   File = "/tmp/stdio-stream-lc-existing",
   Expect0 = <<"0123456789">>,
   Expect1 = <<"abcdefghik">>,
   ok = file:write_file(File, Expect0),
   {ok, Stream0} = stdio:fstream(File, [{read_ahead, 10}]),
   Stream1 = stdio:send(Expect1, Stream0),
   Expect0 = stream:head(Stream1),
   Stream2 = stream:tail(Stream1),
   Expect1 = stream:head(Stream2),
   Stream3 = stream:tail(Stream2),
   undefined = stream:head(Stream3),
   stdio:fclose(Stream3).

%%
%%
lifecycle_new_file(_) ->
   File = "/tmp/stdio-stream-lc-new",
   file:delete(File),
   Expect0 = <<"0123456789">>,
   Expect1 = <<"abcdefghik">>,
   {ok, Stream0} = stdio:fstream(File, [{read_ahead, 10}]),
   undefined = stream:head(Stream0),
   Stream1 = stdio:send(Expect0, Stream0),
   Stream2 = stdio:send(Expect1, Stream1),
   undefined = stream:head(Stream2),
   Stream3 = stream:tail(Stream2),
   Expect0 = stream:head(Stream3),
   Stream4 = stream:tail(Stream3),
   Expect1 = stream:head(Stream4),
   Stream5 = stream:tail(Stream4),
   undefined = stream:head(Stream5),
   stdio:fclose(Stream5).

%%
%%
snapshot_forward(_) ->
   File = "/tmp/stdio-stream-snapshot",
   file:delete(File),
   {ok, Stream0} = stdio:fstream(File, [{read_ahead, 1}, forward]),
   Stream1 = stdio:send(<<"abcd">>, Stream0),
   Snapshot = stdio:snapshot(Stream1),
   Stream2 = stdio:send(<<"0123">>, Stream1),
   [<<"a">>, <<"b">>, <<"c">>, <<"d">>] = stream:list(Snapshot),
   stdio:fclose(Stream2).

%%
%%
snapshot_reverse(_) ->
   File = "/tmp/stdio-stream-snapshot",
   file:delete(File),
   {ok, Stream0} = stdio:fstream(File, [{read_ahead, 1}, reverse]),
   Stream1 = stdio:send(<<"abcd">>, Stream0),
   Snapshot = stdio:snapshot(Stream1),
   Stream2 = stdio:send(<<"0123">>, Stream1),
   [<<"d">>, <<"c">>, <<"b">>, <<"a">>] = stream:list(Snapshot),
   stdio:fclose(Stream2).

