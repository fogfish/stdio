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
-module(stdio_file_SUITE).

-export([all/0]).
-export([
   read/1,
   read_with_buffer/1,
   write/1,
   read_write/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
read(_) ->
   File = "/tmp/stdio-read",
   Expect = <<"0123456789">>,
   ok = file:write_file(File, Expect),
   {ok, Stream} = stdio:fopen(File, [read]),
   Expect = stream:head(Stream),
   stdio:fclose(Stream).

%%
read_with_buffer(_) ->
   File = "/tmp/stdio-read-with_buffer",
   Expect = <<"0123456789">>,
   ok = file:write_file(File, Expect),
   {ok, Stream} = stdio:fopen(File, [read, {read_ahead, 1}]),
   <<"0">> = stream:head(Stream),
   <<"1">> = stream:head(stream:tail(Stream)),
   <<"2">> = stream:head(stream:tail(Stream)),
   stdio:fclose(Stream).

%%
write(_) ->
   File = "/tmp/stdio-write",
   Expect = <<"0123456789">>,
   {ok, Stream} = stdio:fopen(File, [write]),
   stdio:send(Expect, Stream),
   stdio:fclose(Stream),
   {ok, Expect} = file:read_file(File).

%%
read_write(_) ->
   FileIn = "/tmp/stdio-read-write-in",
   FileEg = "/tmp/stdio-read-write-eg",
   Expect = <<"0123456789">>,
   ok = file:write_file(FileIn, Expect),
   {ok, In} = stdio:fopen(FileIn, [read, {read_ahead, 1}]),
   {ok, Eg} = stdio:fopen(FileEg, [write]),
   stdio:sink(In, Eg),
   stdio:fclose(In),
   stdio:fclose(Eg),
   {ok, Expect} = file:read_file(FileEg).





