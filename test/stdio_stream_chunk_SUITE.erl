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
-module(stdio_stream_chunk_SUITE).

-export([all/0]).
-export([
   all_in_one/1,
   all_in_few/1,
   all_in_all/1,
   one_in_few/1,
   one_in_one/1
]).


all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

all_in_one(_) ->
   [<<"a">>, <<"b">>, <<"c">>, <<"d">>] = stream:list(
      stdio:chunk(1,
         stream:build([<<"abcd">>])
      )
   ).

all_in_few(_) ->
   [<<"ab">>, <<"cd">>] = stream:list(
      stdio:chunk(2,
         stream:build([<<"a">>, <<"b">>, <<"cd">>])
      )
   ).

all_in_all(_) ->
   [<<"a1">>, <<"b2">>, <<"c3">>, <<"d4">>] = stream:list(
      stdio:chunk(2,
         stream:build([<<"a1">>, <<"b2">>, <<"c3">>, <<"d4">>])
      )
   ).

one_in_few(_) ->
   [<<"abcd">>, <<"efgh">>] = stream:list(
      stdio:chunk(4,
         stream:build([<<"ab">>, <<"cd">>, <<"ef">>, <<"gh">>])
      )
   ).

one_in_one(_) ->
   [<<"abcd">>] = stream:list(
      stdio:chunk(5,
         stream:build([<<"a">>, <<"b">>, <<"c">>, <<"d">>])
      )
   ).

