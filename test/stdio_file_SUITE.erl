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

-compile({parse_transform, category}).

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

file(Suffix) ->
   lists:flatten(io_lib:format("/tmp/stdio-file-~s", [Suffix])).

%%
read(_) ->
   {ok, <<"0123456789">>} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD <- stdio:fopen(file(?FUNCTION_NAME), [read]),
      Result <- cats:unit( stream:head(stdio:in(FD)) ),
      stdio:fclose(FD),
      cats:unit(Result)
   ].

%%
read_with_buffer(_) ->
   {ok, [<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD <- stdio:fopen(file(?FUNCTION_NAME), [read]),
      Result <- cats:unit( stream:list(stdio:in(2, FD)) ),
      stdio:fclose(FD),
      cats:unit(Result)
   ].

%%
write(_) ->
   {ok, <<"0123456789">>} = [either ||
      FD <- stdio:fopen(file(?FUNCTION_NAME), [write]),
      cats:unit( stream:build([<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>]) ),
      stdio:out(_, FD),
      stdio:fclose(FD),
      
      file:read_file(file(?FUNCTION_NAME))
   ].

%%
read_write(_) ->
   {ok, <<"0123456789">>} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),
      In <- stdio:fopen(file(?FUNCTION_NAME), [read]),
      Eg <- stdio:fopen(file(read_write_eg), [write]),
      stdio:out(stdio:in(1, In), Eg),
      stdio:fclose(_),
      stdio:fclose(In),

      file:read_file(file(read_write_eg))
   ].
