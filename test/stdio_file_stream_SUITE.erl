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

-compile({parse_transform, category}).

-export([all/0]).
-export([
   forward/1,
   reverse/1,
   append/1,
   create/1,
   snapshot_forward/1,
   snapshot_forward_whole/1,
   snapshot_reverse/1,
   snapshot_reverse_whole/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

file(Suffix) ->
   lists:flatten(io_lib:format("/tmp/stdio-stream-~s", [Suffix])).

%%
%%
forward(_) ->
   {ok, [<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD <- stdio:fstream(file(?FUNCTION_NAME), []),
      Result <- cats:unit( stream:list(stdio:in({forward, 2}, FD)) ),
      stdio:fclose(FD),
      cats:unit(Result)
   ].

%%
%%
reverse(_) ->
   {ok, [<<"89">>, <<"67">>, <<"45">>, <<"23">>, <<"01">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD <- stdio:fstream(file(?FUNCTION_NAME), []),
      Result <- cats:unit( stream:list(stdio:in({reverse, 2}, FD)) ),
      stdio:fclose(FD),
      cats:unit(Result)
   ].

%%
%%
append(_) ->
   {ok, <<"0123456789abcdefghik">>} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(<<"abcdefghik">>, FD1),
      stdio:fclose(FD2),

      file:read_file(file(?FUNCTION_NAME))
   ].

%%
%%
create(_) ->
   {ok, <<"0123456789abcdefghik">>} = [either ||
      file:delete(file(?FUNCTION_NAME)),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(stream:build([<<"0123456789">>, <<"abcdefghik">>]), FD1),
      stdio:fclose(FD2),

      file:read_file(file(?FUNCTION_NAME))
   ].

%%
%%
snapshot_forward(_) ->
   {ok, [<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(<<"abcdefghik">>, FD1),
      Result <- cats:unit( stream:list(stdio:in({forward, 2}, FD1)) ),
      stdio:fclose(FD2),

      cats:unit(Result)
   ].

%%
%%
snapshot_forward_whole(_) ->
   {ok, [<<"01">>, <<"23">>, <<"45">>, <<"67">>, <<"89">>, <<"ab">>, <<"cd">>, <<"ef">>, <<"gh">>, <<"ik">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(<<"abcdefghik">>, FD1),
      cats:unit( stream:list(stdio:in({forward, 2}, FD1)) ),
      Result <- cats:unit( stream:list(stdio:in({forward, 2}, FD2)) ),
      stdio:fclose(FD2),

      cats:unit(Result)
   ].

%%
%%
snapshot_reverse(_) ->
   {ok, [<<"89">>, <<"67">>, <<"45">>, <<"23">>, <<"01">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(<<"abcdefghik">>, FD1),
      Result <- cats:unit( stream:list(stdio:in({reverse, 2}, FD1)) ),
      stdio:fclose(FD2),

      cats:unit(Result)
   ].

%%
%%
snapshot_reverse_whole(_) ->
   {ok, [<<"ik">>, <<"gh">>, <<"ef">>, <<"cd">>, <<"ab">>, <<"89">>, <<"67">>, <<"45">>, <<"23">>, <<"01">>]} = [either ||
      file:write_file(file(?FUNCTION_NAME), <<"0123456789">>),

      FD1 <- stdio:fstream(file(?FUNCTION_NAME), []),
      FD2 <- stdio:out(<<"abcdefghik">>, FD1),
      cats:unit( stream:list(stdio:in({reverse, 2}, FD1)) ),
      Result <- cats:unit( stream:list(stdio:in({reverse, 2}, FD2)) ),
      stdio:fclose(FD2),

      cats:unit(Result)
   ].
