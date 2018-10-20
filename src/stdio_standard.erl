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
-module(stdio_standard).

-include_lib("datum/include/datum.hrl").

-export([
   reader/0,
   reader/1,
   writer/0,
   send/2
]).

%%
%%
-spec reader() -> datum:either( stdio:stream() ).
-spec reader(integer()) -> datum:either( stdio:stream() ).

reader() ->
   case file:read_line(standard_io) of
      {ok, Head} ->
         stream:new(Head, fun reader/0);
      eof ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end.   

reader(Chunk) ->
   case file:read(standard_io, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, reader, Chunk});
      eof ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end.

%%
%%
-spec writer() -> datum:either( stdio:stream() ).

writer() ->
   stream:new(undefined, {?MODULE, reader, undefined}).

%%
%%
-spec send(binary(), stdio:stream()) -> datum:either().

send(Data, #stream{tail = {?MODULE, _, undefined}} = Stream) ->
   case file:write(standard_io, Data) of
      ok ->
         Stream;
      {error, Reason} ->
         exit(Reason)
   end.
