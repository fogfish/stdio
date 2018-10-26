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

-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").
-include("stdio.hrl").

-export([
   new/0,
   free/1,
   read/2,
   write/2
]).

%%
%%
-spec new() -> datum:either( stdio:iostream() ).

new() ->
   {ok, #iostream{module = ?MODULE}}.


%%
%%
-spec free(undefined) -> datum:either().

free(undefined) ->
   ok.


%%
%%
-spec read(integer() | undefined, undefined) -> stdio:stream().

read(undefined, undefined) ->
   case file:read_line(standard_io) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, reader, undefined});
      eof ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end;

read(Chunk, undefined) ->
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
-spec write(binary(), undefined) -> datum:either( undefined ).

write(Data, undefined) ->
   [either ||
      file:write(standard_io, Data),
      cats:unit(undefined)
   ].
