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
%% @doc
%%   A file streams
-module(stdio_file).

-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").
-include("stdio.hrl").

-export([
   new/1,
   free/1,
   sync/1,
   read/2,
   read/1,
   write/2
]).


-record(sfd, {
   fd    = undefined :: file:fd(),
   chunk = undefined :: integer()
}).


%%
%%
-spec new(file:fd()) -> datum:either( stdio:iostream() ).

new(FD) ->
   {ok, #iostream{module = ?MODULE, fd = #sfd{fd = FD}}}.

%%
%%
-spec free(#sfd{}) -> datum:either().

free(#sfd{fd = FD}) ->
   file:close(FD).

%%
%%
-spec sync(#sfd{}) -> datum:either().

sync(#sfd{fd = FD}) ->
   file:sync(FD).


%%
%%
-spec read(integer() | undefined, #sfd{}) -> stdio:stream().

read(undefined, #sfd{} = FD) ->
   read(FD#sfd{chunk = ?CONFIG_CHUNK_READER});

read(Chunk, #sfd{} = FD) ->
   read(FD#sfd{chunk = Chunk}).

read(#sfd{fd = FD, chunk = Chunk} = Stream) ->
   case file:read(FD, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, read, Stream});
      eof  ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end.

%%
%%
-spec write(binary(), #sfd{}) -> datum:either( #sfd{} ).

write(Data, #sfd{fd = FD} = Stream) ->
   [either ||
      file:write(FD, Data),
      cats:unit(Stream)
   ].
