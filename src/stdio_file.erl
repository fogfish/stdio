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

-export([
   reader/2,
   reader/1,
   writer/2,
   close/1,
   send/2
]).

-record(sfd, {
   fd    = undefined :: file:fd(),
   chunk = undefined :: integer()
}).

%%
%%
-spec reader(stdio:filename(), file:mode()) -> datum:either( stdio:stream() ).

reader(File, Opts) ->
   [either ||
      file:open(File, [raw, binary | Opts]),
      cats:unit(
         reader(
            #sfd{
               fd    = _, 
               chunk = lens:get(lens:pair(read_ahead, 64 * 1024), Opts)
            }
         )
      )
   ].

reader(#sfd{fd = FD, chunk = Chunk} = Stream) ->
   case file:read(FD, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, reader, Stream});
      eof  ->
         file:close(FD),
         stream:new();
      {error, Reason} ->
         file:close(FD),
         exit(Reason)
   end.

%%
%%
-spec writer(stdio:filename(), file:mode()) -> datum:either( stdio:stream() ).

writer(File, Opts) ->
   [either ||
      file:open(File, [raw, binary | Opts]),
      cats:unit(
         stream:new(undefined, 
            {?MODULE, reader, 
               #sfd{
                  fd    = _,
                  chunk = lens:get(lens:c(lens:keylist(1, delayed_write, {delayed_write, 64 * 1024, 2}), lens:t2()), Opts)
               }
            }
         )
      )
   ].

%%
%%
-spec close(stdio:stream()) -> datum:either().

close(#stream{tail = {?MODULE, _, #sfd{fd = FD}}}) ->
   file:close(FD).

%%
%%
-spec send(binary(), stdio:stream()) -> datum:either().

send(Data, #stream{tail = {?MODULE, _, #sfd{fd = FD}}} = Stream) ->
   case file:write(FD, Data) of
      ok ->
         Stream;
      {error, Reason} ->
         exit(Reason)
   end.
