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
%%   append only file
-module(stdio_file_stream).

-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").
-include("stdio.hrl").

-export([
   new/2,
   free/1,
   sync/1,
   read/2,
   write/2,
   forward/1,
   reverse/1
]).

-record(sfd, {
   fd    = undefined :: _,   %% file descriptor
   chunk = undefined :: _,   %% stream chunk (unit of work)
   size  = undefined :: _,   %% size of stream in bytes
   at    = undefined :: _    %% stream position
}).


%%
%%
-spec new(stdio:filename(), file:mode()) -> datum:either( stdio:iostream() ).

new(File, Opts) ->
   [either ||
      file:open(File, [raw, read, append, binary | allowed_opts(Opts)]),
      cats:unit(
         #iostream{
            module = ?MODULE, 
            fd     = #sfd{
               fd    = _,
               size  = filelib:file_size(File)
            }
         }
      )
   ].

%%
%%
allowed_opts(Opts) ->
   lists:filter(fun is_allowed/1, Opts).

is_allowed({delayed_write, _, _}) -> true;
is_allowed({read_ahead, _}) -> true;
is_allowed(_) -> false.


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
-spec read({forward | reverse, integer()} | undefined, #sfd{}) -> stdio:stream().

read(undefined, #sfd{} = FD) ->
   forward(1, FD);
read({forward, Chunk}, #sfd{} = FD) ->
   forward(Chunk, FD);
read({reverse, Chunk}, #sfd{} = FD) ->
   reverse(Chunk, FD).


forward(Chunk, #sfd{} = FD) ->
   forward(FD#sfd{chunk = Chunk, at = 0}).

forward(#sfd{at = Size, size = Size}) ->
   stream:new();

forward(#sfd{fd = FD, chunk = Chunk, at = At} = Stream) ->
   case file:pread(FD, At, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, forward, Stream#sfd{at = At + Chunk}});
      eof  ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end.


reverse(Chunk, #sfd{size = Size} = FD) ->
   reverse(FD#sfd{chunk = Chunk, at = Size}).

reverse(#sfd{at = 0}) ->
   stream:new();

reverse(#sfd{fd = FD, chunk = Chunk, at = At} = Stream) ->
   case file:pread(FD, At - Chunk, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, reverse, Stream#sfd{at = At - Chunk}});
      eof  ->
         stream:new();
      {error, Reason} ->
         exit(Reason)
   end.


%%
%%
-spec write(binary(), #sfd{}) -> datum:either( #sfd{} ).

write(Data, #sfd{fd = FD, size = Size} = Stream) ->
   [either ||
      file:write(FD, Data),
      cats:unit(Stream#sfd{size = Size + size(Data)})
   ].
