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

-export([
   forward/2,
   forward/1,
   reverse/2,
   reverse/1,
   close/1,
   send/2,
   snapshot/1
]).

-record(sfd, {
   fd    = undefined :: _,   %% file descriptor
   chunk = undefined :: _,   %% stream chunk (unit of work)
   size  = undefined :: _,   %% size of stream in bytes
   at    = undefined :: _    %% 
}).


%%
%%
-spec forward( stdio:filename(), file:mode() ) -> datum:either( stdio:stream() ).

forward(File, Opts) ->
   [either ||
      file:open(File, [raw, read, append, binary | allowed_opts(Opts)]),
      cats:unit(
         forward(
            #sfd{
               fd    = _, 
               chunk = lens:get(lens:pair(read_ahead), Opts),
               size  = filelib:file_size(File),
               at    = 0
            }
         )
      )
   ].

forward(#sfd{fd = FD, chunk = Chunk, at = At} = Stream) ->
   case file:pread(FD, At, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, forward, Stream#sfd{at = At + Chunk}});
      eof  ->
         stream:new(undefined, {?MODULE, forward, Stream});
      {error, Reason} ->
         file:close(FD),
         exit(Reason)
   end.


%%
%%
-spec reverse( stdio:filename(), file:mode() ) -> datum:either( stdio:stream() ).

reverse(File, Opts) ->
   [either ||
      file:open(File, [raw, read, append, binary | allowed_opts(Opts)]),
      cats:unit(
         reverse(
            #sfd{
               fd    = _, 
               chunk = lens:get(lens:pair(read_ahead), Opts),
               size  = filelib:file_size(File),
               at    = filelib:file_size(File)
            }
         )
      )
   ].

reverse(#sfd{at = 0} = Stream) ->
   stream:new(undefined, {?MODULE, reverse, Stream});

reverse(#sfd{fd = FD, chunk = Chunk, at = At} = Stream) ->
   case file:pread(FD, At - Chunk, Chunk) of
      {ok, Head} ->
         stream:new(Head, {?MODULE, reverse, Stream#sfd{at = At - Chunk}});
      eof  ->
         stream:new(undefined, {?MODULE, reverse, Stream});
      {error, Reason} ->
         file:close(FD),
         exit(Reason)
   end.

%%
%%
-spec close(stdio:stream()) -> datum:either().

close(#stream{tail = {?MODULE, _, #sfd{fd = FD}}}) ->
   file:close(FD).

%%
%%
-spec send(binary(), stdio:stream()) -> datum:either().

send(Data, #stream{tail = {?MODULE, IO, #sfd{fd = FD, size = Size} = File}} = Stream) ->
   case file:write(FD, Data) of
      ok ->
         Stream#stream{tail = {?MODULE, IO, File#sfd{size = Size + size(Data)}}};
      {error, Reason} ->
         exit(Reason)
   end.

%%
%%
-spec snapshot(stdio:stream()) -> stdio:stream().

snapshot(#stream{tail = {?MODULE, forward, #sfd{} = File}}) ->
   stream:unfold(fun snapshot_forward/1, File#sfd{at = 0});

snapshot(#stream{tail = {?MODULE, reverse, #sfd{size = Size} = File}}) ->
   stream:unfold(fun snapshot_reverse/1, File#sfd{at = Size}).


snapshot_forward(#sfd{size = Size, at = At})
 when At >= Size ->
   ?None;

snapshot_forward(#sfd{fd = FD, chunk = Chunk, at = At} = File) ->
   case file:pread(FD, At, Chunk) of
      {ok, Head} ->
         {Head, File#sfd{at = At + Chunk}};
      eof ->
         ?None;
      {error, Reason} ->
         exit(Reason)
   end.

snapshot_reverse(#sfd{at = 0}) ->
   ?None;
   
snapshot_reverse(#sfd{fd = FD, chunk = Chunk, at = At} = File) ->
   case file:pread(FD, At - Chunk, Chunk) of
      {ok, Head} ->
         {Head, File#sfd{at = At - Chunk}};
      eof ->
         ?None;
      {error, Reason} ->
         exit(Reason)
   end.

%%
%%
allowed_opts(Opts) ->
   lists:filter(fun is_allowed/1, Opts).

is_allowed({delayed_write, _, _}) -> true;
is_allowed({read_ahead, _}) -> true;
is_allowed(_) -> false.

