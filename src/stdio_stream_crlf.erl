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
-module(stdio_stream_crlf).
-include_lib("datum/include/datum.hrl").

-export([unfold/1]).

unfold(Stream) ->
   stream:unfold(fun decode/1, {<<>>, ?None, Stream}).

decode({<<>>, ?None, ?stream()}) ->
   ?None;

decode({Head, ?None, ?stream() = Stream}) ->
   {Head, {<<>>, ?None, Stream}};

decode({Head, ?None, Stream}) ->
   case binary:split(<<Head/binary, (stream:head(Stream))/binary>>,  [<<$\r, $\n>>, <<$\n>>]) of
      %% stream head do not have CRLF
      [Tail] ->
         decode({Tail, ?None, stream:tail(Stream)});
      [HTail, TTail] ->
         {HTail, {<<>>, TTail, stream:tail(Stream)}}
   end;

decode({Head, Tail, Stream}) ->
   case binary:split(<<Head/binary, Tail/binary>>, [<<$\r, $\n>>, <<$\n>>]) of
      %% stream tail do not have CRLF
      [HTail] ->
         decode({HTail, ?None, Stream});
      [HTail, TTail] ->
         {HTail, {<<>>, TTail, Stream}}
   end.
