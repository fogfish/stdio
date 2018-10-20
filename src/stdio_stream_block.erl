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
-module(stdio_stream_block).
-include_lib("datum/include/datum.hrl").

-export([unfold/2]).

unfold(N, Stream) ->
   stream:unfold(fun decode/1, {<<>>, N, Stream}).

decode({<<>>, _, ?stream()}) ->
   ?None;

decode({Head, N, ?stream() = Stream})
 when size(Head) < N ->
   {Head, {<<>>, N, Stream}};

decode({Head, N, Stream})
 when size(Head) < N ->
   Tail = stream:head(Stream),
   decode({<<Head/binary, Tail/binary>>, N, stream:tail(Stream)});

decode({Head, N, Stream}) ->
   <<HHead:N/binary, THead/binary>> = Head,
   {HHead, {THead, N, Stream}}.
