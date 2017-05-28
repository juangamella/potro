%% Copyright (c) 2017, Juan Luis Gamella Martín
%% All rights reserved.

%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:

%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.

%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.

%% * Neither the name of the copyright holder nor the names of its
%%   contributors may be used to endorse or promote products derived from
%%   this software without specific prior written permission.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc Template for all the erlang modules used in the potro app
-module(potro_r_users).

%% Load eunit
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Includes

%% Exports
-export([init/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         forbidden/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         handle_post/2
        ]).

%% Macro definitions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

init(Req, State) ->
  {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

malformed_request(Req, State) ->
  {false, Req, State}.

is_authorized(Req, State) ->
  {true, Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

content_types_provided(Req, State) ->
  Provided = [{{<<"application">>, <<"json">>, '*'}, to_json}],
  {Provided, Req, State}.

content_types_accepted(Req, State) ->
  Accepted = [{{<<"application">>, <<"json">>, '*'}, handle_post}],
  {Accepted, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

to_json(Req, State) ->
  {<<"{\"hello\": \"world\"}">>, Req, State}.

handle_post(Req, State) ->
  {true, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests start
-ifdef(TEST).

%% @doc Test functions that can run without additional context
basic_test_() ->
  [].

%% Tests end
-endif.
