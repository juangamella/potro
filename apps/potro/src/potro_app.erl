%%%-------------------------------------------------------------------
%% @doc potro public API
%% @end
%%%-------------------------------------------------------------------

-module(potro_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
  %% Start cowboy
  PoolSize = proplists:get_value(pool_size, StartArgs, 100),
  Port = proplists:get_value(port, StartArgs, 8080),
  Dispatch = cowboy_router:compile(potro_routes:routes()),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               PoolSize,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch}}
                              ),
  potro_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
