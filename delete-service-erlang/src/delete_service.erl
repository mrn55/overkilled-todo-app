-module(delete_service).
-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    application:start(delete_service).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/todo/:id", delete_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 5004}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Delete service started!~n"),
    {ok, self()}.

stop(_State) ->
    io:format("Delete service stopping.~n"),
    ok.
