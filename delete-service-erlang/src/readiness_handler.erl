-module(readiness_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Host = os:getenv("DATABASE_HOST"),
    User = os:getenv("DATABASE_USER"),
    Password = os:getenv("DATABASE_PASSWORD"),
    Database = os:getenv("DATABASE_NAME"),
    case mysql:start_link([
        {host, Host},
        {user, User},
        {password, Password},
        {database, Database}
    ]) of
        {ok, Pid} ->
            Reply = case mysql:query(Pid, "SELECT 1") of
                {ok, _, _} ->
                    {200, <<"{\"status\":\"ready\"}">>};
                {ok, _} ->
                    {200, <<"{\"status\":\"ready\"}">>};
                Error ->
                    io:format("Readiness query failed: ~p~n", [Error]),
                    {503, <<"{\"status\":\"unready\"}">>}
            end,
            mysql:stop(Pid),
            respond(Req0, State, Reply);
        Error ->
            io:format("Readiness connection failed: ~p~n", [Error]),
            respond(Req0, State, {503, <<"{\"status\":\"unready\"}">>})
    end.

respond(Req0, State, {Status, Body}) ->
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req0),
    {ok, Req, State}.
