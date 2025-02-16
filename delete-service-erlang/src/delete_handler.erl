-module(delete_handler).
-export([init/2]).

init(Req, State) ->
    io:format("Request received in delete_handler~n"),
    Id = cowboy_req:binding(id, Req),
    io:format("Extracted ID: ~p~n", [Id]),

    case Id of
        undefined ->
            io:format("ID not found in request URL.~n"),
            Resp = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Invalid request">>, Req),
            {ok, Resp, State};
        _ ->
            delete_todo(Id),
            Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"Deleted!">>, Req),
            {ok, Resp, State}
    end.

delete_todo(Id) ->
    io:format("Received delete request for ID: ~p~n", [Id]),

    Host = os:getenv("DATABASE_HOST"),
    User = os:getenv("DATABASE_USER"),
    Password = os:getenv("DATABASE_PASSWORD"),
    Database = os:getenv("DATABASE_NAME"),

    %% Convert binary ID to integer
    IntId = binary_to_integer(Id),
    
    case mysql:start_link([
        {host, Host},
        {user, User},
        {password, Password},
        {database, Database}
    ]) of
        {ok, Pid} ->
            Query = io_lib:format("DELETE FROM todos WHERE id = ~p", [IntId]),
            io:format("Executing query: ~s~n", [Query]),
            case mysql:query(Pid, Query) of
                {ok, _} ->
                    {200, [{<<"content-type">>, <<"application/json">>}], <<"{\"message\": \"Todo deleted\"}">>};
                Error ->
                    io:format("MySQL Query Error: ~p~n", [Error]),
                    {500, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"Database error\"}">>}
            end;
        Error ->
            io:format("MySQL Connection Error: ~p~n", [Error]),
            {500, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"DB connection failed\"}">>}
    end.
