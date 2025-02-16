FROM erlang:latest

WORKDIR /app

COPY . .

RUN rebar3 get-deps

RUN rebar3 compile

EXPOSE 5004

# CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(delete_service), timer:sleep(infinity).'"]
CMD ["sh", "-c", "rebar3 shell"]