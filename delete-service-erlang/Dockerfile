FROM erlang:26.2.5.21 AS build

WORKDIR /app

COPY rebar.config rebar.lock ./

RUN rebar3 get-deps

COPY src ./src

RUN rebar3 compile

FROM erlang:26.2.5.21-alpine

WORKDIR /app

COPY --from=build /app/_build ./_build
COPY --from=build /app/rebar.config /app/rebar.lock ./

EXPOSE 5004

CMD ["sh", "-c", "exec erl -pa _build/default/lib/*/ebin -noshell -eval 'application:ensure_all_started(delete_service), timer:sleep(infinity).'"]
