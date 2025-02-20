FROM erlang:latest

WORKDIR /app

COPY . .

RUN rebar3 get-deps

RUN rebar3 compile

FROM erlang:alpine

WORKDIR /app

COPY --from=0 /app .

EXPOSE 5004

CMD ["sh", "-c", "rebar3 shell"]