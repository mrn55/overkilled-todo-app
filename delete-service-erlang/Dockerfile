FROM erlang:26.2.5.21-alpine AS build

WORKDIR /app

COPY rebar.config rebar.lock ./

RUN rebar3 get-deps

COPY src ./src

RUN rebar3 as prod release

FROM erlang:26.2.5.21-alpine

LABEL org.opencontainers.image.title="Overkilled Todo Delete Service" \
      org.opencontainers.image.description="Erlang CRUD delete service for the Overkill(ed) Todo App" \
      org.opencontainers.image.source="https://github.com/mrn55/overkilled-todo-app" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.licenses="MIT"

RUN addgroup -S -g 10001 app && adduser -S -u 10001 -G app app

WORKDIR /app

COPY --from=build --chown=app:app /app/_build/prod/rel/delete_service ./delete_service

USER app

EXPOSE 5004

CMD ["/app/delete_service/bin/delete_service", "foreground"]
