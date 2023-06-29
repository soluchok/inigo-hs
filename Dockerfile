FROM node:lts as frontend

WORKDIR /frontend

COPY graphql-engine/frontend .

RUN pwd

RUN yarn install

RUN yarn server-build:ce

FROM haskell:9.4.5 as backend

WORKDIR /backend

COPY . .

RUN apt-get update && apt-get install -y unixodbc-dev libpq-dev
RUN cd graphql-engine && cabal update && cabal build graphql-engine

FROM haskell:9.4.5-slim
LABEL org.opencontainers.image.source=https://github.com/soluchok/inigo-hs

ENV HASURA_GRAPHQL_ENABLE_CONSOLE true
ENV HASURA_GRAPHQL_CONSOLE_ASSETS_DIR /assets

COPY --from=frontend /frontend/dist/apps/server-assets-console-ce /assets
COPY --from=backend /backend/graphql-engine/dist-newstyle/build/x86_64-linux/ghc-9.4.5/graphql-engine-1.0.0/x/graphql-engine/opt/build/graphql-engine/graphql-engine / 

COPY --from=backend /usr/lib/x86_64-linux-gnu/libodbc.* /usr/lib/x86_64-linux-gnu
COPY --from=backend /usr/lib/x86_64-linux-gnu/libpq.* /usr/lib/x86_64-linux-gnu
COPY --from=backend /usr/lib/x86_64-linux-gnu/libltdl.* /usr/lib/x86_64-linux-gnu

CMD ["/graphql-engine","serve"]
