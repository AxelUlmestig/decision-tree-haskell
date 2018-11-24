# build frontend
FROM node:carbon as build-frontend

WORKDIR /frontend
COPY src/web/frontend /frontend

RUN npm install
RUN npm run build

# build backend
FROM haskell:8 as build-backend

WORKDIR /backend
COPY . /backend

RUN stack setup
# static linking to be able to copy to 'alpine'
RUN stack build haskell-decision-tree:exe:server --copy-bins --local-bin-path . --ghc-options '-static -optc-static -optl-static -optl-pthread'

# (almost) empty image with executables
# bash seemed to be needed for some reason
FROM scratch
EXPOSE 3000

WORKDIR /decision-tree
COPY --from=build-backend /backend/server .
COPY --from=build-frontend /frontend /decision-tree/src/web/frontend

ENTRYPOINT ["/decision-tree/server"]
