FROM haskell:8
EXPOSE 3000

WORKDIR /decision-tree
COPY . /decision-tree

#install node js
RUN apt-get update && apt-get install -y curl
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get install -yq nodejs build-essential

#install packages and compile
RUN stack setup
RUN stack install
RUN npm install --prefix src/web/frontend
RUN stack exec build-frontend

ENTRYPOINT stack exec server
