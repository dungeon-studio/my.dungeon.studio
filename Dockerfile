FROM node:8.9
MAINTAINER Alex Brandt <alunduil@alunduil.com>

RUN npm install --global bower

WORKDIR /usr/local/src/my.api.dungeon.studio

COPY package.json /usr/local/src/my.api.dungeon.studio/package.json
RUN npm install

COPY . /usr/local/src/my.api.dungeon.studio/
RUN npm run build
