FROM node:8.9
MAINTAINER Alex Brandt <alunduil@alunduil.com>

RUN npm install --global bower

WORKDIR /usr/local/src/my.api.dungeon.studio

COPY bower.json /usr/local/src/my.api.dungeon.studio/bower.json
RUN bower --allow-root install

COPY package.json /usr/local/src/my.api.dungeon.studio/package.json
COPY package-lock.json /usr/local/src/my.api.dungeon.studio/package-lock.json
RUN npm install

COPY . /usr/local/src/my.api.dungeon.studio/
RUN npm run build

FROM nginx:alpine
MAINTAINER Alex Brandt <alunduil@alunduil.com>

COPY --from=0 /usr/local/src/my.api.dungeon.studio/dist/ /usr/share/nginx/html
