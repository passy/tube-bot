FROM node:6.3.1
MAINTAINER Pascal Hartig <phartig@rdrei.net>

RUN useradd --user-group --create-home --shell /bin/false app
RUN mkdir -p /app
COPY package.json bower.json *.js /app/
COPY src /app/src
COPY config /app/config
RUN chown -R app:app /app

USER app
WORKDIR /app
RUN npm install
RUN npm run build

# vim:tw=0:

