## disruption-fb-bot

[![Build Status](https://travis-ci.org/passy/tube-bot.svg?branch=master)](https://travis-ci.org/passy/tube-bot)
[![](https://images.microbadger.com/badges/version/passy/tube-bot.svg)](https://microbadger.com/images/passy/tube-bot "Get your own version badge on microbadger.com")

> A very rudimentary, incomplete Messenger bot for alerting you about Tube
> disruptions.

This is a hybrid written in JavaScript and PureScript (after I got too tired of
stacktrace-less runtime errors), powered by a Haskell service, backed by a
RethinkDB.

Check out
[passy/disruption-bot-deployment](https://github.com/passy/disruption-bot-deployment)
for an easy setup.

## Setup

```
$ yarn
$ npm run build
$ docker run rethink:2.3
# For prod-ish use.
$ npm run start
# For dev.
$ npm run watch
$ pulp -w build
```

- Copy `config/default.json.example` to `config/default.json`.
- Create a new [Facebook App](https://developers.facebook.com).
- Copy the app secret from here and use it for your config.
- Create a new Facebook Page that you're bot will identify as.
- Find the Page ID in the page source (Ctrl+F for `pageId`). Yeah, this is
  ridiculous but I couldn't find another way to find it.
- Add "Messenger" as product. Generate a page access token from here, use it for
  the config.
- Set up the webhook. Generate a random long string as verification token. Use
  this in your config and enter it here.
- Use ngrok or localtunnel for testing to have an https-enabled way to link to
  your local webserver.
- Tick all the boxes in the popup for the messenger, even if we don't use most
  of them at the moment.
- Check `docker ps` for the host and port of your rethinkdb host.

## Development

Run these in separate terminal multiplexer panels:

```
$ pulp -w build
$ npm run watch
```
