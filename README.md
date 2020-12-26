# Quad CI

![Build Status][]

Quad CI is a _simple_, _tiny_ and _beginner friendly_ Continuous Integration system written in Haskell.

### Features

- sandboxed builds in docker containers
- multi-node architecture with agents picking up jobs to work on
- http api to interact with the frontend and other nodes
- support for triggering builds with github webhooks

All in 1K lines of code!

![Build detail](screenshot.jpg)

### Getting Started

```bash
# Needed for RecordDotSyntax
$ stack install record-dot-preprocessor

# Run server
$ stack run -- start-server

# Run agent
$ stack run -- start-agent
```

Try running a simple build:

```bash
$ curl -X POST -H "Content-Type: application/json" -d \
@test/github-payload.sample.json "http://localhost:9000/webhook/github"

```

Quad CI comes with a web UI, which can be accessed at `http://localhost:3000`. To install it, run the following:

```bash
cd frontend/
yarn
yarn next
```

### Why?

TODO expand on this

An approachable codebase at just 1000 lines of Haskell code.

Real world example of Haskell application.

Show what Simple Haskell looks like.

RecordDotSytax is cool, great alternative to lens for most cases.

### Architecture

Single server - multiple agents.

Builds share workspace.

STM queue

1 build/agent concurrency limit

TODO say more

### Codebase overview

_`src/Core.hs`_  
Domain types (`Build`, `Pipeline` etc.) along with main state machine (`progress`)

_`src/Docker.hs`_  
Talks to Docker api

_`src/Runner.hs`_  
Runs a single build, collecting logs (`Core.collectLogs`) and processing state updates (`Core.progress`)

_`src/JobHandler.hs`_  
Introduces `Job` type, which is just a `Build` that can be _queued_ and _scheduled_

_`src/JobHandler/Memory.hs`_  
An in-memory implementation of `JobHandler`, built on top of STM

_`src/Github.hs`_  
Talks to Github api

_`src/Agent.hs`_  
Agents ask the server for work to do, run builds (`Runner`) and send updates back to the server

_`src/Server.hs`_  
The server collects jobs to be run (when receiving webhook events). It keeps an internal job queue (`JobHandler`) exposed as an http api (used by web ui)

_`src/Cli.hs`_  
Main entrypoint. Calls either `Server.run` or `Agent.run`

_`src/Socket.hs`_  
Low-level code to send http requests to a socket. Not interesting, can be ignored.

[build status]: https://github.com/alpacaaa/quad-ci/workflows/ci/badge.svg
