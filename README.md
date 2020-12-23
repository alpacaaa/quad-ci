# Quad CI

Quad CI is a _simple_ Continuous Integration system written in Haskell.

### Features

- sandboxed builds in docker containers
- a multi-node architecture with agents picking up jobs to work on
- an http api to interact with the frontend and other nodes
- support for triggering builds with github webhooks

![Build detail](screenshot.jpg)

### Getting Started

```bash
stack build

# Run server
stack run -- start-server

# Run agent
stack run -- start-agent
```

Try running a simple build:

```bash
$ curl -X POST -H "Content-Type: application/json" -d
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
