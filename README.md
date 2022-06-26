Poker Dot
=========

![Build status](https://github.com/adamnfish/pokerdot/actions/workflows/main.yml/badge.svg)

Play No Limit Texas Hold'em Poker online. It is still a
work-in-progress, but should be playable.

<img src="frontend/assets/logo.svg" width="100px">

[https://pokerdot.adamnfish.io/](https://pokerdot.adamnfish.io/)

## Licence

pokerdot is open-source and free to play for non-commercial use, but
is not licensed for commercial use.

If you would like to use pokerdot or its source-code for your business
or expect to make any money from its use, you must get in touch with
me to discuss your use.  If you will be using pokerdot for any
virtuous purpose (in particular education and community support,
charity, the climate crisis) please make sure to mention this when you
contact me.

## Development

### Structure

The application is made of a static Elm frontend and a Scala API.

#### Frontend

The [`frontend`](frontend) directory contains an Elm application for
pokerdot's web-ui.  The application is built using
[parcel](https://parceljs.org/).

#### API

The Scala API is split into multiple sub-projects.

The pokerdot API is designed to be run as a serverless lambda function
to keep running costs to a minumum.

##### core

Contains the pokerdot application itself, including all the logic for
playing poker.

##### lambda

Wraps the pokerdot application in an AWS Lambda handler, for the real
deployed version of pokerdot.

Data is persisted to AWS DynamoDB, and websocket connections are
managed by API Gateway.

##### devserver

Provides a program that wraps the core application in a local
development server.

Data is only persisted in-memory, so server restarts will lose all
application data.  The devserver does not provide hot reloading on
code changes.  Iterating on the Scala API is easier to do with tests
than by running the application and manually running it.

##### integration

This project contains end-to-end tests of the core API to make sure it
works as expected, without actually having to run the server.

### Running locally

To do UI development or to play pokerdot without deploying it to AWS,
you can run the application yourself.

You can run the frontend and the API in two separate terminals:

#### Frontend application

You will need `node` and `yarn` installed, all other dependencies will
be fetched for you.

```bash
$ cd frontend
$ yarn start
```

#### API

You will need a JDK installed (pokerdot uses 11, but others should
work fine) and `sbt`.

```bash
$ sbt
> project devServer
> run --debug rng
```

In general, running the server is:

```bash
> run [--debug] [seed|rng]
```

The `--debug` flag will ask the API to print client requests and
connection information to the terminal.

If you do not specify a seed, it will default to `0` for repeatable
games. You can specify a seed (as a `Long`) or the string `rng` to use
random seeds.

### Deploying to AWS

*Please remember that while pokerdot is open source and free to play
for non-commercial use, it is not licencsed for commercial use.*

* Have an AWS account
* Setup your DNS
* Create HTTPS certificates
* Create a cloudformation stack for [the persistent resources](cloudformation/pokerdot-storage.template.yaml)
* Create the [application cloudformation stack](cloudformation/pokerdot.template.yaml)

After creating these two cloudformation stacks pokerdot will be
running on your account.

#### DNS + certificates

You will need to set up a domain in Route53 before deploying
pokerdot's cloudformation stacks.  Subdomains will be created for the
static application (`pokerdot.[domain]`) and the API
(`pokerdot-api.[domain]`).

HTTPS certificates need to be created in Amazon Certificate Manager
for these (sub)domains.

#### Cloudformation stacks

The [`cloudformation`](cloudformation) directory contains two
cloudformation templates.

##### Persistent resources

To support multiple versions of pokerdot in the same AWS account, this
stack is parameterised by `Stage`.

This template sets up:
* S3 bucket that contains the static application
* The game's database tables
  * Games table
  * Players table

The outputs of this template are used as inputs for the application
template, so it must be created first.

##### Application

The application's cloudformation stack is also parameterised on Stage,
but needs to also be told:

* the application's domain name
* the HTTPS certificates to use for tho two application subdomains
* a "dist bucket" that is used to store application artifacts
* the outputs from the persistent resources cloudformation stack

**Note:** This cloudformation stack creates a CloudFront distribution to serve the static application.
AWS is very slow to make changes to CloudFront resources, so be patient. This may take tens of minutes to complete.
