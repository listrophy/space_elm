# SpaceElm

A multiplayer, cooperative space shooter game written in Elm and Rails.

## Prerequisites

You'll need Bundler and Elm (installed as indicated below by `npm` or some other way... see [elm-lang.org](http://elm-lang.org/)). You should have Redis up and running, but you can change `config/cable.yml` to use the `async` adapter for development.

## Setup

    $ git clone git@github.com:listrophy/space_elm
    $ cd space_elm
    $ gem install bundler
    $ bundle
    $ npm install -g elm
    $ npm install elm-live
    $ rake db:create
    $ rake db:migrate

In one terminal, run:

    $ elm-live elm/Main.elm --output=public/elm.js

In another terminal, run:

    $ rails s

## License

See the LICENSE file in this repository.
