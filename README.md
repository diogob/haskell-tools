# Haskell Tools

This is a tool for collecting github data to feed the Haskell Tools website backend.
In the folder frontend there is also a Elm frontend to present the data.

## Setting up the backend

First make sure you have the requirements to build:
 * [PostgreSQL](http://www.postgresql.org) (with libpq develeopment files)
 * [Stack](https://www.stackage.org) - to build the data collector.
 * [PostgREST](http://postgrest.com) - to run the REST api server.

Then follow the steps:
  * First setup the database (the double and single-quotes sequence is not a typo)
```
psql -v password="'password_for_postgrest_user'" postgres < db/haskell-tools.sql
```
  * Compile the data collector
```
stack setup
stack build
```

TODO:
  * Collect some github data
  * Start the API server

