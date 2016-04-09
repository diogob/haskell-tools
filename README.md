# Haskell Tools

This is a tool for collecting github data to feed the Haskell Tools website backend.
In the folder frontend there is also a Elm frontend to present the data.

## Setting up the backend

First make sure you have the requirements to build:
 * [PostgreSQL](http://www.postgresql.org) (with libpq develeopment files)
 * [Stack](https://www.stackage.org) - to build the data collector.
 * [PostgREST](http://postgrest.com) - to run the REST api server.

Then follow the steps:
  1. First setup the database (the double and single-quotes sequence is not a typo)
     
        psql -v password="'password_for_postgrest_user'" postgres < db/haskell-tools.sql
         
  2. Compile the data collector
  
        stack setup
        stack build

  3. Now to collect some data from Github repos use the command `haskell-tools-load` replacing DBUSER, DBPASS and DBHOST for the approprite values for the target database:
     
        stack exec haskell-tools-load "postgres://DBUSER:DBPAS@DBHOST/haskell_tools"

  4. Start the API server using the password from step 1:

        postgrest postgres://postgrest:password_for_postgrest_user@localhost/haskell_tools -a anonymous

This will open the API server in the port 3000.
Having done this the frontend is compiled to connect on localhost:3000 by default, so compiling the elm application and opening it in a browser should work.
