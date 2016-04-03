CREATE DATABASE haskell_tools;

\c haskell_tools

-- Private data structures

CREATE TABLE public.repos (
  name text,
  owner text,
  url text,
  watchers integer,
  forks integer,
  PRIMARY KEY (name, owner)
);

-- API exposed through PostgREST
CREATE SCHEMA api;

CREATE VIEW api.top_repos AS
SELECT * FROM repos ORDER BY (watchers * forks) DESC;

CREATE ROLE postgrest;
CREATE ROLE anonymous;
GRANT anonymous TO postgrest;
GRANT USAGE ON SCHEMA api TO anonymous;
GRANT SELECT ON api.top_repos TO anonymous;
