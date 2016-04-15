CREATE DATABASE haskell_tools;

\c haskell_tools

-- Private data structures

CREATE TABLE public.repos (
  name text,
  owner text,
  stars integer,
  forks integer,
  PRIMARY KEY (name, owner)
);

CREATE TABLE public.packages (
  name text,
  version text NOT NULL,
  license text NOT NULL,
  description text NOT NULL,
  category text NOT NULL,
  homepage text NOT NULL,
  package_url text NOT NULL,
  repo_type text,
  repo_location text,
  PRIMARY KEY (name)
);

-- API exposed through PostgREST
CREATE SCHEMA api;

CREATE VIEW api.top_repos AS
SELECT * FROM repos ORDER BY (watchers * forks) DESC;

CREATE USER postgrest PASSWORD :password;
CREATE ROLE anonymous;
GRANT anonymous TO postgrest;
GRANT USAGE ON SCHEMA api TO anonymous;
GRANT SELECT ON api.top_repos TO anonymous;
