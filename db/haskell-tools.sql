CREATE DATABASE haskell_tools;

\c haskell_tools

-- Private data structures

CREATE TABLE public.packages (
  package_name text,
  version text NOT NULL,
  license text NOT NULL,
  description text NOT NULL,
  category text NOT NULL,
  homepage text NOT NULL,
  package_url text NOT NULL,
  repo_type text,
  repo_location text,
  PRIMARY KEY (package_name)
);

CREATE TABLE public.repos (
  package_name text PRIMARY KEY REFERENCES packages,
  stars integer NOT NULL DEFAULT 0,
  forks integer NOT NULL DEFAULT 0,
  collaborators integer NOT NULL DEFAULT 1
);

CREATE TABLE public.dependencies (
  dependent text REFERENCES public.packages (package_name),
  dependency text REFERENCES public.packages (package_name),
  version_range text,
  PRIMARY KEY (dependent, dependency)
);

CREATE TABLE public.extensions (
  package_name text REFERENCES public.packages (package_name),
  extension text,
  PRIMARY KEY (package_name, extension)
);

CREATE VIEW public.package_repos AS
SELECT
  p.package_name,
  r[1] as owner,
  r[2] as repo
FROM
  packages p,
  regexp_matches(repo_location, 'github.com[:\/]([^\/]*)\/([^\. ]*)') r
WHERE
  repo_location ~* 'github';

CREATE OR REPLACE VIEW public.categories AS
SELECT
  p.package_name,
  initcap(btrim(translate(c.c, chr(10) || chr(13), ''))) AS category_name
FROM
  packages p,
  LATERAL regexp_split_to_table(p.category, ','::text) c(c)
WHERE
  btrim(c.c) <> ''::text;

-- API exposed through PostgREST
CREATE SCHEMA api;

CREATE VIEW api.top_repos AS
SELECT * FROM repos ORDER BY (stars * forks) DESC;

CREATE USER postgrest PASSWORD :password;
CREATE ROLE anonymous;
GRANT anonymous TO postgrest;
GRANT USAGE ON SCHEMA api TO anonymous;
GRANT SELECT ON api.top_repos TO anonymous;
