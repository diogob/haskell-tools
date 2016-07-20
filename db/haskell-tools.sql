CREATE DATABASE haskell_tools;

\c haskell_tools
CREATE SCHEMA private;
SET search_path TO private, public;
ALTER DATABASE haskell_tools SET search_path TO public, private;

CREATE EXTENSION unaccent;
CREATE EXTENSION pg_trgm;
-- Private data structures

CREATE TABLE private.packages (
  package_name text PRIMARY KEY,
  version text NOT NULL,
  license text NOT NULL,
  description text NOT NULL,
  category text NOT NULL,
  homepage text NOT NULL,
  package_url text NOT NULL,
  repo_type text,
  repo_location text,
  created_at timestamp NOT NULL default current_timestamp,
  updated_at timestamp NOT NULL default current_timestamp
);

CREATE TABLE private.repos (
  package_name text PRIMARY KEY REFERENCES packages,
  stars integer NOT NULL DEFAULT 0,
  forks integer NOT NULL DEFAULT 0,
  collaborators integer NOT NULL DEFAULT 1,
  created_at timestamp NOT NULL default current_timestamp,
  updated_at timestamp NOT NULL default current_timestamp
);

CREATE TABLE private.dependencies (
  dependent text REFERENCES private.packages (package_name),
  dependency text REFERENCES private.packages (package_name),
  version_range text,
  created_at timestamp NOT NULL default current_timestamp,
  updated_at timestamp NOT NULL default current_timestamp,
  PRIMARY KEY (dependent, dependency)
);

CREATE TABLE private.extensions (
  package_name text REFERENCES private.packages (package_name),
  extension text,
  created_at timestamp NOT NULL default current_timestamp,
  updated_at timestamp NOT NULL default current_timestamp,
  PRIMARY KEY (package_name, extension)
);

-- Triggers to update updated_at columns
CREATE FUNCTION private.update_updated_at()
RETURNS TRIGGER
LANGUAGE 'plpgsql'
AS $$
BEGIN
  NEW.updated_at := current_timestamp;
  RETURN NEW;
END;
$$;

CREATE TRIGGER update_updated_at
BEFORE UPDATE ON private.packages
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at();

CREATE TRIGGER update_updated_at
BEFORE UPDATE ON private.dependencies
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at();

CREATE TRIGGER update_updated_at
BEFORE UPDATE ON private.extensions
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at();

CREATE TRIGGER update_updated_at
BEFORE UPDATE ON private.repos
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at();

CREATE OR REPLACE VIEW private.package_repos AS
SELECT
  p.package_name,
  r[1] as owner,
  r[2] as repo,
  p.updated_at
FROM
  packages p,
  regexp_matches(repo_location, 'github.com[:\/]([^\/]*)\/([^\. /]*)') r
WHERE
  repo_location ~* 'github';

-- Trigger to delegate update to packages
CREATE OR REPLACE FUNCTION private.update_package_updated_at()
RETURNS TRIGGER
LANGUAGE 'plpgsql'
AS $$
BEGIN
  UPDATE private.packages SET updated_at = NEW.updated_at WHERE package_name = NEW.package_name;
  RETURN NEW;
END;
$$;

CREATE TRIGGER update_package_updated_at
INSTEAD OF UPDATE ON private.package_repos
FOR EACH ROW
EXECUTE PROCEDURE update_package_updated_at();


CREATE OR REPLACE VIEW private.categories AS
SELECT
  p.package_name,
  initcap(btrim(translate(c.c, chr(10) || chr(13), ''))) AS category_name
FROM
  packages p,
  LATERAL regexp_split_to_table(p.category, ','::text) c(c)
WHERE
  btrim(c.c) <> ''::text;

CREATE MATERIALIZED VIEW private.package_totals AS
WITH RECURSIVE dependency_tree AS (
  SELECT package_name, package_name as parent, 1 as deps FROM private.packages
  UNION
  SELECT t.package_name, d.dependency as parent, 1 as deps FROM private.dependencies d JOIN dependency_tree t ON d.dependent = t.parent
),
dependent_tree AS (
  SELECT package_name, package_name as parent, 0 as deps FROM private.packages
  UNION
  SELECT t.package_name, d.dependent as parent, 1 as deps FROM private.dependencies d JOIN dependent_tree t ON d.dependency = t.parent
),
totals AS (
  SELECT package_name, ty.sum as all_dependencies, tt.sum as all_dependents
  FROM
  (SELECT package_name, sum(deps) FROM dependency_tree GROUP BY package_name) ty
  JOIN
  (SELECT package_name, sum(deps) FROM dependent_tree GROUP BY package_name) tt
  USING (package_name)
)
SELECT
  package_name,
  (
  SELECT coalesce(json_agg(DISTINCT e.extension), '[]')
  FROM private.extensions e
  WHERE e.extension IS NOT NULL AND e.package_name = t.package_name
  ) AS extensions,
  (
  SELECT coalesce(json_agg(d.dependency), '[]')
  FROM private.dependencies d
  WHERE d.dependency IS NOT NULL AND d.dependent = t.package_name
  ) AS dependencies,
  (
  SELECT coalesce(json_agg(d.dependent), '[]')
  FROM private.dependencies d
  WHERE d.dependent IS NOT NULL AND d.dependency = t.package_name
  ) AS dependents,
  all_dependencies,
  all_dependents,
  all_dependents / all_dependencies::numeric as ratio
FROM
  totals t;

CREATE INDEX ON private.package_totals (package_name);
CREATE INDEX ON private.package_totals (package_name) WHERE ratio > 1;
CREATE INDEX ON private.package_totals (package_name) WHERE ratio < 1;

-- API exposed through PostgREST
CREATE OR REPLACE VIEW public.packages AS
SELECT
  p.package_name,
  p.version,
  p.license,
  p.description,
  p.category,
  p.homepage,
  p.package_url,
  p.repo_type,
  p.repo_location,
  r.stars,
  r.forks,
  r.collaborators,
  t.extensions,
  t.dependencies,
  t.dependents,
  -- when querying created at we usually want to know when it first got into our database
  LEAST(p.created_at, r.created_at) as created_at,
  -- when querying updated at we usually want to know when it was last updated
  GREATEST(p.updated_at, r.updated_at) as updated_at,
  t.all_dependencies,
  t.all_dependents,
  t.ratio
FROM
  private.packages p
  JOIN private.repos r USING (package_name)
  JOIN private.package_totals t USING (package_name);

CREATE OR REPLACE FUNCTION public.package_search(query text)
   RETURNS SETOF public.packages
   LANGUAGE sql
   STABLE
  AS $function$
  SELECT
      p.*
  FROM
      public.packages p
  WHERE
      private.to_tsvector(p.description) @@ plainto_tsquery(private.unaccent(query))
      OR
      p.package_name % query
  ORDER BY
      ts_rank( setweight(to_tsvector(p.package_name), 'A') || setweight(to_tsvector(p.description), 'B')
             , plainto_tsquery(unaccent(query))
             ) DESC;
  $function$;

CREATE OR REPLACE VIEW public.extensions AS
SELECT
  extension,
  count(distinct package_name) AS packages
FROM
  private.extensions
GROUP BY
  extension;

CREATE USER postgrest PASSWORD :password;
CREATE ROLE anonymous;
GRANT anonymous TO postgrest;
GRANT EXECUTE ON FUNCTION public.package_search(text) TO anonymous;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO anonymous;
