CREATE EXTENSION IF NOT EXISTS citext;

CREATE TABLE IF NOT EXISTS users (
    id serial PRIMARY KEY,
    name text NOT NULL,
    LOGIN citext NOT NULL UNIQUE,
    password_hash text NOT NULL,
    password_salt text NOT NULL,
    registration_date timestamp with time zone NOT NULL DEFAULT NOW(),
    is_admin bool NOT NULL,
    is_allowed_to_post bool NOT NULL
);

CREATE TABLE IF NOT EXISTS images (
    id serial PRIMARY KEY,
    file_extension text NOT NULL,
    content bytea NOT NULL
);

CREATE TABLE IF NOT EXISTS categories (
    name citext PRIMARY KEY,
    parent_category citext REFERENCES categories (name) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS articles (
    id serial PRIMARY KEY,
    title text NOT NULL,
    created_at timestampt with time zone NOT NULL DEFAULT NOW(),
    author integer REFERENCES users (id) ON DELETE CASCADE NOT NULL,
    category citext REFERENCES categories (name) ON DELETE SET NULL,
    body text NOT NULL,
    is_published bool NOT NULL
);

CREATE TABLE IF NOT EXISTS articles_images (
    article_id integer REFERENCES articles (id) ON DELETE CASCADE,
    image_id integer REFERENCES images (id) ON DELETE CASCADE,
    PRIMARY KEY (article_id, image_id)
);

