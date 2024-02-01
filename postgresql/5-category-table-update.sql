ALTER TABLE IF EXISTS categories
    ADD COLUMN IF NOT EXISTS id serial;

ALTER TABLE IF EXISTS articles
    DROP CONSTRAINT articles_category_fkey CASCADE,
    ADD COLUMN IF NOT EXISTS category_id integer;

ALTER TABLE IF EXISTS categories
    DROP CONSTRAINT categories_parent_category_fkey CASCADE,
    DROP CONSTRAINT categories_pkey CASCADE,
    ADD COLUMN IF NOT EXISTS parent_category_id integer;

UPDATE
    categories AS C
SET
    parent_category_id = (
        SELECT
            id
        FROM
            categories
        WHERE
            name = C.parent_category);

ALTER TABLE IF EXISTS categories
    ADD PRIMARY KEY (id),
    ALTER COLUMN name SET NOT NULL;

UPDATE
    articles AS A
SET
    category_id = (
        SELECT
            id
        FROM
            categories
        WHERE
            name = A.category);

ALTER TABLE IF EXISTS articles
    DROP COLUMN IF EXISTS category;

ALTER TABLE IF EXISTS articles RENAME COLUMN category_id TO category;

ALTER TABLE IF EXISTS articles
    ADD CONSTRAINT articles_category_fkey FOREIGN KEY (category) REFERENCES categories (id) ON DELETE SET NULL;

ALTER TABLE IF EXISTS categories
    DROP COLUMN IF EXISTS parent_category;

ALTER TABLE IF EXISTS categories RENAME COLUMN parent_category_id TO parent_category;

ALTER TABLE IF EXISTS categories
    ADD CONSTRAINT categories_parent_category_fkey FOREIGN KEY (parent_category) REFERENCES categories (id) ON DELETE SET NULL;

ALTER TABLE categories
    ADD CONSTRAINT categories_name_key UNIQUE (name);

