ALTER TABLE IF EXISTS users
    ALTER COLUMN password_hash TYPE bytea
    USING password_hash::bytea,
    ALTER COLUMN password_salt TYPE bytea
    USING password_salt::bytea,
    ALTER COLUMN is_admin SET DEFAULT FALSE,
    ALTER COLUMN is_allowed_to_post SET DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS password_hash_iterations integer;

