CREATE OR REPLACE FUNCTION extensionToMimeType (ext text)
    RETURNS text
    AS $mime_type$
BEGIN
    RETURN 'image/' || CASE WHEN ext = 'jpg' THEN
        'jpeg'
    ELSE
        ext
    END;
END;
$mime_type$
LANGUAGE plpgsql;

ALTER TABLE IF EXISTS images
    ADD COLUMN IF NOT EXISTS name text,
    ADD COLUMN IF NOT EXISTS mime_type text;

UPDATE
    images
SET
    name = id::text
WHERE (name IS NULL);

UPDATE
    images
SET
    mime_type = extensionToMimeType (file_extension)
WHERE (mime_type IS NULL);

ALTER TABLE IF EXISTS images
    ALTER COLUMN name SET NOT NULL,
    ALTER COLUMN mime_type SET NOT NULL;

