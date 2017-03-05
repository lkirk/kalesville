CREATE TABLE IF NOT EXISTS "recipes" (
       "id"          SERIAL,
       "title"       VARCHAR(256),
       "ingredients" TEXT,
       "procedures"  TEXT
);

CREATE TABLE IF NOT EXISTS "user_comments" (
       "id"     SERIAL,
       "author" VARCHAR(100),
       "text"   TEXT
);
