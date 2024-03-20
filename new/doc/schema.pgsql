-- CREATE EXTENSION postgis;
-- CREATE EXTENSION address_standardizer;

CREATE TYPE KIND AS ENUM ('note', 'link');
CREATE TABLE entries (
  name TEXT NOT NULL UNIQUE,
  kind KIND NOT NULL,
  date TIMESTAMP WITH TIME ZONE,
  duration INTERVAL,
  geo GEOGRAPHY,
  text TSVECTOR,
  metadata JSONB,
  CONSTRAINT entries_ref
    UNIQUE(name,kind)
);

CREATE TABLE entries_sub (
  child TEXT NOT NULL REFERENCES entries(name),
  parent TEXT NOT NULL REFERENCES entries(name),
  CONSTRAINT entries_sub_unique
    UNIQUE(child,parent)
);

CREATE TABLE notes (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'note'),
  path TEXT NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

CREATE TABLE links (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'link'),
  protocol TEXT NOT NULL,
  ref TEXT NOT NULL,
  file TEXT NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);
