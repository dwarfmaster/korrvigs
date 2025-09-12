DO $$ BEGIN
  CREATE TYPE KIND AS ENUM ('note', 'link', 'file', 'event', 'calendar');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS entries (
  id SERIAL PRIMARY KEY,
  kind KIND NOT NULL,
  name TEXT NOT NULL UNIQUE,
  date TIMESTAMP WITH TIME ZONE,
  duration INTERVAL,
  geo GEOGRAPHY,
  text TSVECTOR,
  title TEXT,
  CONSTRAINT entries_ref
    UNIQUE(id,kind)
);

CREATE TABLE IF NOT EXISTS entries_metadata (
  entry INTEGER NOT NULL REFERENCES entries(id),
  key TEXT NOT NULL,
  value JSONB NOT NULL,
  CONSTRAINT entries_metadata_ref
    UNIQUE(entry,key)
);

CREATE TABLE IF NOT EXISTS entries_sub (
  child INTEGER NOT NULL REFERENCES entries(id),
  parent INTEGER NOT NULL REFERENCES entries(id),
  CONSTRAINT entries_sub_unique
    UNIQUE(child,parent)
);

CREATE TABLE IF NOT EXISTS entries_ref_to (
  referer INTEGER NOT NULL REFERENCES entries(id),
  referee INTEGER NOT NULL REFERENCES entries(id),
  CONSTRAINT entries_ref_unique
    UNIQUE(referer,referee)
);

CREATE TABLE IF NOT EXISTS computations (
  entry INTEGER NOT NULL REFERENCES entries(id),
  name TEXT NOT NULL,
  action JSONB NOT NULL,
  CONSTRAINT computations_ref_unique
    UNIQUE(entry,name)
);

CREATE TABLE IF NOT EXISTS notes (
  id INTEGER NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'note'),
  path TEXT NOT NULL,
  collections TEXT[] NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (id,kind) references entries(id,kind)
);

CREATE TABLE IF NOT EXISTS notes_collections (
  id INTEGER NOT NULL REFERENCES notes(id),
  name TEXT NOT NULL,
  entry TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS links (
  id INTEGER NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'link'),
  protocol TEXT NOT NULL,
  ref TEXT NOT NULL,
  file TEXT NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (id,kind) references entries(id,kind)
);

DO $$ BEGIN
  CREATE TYPE FILESTATUS AS ENUM ('fileplain', 'filepresent', 'fileabsent');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS files (
  id INTEGER NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'file'),
  path TEXT NOT NULL,
  meta TEXT NOT NULL,
  status FILESTATUS NOT NULL,
  mime TEXT NOT NULL,
  CONSTRAINT files_entries
    FOREIGN KEY (id,kind) references entries(id,kind)
);

CREATE TABLE IF NOT EXISTS events (
  id INTEGER NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'event'),
  calendar TEXT NOT NULL,
  file TEXT NOT NULL,
  uid TEXT NOT NULL,
  CONSTRAINT events_entries
    FOREIGN KEY (id,kind) references entries(id,kind)
);

CREATE TABLE IF NOT EXISTS calendars (
  id INTEGER NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'calendar'),
  server TEXT NOT NULL,
  usr TEXT NOT NULL,
  calname TEXT NOT NULL,
  CONSTRAINT calendars_entries
    FOREIGN KEY (id,kind) references entries(id,kind)
);
