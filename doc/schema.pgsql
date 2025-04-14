DO $$ BEGIN
  CREATE TYPE KIND AS ENUM ('note', 'link', 'file', 'event', 'calendar');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS entries (
  name TEXT NOT NULL UNIQUE,
  kind KIND NOT NULL,
  date TIMESTAMP WITH TIME ZONE,
  duration INTERVAL,
  geo GEOGRAPHY,
  text TSVECTOR,
  CONSTRAINT entries_ref
    UNIQUE(name,kind)
);

CREATE TABLE IF NOT EXISTS entries_metadata (
  name TEXT NOT NULL REFERENCES entries(name),
  key TEXT NOT NULL,
  value JSONB NOT NULL,
  CONSTRAINT entries_metadata_ref
    UNIQUE(name,key)
);

CREATE TABLE IF NOT EXISTS entries_sub (
  child TEXT NOT NULL REFERENCES entries(name),
  parent TEXT NOT NULL REFERENCES entries(name),
  CONSTRAINT entries_sub_unique
    UNIQUE(child,parent)
);

CREATE TABLE IF NOT EXISTS entries_ref_to (
  referer TEXT NOT NULL REFERENCES entries(name),
  referee TEXT NOT NULL REFERENCES entries(name),
  CONSTRAINT entries_ref_unique
    UNIQUE(referer,referee)
);

CREATE TABLE IF NOT EXISTS computations (
  entry TEXT NOT NULL REFERENCES entries(name),
  name TEXT NOT NULL,
  action JSONB NOT NULL,
  CONSTRAINT computations_ref_unique
    UNIQUE(entry,name)
);

CREATE TABLE IF NOT EXISTS notes (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'note'),
  path TEXT NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

CREATE TABLE IF NOT EXISTS links (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'link'),
  protocol TEXT NOT NULL,
  ref TEXT NOT NULL,
  file TEXT NOT NULL,
  CONSTRAINT notes_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

DO $$ BEGIN
  CREATE TYPE FILESTATUS AS ENUM ('fileplain', 'filepresent', 'fileabsent');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS files (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'file'),
  path TEXT NOT NULL,
  meta TEXT NOT NULL,
  status FILESTATUS NOT NULL,
  mime TEXT NOT NULL,
  CONSTRAINT files_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

CREATE TABLE IF NOT EXISTS events (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'event'),
  calendar TEXT NOT NULL,
  file TEXT NOT NULL,
  uid TEXT NOT NULL,
  CONSTRAINT events_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

CREATE TABLE IF NOT EXISTS calendars (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'calendar'),
  server TEXT NOT NULL,
  usr TEXT NOT NULL,
  calname TEXT NOT NULL,
  CONSTRAINT calendars_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);
