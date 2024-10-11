CREATE TYPE KIND AS ENUM ('note', 'link', 'file', 'event');
CREATE TABLE entries (
  name TEXT NOT NULL UNIQUE,
  kind KIND NOT NULL,
  date TIMESTAMP WITH TIME ZONE,
  duration INTERVAL,
  geo GEOGRAPHY,
  text TSVECTOR,
  CONSTRAINT entries_ref
    UNIQUE(name,kind)
);

CREATE TABLE entries_metadata (
  name TEXT NOT NULL REFERENCES entries(name),
  key TEXT NOT NULL,
  value JSONB NOT NULL,
  read_only BOOL NOT NULL,
  CONSTRAINT entries_metadata_ref
    UNIQUE(name,key)
);

CREATE TABLE entries_sub (
  child TEXT NOT NULL REFERENCES entries(name),
  parent TEXT NOT NULL REFERENCES entries(name),
  CONSTRAINT entries_sub_unique
    UNIQUE(child,parent)
);

CREATE TABLE entries_ref_to (
  referer TEXT NOT NULL REFERENCES entries(name),
  referee TEXT NOT NULL REFERENCES entries(name),
  CONSTRAINT entries_ref_unique
    UNIQUE(referer,referee)
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

CREATE TYPE FILESTATUS AS ENUM ('fileplain', 'filepresent', 'fileabsent');
CREATE TABLE files (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'file'),
  path TEXT NOT NULL,
  meta TEXT NOT NULL,
  status FILESTATUS NOT NULL,
  mime TEXT NOT NULL,
  CONSTRAINT files_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);

CREATE TABLE events (
  name TEXT NOT NULL PRIMARY KEY,
  kind KIND NOT NULL CHECK(kind = 'event'),
  calendar TEXT NOT NULL,
  file TEXT NOT NULL,
  uid TEXT NOT NULL,
  CONSTRAINT events_entries
    FOREIGN KEY (name,kind) references entries(name,kind)
);
