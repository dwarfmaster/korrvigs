
create table if not exists classes (
  class_name text not null primary key,
  parent_class text references classes on delete cascade
);

create table if not exists entries (
  entry_id uuid not null primary key,
  entry_name text not null unique,
  entry_notes text not null
);

create table if not exists entities (
  entity_id serial not null primary key,
  entity_class text not null references classes on delete restrict,
  entity_uuid uuid not null references entries on delete cascade,
  entity_sub text,
  entity_query text,
  constraint entity_unique_uuidsubclass
    unique(entity_uuid, entity_sub, entity_query)
);

create table if not exists class_entry (
  class_entry_class text not null references classes,
  class_entry_entry uuid not null references entries
);
