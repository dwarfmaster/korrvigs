
use utils.nu

# Get the root to the wiki
export def 'root' [] {
  $env.KORRVIGS_ROOT? | default "/home/luc/downloads/wiki"
}

# Get the path to the socket
export def 'socket' [] {
  (
    $env.KORRVIGS_SOCKET? 
    | default $"($env.XDG_STATE_HOME? 
                | default "/home/luc/.local/state")/korrvigs/server.sock"
  )
}

# Run a query on the wiki
export def 'query' [] {
  let start = $in
  let sock = $"UNIX-CONNECT:(socket)"
  let r = (["QUERY\n", $start] | str join | socat $sock -)
  $r | lines | skip 1 | each { from json }
}

# Create a new entry in the wiki
export def 'create entry' [
  instance: string # UUID of the entry describing its class
  name: string # The of the entry
] {
  let uuid = (uuidgen)
  let dir = ($uuid | from uuid | resolve dir)
  mkdir $dir
  let def = ($dir | path join "default.meta")
  $"name, self, \"($name)\"
instance-of, self, '($instance)'" | save $def
  { uuid: $uuid sub: null query: null name: $name }
}

export def 'create sub' [
  file: string     # Path to the file to attach
  --sub: string    # The path of the sub
  instance: string # The UUID of the class
  name: string     # The name of the sub
] {
  let start = $in
  let sub = (if ($sub | is-empty) { $file | path basename } else { $sub })
  let entry = ($start | update sub $sub)
  let meta = ($entry | resolve meta)
  let dtl = ($entry | to datalog)
  $"name, self, \"($name)\"
instance-of, self, '($instance)'" | save $meta
  mv $file ($entry | resolve file)
  $entry | upsert name $name
}

# Get the name of an entry
export def 'query name' [] {
  let entry = $in
  if ($entry | get name? | is-empty) {
    let entry = ($entry | to datalog)
    $"query\(N) :- name\('($entry)', N)." | query | get 0.0
  } else {
    $entry | get name
  }
}

# Create an entry from an uuid
export def 'from uuid' [
  --sub: string   # Set the sub of the entry
  --query: string # Set the query of the entry
] {
  { uuid: $in sub: $sub query: $query }
}

# Transform a detalog representation into the entry
export def 'from datalog' [] {
  let dtl = $in
  let entry = ($dtl 
              | parse -r '(?P<uuid>[\d\w]{8}-[\d\w]{4}-[\d\w]{4}-[\d\w]{4}-[\d\w]{12})(/(?P<sub>[^/#]*))?(#(?P<query>[^/#]*))?'
              | get 0 | select uuid sub query)
  ( $entry
  | update sub (if ($entry.sub | is-empty) { null } else { $entry.sub })
  | update query (if ($entry.query | is-empty) { null } else { $entry.query }) )
}

# Transform an entry into its datalog representation
export def 'to datalog' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = (if ($entry.sub | is-empty) { "" } else { $"/($entry.sub)" })
  let query = (if ($entry.query | is-empty) { "" } else { $"#($entry.query)" })
  [ $uuid $sub $query ] | str join
}

# Get path of the directory of the entry
export def 'resolve dir' [] {
  let uuid = $in.uuid
  root | path join ($uuid | str substring 0..2) $uuid
}

# Get path of file from entry table
export def 'resolve file' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = ($entry.sub | default "default.meta")
  $entry | resolve dir | path join $sub
}

# Get the path of the meta file associated with an entry
export def 'resolve meta' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = ( $entry.sub
            | default "default.meta"
            | path parse
            | update extension "meta"
            | path join )
  $entry | resolve dir | path join $sub
}

# Find a class by name
export def 'resolve class' [] {
  let name = $in
  (
    $"query\(C) :- name\(C, \"($name)\"), instance-of\(C, I), instance-of\(I, I)."
    | query
    | get 0.0
  )
}

# Add new atom to meta file
export def 'meta add' [
  atom: string # New atom to append
] {
  let entry = $in
  $"\n($atom)" | save --append ($entry | resolve meta)
  $entry
}

# Remove atom from meta file
export def 'meta rm' [
  atom: string # Atom to remove
] {
  let entry = $in
  let meta = ($entry | resolve meta)
  let temp = (mktemp)
  (
    $meta | open --raw | lines
    | filter { |line| $line != $atom }
    | save --force $temp
  )
  mv $temp --force $meta
  $entry
}
