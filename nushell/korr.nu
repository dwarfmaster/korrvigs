
# Get the root to the wiki
def 'korr root' [] {
  $env.KORRVIGS_ROOT? | default "/home/luc/downloads/wiki"
}

# Get the path to the socket
def 'korr socket' [] {
  (
    $env.KORRVIGS_SOCKET? 
    | default $"($env.XDG_STATE_HOME? 
                | default "/home/luc/.local/state")/korrvigs/server.sock"
  )
}

# Run a query on the wiki
def 'korr query' [] {
  let start = $in
  let sock = $"UNIX-CONNECT:(korr socket)"
  $start | socat $sock - | lines | each { from json }
}

# Create a new entry in the wiki
def 'korr create entry' [
  instance: string # UUID of the entry describing its class
  name: string # The of the entry
] {
  let uuid = (uuidgen)
  let dir = ($uuid | korr from uuid | korr resolve dir)
  mkdir $dir
  let def = ($dir | path join "default.meta")
  $"name\(self, \"($name)\").
instance-of\(self, '($instance)')." | save $def
  { uuid: $uuid sub: null query: null name: $name }
}

def 'korr create sub' [
  file: string     # Path to the file to attach
  --sub: string    # The path of the sub
  instance: string # The UUID of the class
  name: string     # The name of the sub
] {
  let start = $in
  let sub = (if ($sub | is-empty) { $file | path basename } else { $sub })
  let entry = ($start | update sub $sub)
  let meta = ($entry | korr resolve meta)
  let dtl = ($entry | korr to datalog)
  $"name\(self, \"($name)\").
instance-of\(self, '($instance)')." | save $meta
  mv $file ($entry | korr resolve file)
  $entry | upsert name $name
}

# Create notes for entry
def 'korr create note' [] {
  let entry = $in
  let dtl = ($entry | korr to datalog)
  let entryName = ($entry | korr get name)
  let sub = ($entryName | str to-filename --suffix "_notes.md")
  let name = ([ $entryName " notes"] | str join)
  let instance = ( "query(U) :- name(U, \"Pandoc document\")." 
                 | korr query | get 0.0.uuid )
  let temp = (mktemp)
  $"---
title: ($name)
---
" | save --force $temp
  let note = ($entry | korr create sub $temp --sub $sub $instance $name)
  $entry | korr meta add $"notes\(self, self/'($sub)')."
  $note
}

# Create a korrvigs module attached to an entry
def 'korr create korr' [] {
  let entry = $in
  let dtl = ($entry | korr to datalog)
  let entryName = ($entry | korr get name)
  let sub = ($entryName | str to-filename --suffix "_rules.pl")
  let name = ([ $entryName " rules" ] | str join)
  let instance = ( "query(U) :- name(U, \"Korrvigs module\")."
                 | korr query | get 0.0.uuid )
  let temp = (mktemp)
  let kmodule = ($entry | korr create sub $temp --sub $sub $instance $name)
  $entry | korr meta add $"relation-rules\(self, self/'($sub)')."
  $kmodule
}

# List all classes
def 'korr get classes' [] {
  (
    "query(N, C) :- instance-of(T, T), instance-of(C, T), name(C, N)."
    | korr query
    | each { { Name: $in.0, Entity: $in.1.uuid } }
  )
}

# The the korrvigs types
def 'korr get types' [] {
  (
    "query(N, C) :- name(T, \"Korrvigs type\"), instance-of(C, T), name(C, N)."
    | korr query
    | each { { Name: $in.0, Entity: $in.1.uuid } }
  )
}

# Get the class of classes
def 'korr get class' [] {
  (
    "query(T) :- instance-of(T, T)."
    | korr query
    | get 0.0
    | upsert name "Ontological class"
  )
}

# Get the class of relations
def 'korr get relation' [] {
  ( "query(R) :- name(R, \"Ontology relation\"), instance-of(R, T), instance-of(T,T)."
  | korr query
  | get 0.0
  | upsert name "Ontological relation" )
}

# Get the notes of an entry
def 'korr get notes' [] {
  let entry = ($in | korr to datalog)
  $"query\(N) :- notes\('($entry)', N)." | korr query | each { get 0 }
}

def 'korr get korrs' [] {
  let entry = ($in | korr to datalog)
  $"query\(K) :- relation-rules\('($entry)', K)." | korr query | each { get 0 }
}

# Get the name of an entry
def 'korr get name' [] {
  let entry = $in
  if ($entry | get name? | is-empty) {
    let entry = ($entry | korr to datalog)
    $"query\(N) :- name\('($entry)', N)." | korr query | get 0.0
  } else {
    $entry | get name
  }
}

# Create an entry from an uuid
def 'korr from uuid' [
  --sub: string   # Set the sub of the entry
  --query: string # Set the query of the entry
] {
  { uuid: $in sub: $sub query: $query }
}

# Transform a detalog representation into the entry
def 'korr from datalog' [] {
  let dtl = $in
  let entry = ($dtl 
              | parse -r '(?P<uuid>[\d\w]{8}-[\d\w]{4}-[\d\w]{4}-[\d\w]{4}-[\d\w]{12})(/(?P<sub>[^/#]*))?(#(?P<query>[^/#]*))?'
              | get 0 | select uuid sub query)
  ( $entry
  | update sub (if ($entry.sub | is-empty) { null } else { $entry.sub })
  | update query (if ($entry.query | is-empty) { null } else { $entry.query }) )
}

# Transform an entry into its datalog representation
def 'korr to datalog' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = (if ($entry.sub | is-empty) { "" } else { $"/($entry.sub)" })
  let query = (if ($entry.query | is-empty) { "" } else { $"#($entry.query)" })
  [ $uuid $sub $query ] | str join
}

# Get path of the directory of the entry
def 'korr resolve dir' [] {
  let uuid = $in.uuid
  korr root | path join ($uuid | str substring 0..2) $uuid
}

# Get path of file from entry table
def 'korr resolve file' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = ($entry.sub | default "default.meta")
  $entry | korr resolve dir | path join $sub
}

# Get the path of the meta file associated with an entry
def 'korr resolve meta' [] {
  let entry = $in
  let uuid = $entry.uuid
  let sub = ( $entry.sub
            | default "default.meta"
            | path parse
            | update extension "meta"
            | path join )
  $entry | korr resolve dir | path join $sub
}

# Add new atom to meta file
def 'korr meta add' [
  atom: string # New atom to append
] {
  let entry = $in
  $"\n($atom)" | save --append ($entry | korr resolve meta)
  $entry
}

# Remove atom from meta file
def 'korr meta rm' [
  atom: string # Atom to remove
] {
  let entry = $in
  let meta = ($entry | korr resolve meta)
  let temp = (mktemp)
  (
    $meta | open --raw | lines
    | filter { |line| $line != $atom }
    | save --force $temp
  )
  mv $temp --force $meta
  $entry
}

def 'ui filter' [] {
  (
    each { |l| $"($l.0) ($l.1)" } | to text
    | fzf --with-nth=2..
    | parse "{key} {value}"
    | get 0
  )
}

def 'ui select' [] {
  let named = ("query(C, N) :- name(C, N)." | korr query
              | each { [ ($in.0 | korr to datalog) $in.1 ] } )
  $named | ui filter | get key | korr from datalog
}

def 'ui create' [] {
  let class = (korr get classes | each { [ $in.Entity $in.Name ] } | ui filter)
  let title = (gum input --placeholder $"Name for new ($class.value)")
  korr create entry $class.key $title
}

def 'ui create subclass' [] {
  let class = (korr get classes | each { [ $in.Entity $in.Name ] } | ui filter)
  let title = (gum input --placeholder $"Name for new subclass of ($class.value)")
  let class_class = (korr get class | get uuid)
  ( korr create entry $class_class $title
  | korr meta add $"is-a\(self, '($class.key)')." )
}

def 'ui create relation' [name: string] {
  let class_rel = (korr get relation | get uuid)
  let arity = (gum input --placeholder $"Arity for ($name)" | into int)
  let classes = ( korr get classes | append (korr get types)
                | each { [ $in.Entity $in.Name ] })
  let args = ( 
    1..($arity) 
    | each ({ |id| 
      let class = ($classes | ui filter)
      $"relation-type-at\(self, ($id), '($class.key)')."
    })
    | str join "\n"
  )
  ( korr create entry $class_rel $name 
  | korr meta add $args )
}

def 'ui annotate' [] {
  let entry = $in
  let notes = ($entry | korr get notes)
  let note = (if ($notes | is-empty) {
    $entry | korr create note
  } else {
    $notes | get 0
  });
  run-external $env.EDITOR ($note | korr resolve file)
  $note
}

def 'ui add rules' [] {
  let entry = $in
  let korrs = ($entry | korr get korrs)
  let korr = (if ($korrs | is-empty) {
    $entry | korr create korr
  } else {
    $korrs | get 0
  })
  run-external $env.EDITOR ($korr | korr resolve file)
  $korr
}

def 'ui remove' [] {
  let entry = (ui select)
  # TODO find a remove references to this entry
  if ($entry | get sub | is-empty) {
    let dir = ($entry | korr resolve dir)
    let count = ( ls $dir | select name
                | each { get name | path parse | get extension }
                | filter { |ext| $ext == "meta" } | length )
    gum confirm ([ "Will remove " $count " entries"] | str join)
    if $env.LAST_EXIT_CODE == 0 {
      rm -r --force $dir
    }
  } else {
    let file = ($entry | korr resolve file)
    let meta = ($entry | korr resolve meta)
    gum confirm ([ "Will remove " ($file | path relative-to (korr root)) ] | str join)
    if $env.LAST_EXIT_CODE == 0 {
      rm --force $file $meta
    }
  }
}

def 'ui export relations def' [] {
  (
    "query(NC, N, ND) :- relation-type-at(C, N, D), name(C, NC), name(D, ND)."
    | korr query
    | each { { rel: $in.0 pos: $in.1 type: $in.2 } }
    | group-by rel
    | items ({ |key, value| 
      let args = (
        $value
        | sort-by pos
        | get type
        | each ({ |arg|
          if $arg == "Korrvigs number" {
            "number"
          } else if $arg == "Korrvigs string" {
            "string"
          } else {
            "entry"
          }
        })
      )
      { key: $key value: $args }
    })
    | transpose -r | get 0
  )
}

def 'ui export relations summary' [] {
  (
    "query(NC, N, ND) :- relation-type-at(C, N, D), name(C, NC), name(D, ND)."
    | korr query
    | each { { rel: $in.0 pos: $in.1 type: $in.2 } }
    | group-by rel
    | items ({ |key, value| 
      let args = (
        $value
        | sort-by pos
        | get type
        | each ({ |arg|
          if $arg == "Korrvigs number" {
            $"(ansi cyan)number(ansi reset)"
          } else if $arg == "Korrvigs string" {
            $"(ansi cyan)string(ansi reset)"
          } else if $arg == "Korrvigs entity" {
            $"(ansi green)Entity(ansi reset)"
          } else {
            $"(ansi green)($arg)(ansi reset)"
          }
        })
        | str join $"(ansi purple_bold) × (ansi reset)"
      )
      { key: $key value: $"-> (ansi red_bold)($key)(ansi reset) (ansi purple_bold):(ansi reset) ($args)" }
    })
    | sort-by key | get value
    | str join "\n"
  )
}

def 'ui export relations rules' [] {
  (
    "query(R) :- relation-rules(C, R)."
    | korr query
    | each { get 0 | korr resolve file | open | lines }
    | flatten
  )
}

# Convert string to valid filename
def 'str to-filename' [
  --suffix: string # Suffix to add to generated string
] {
  let r = ( $in 
          | iconv -f utf-8 -t ascii//TRANSLIT 
          | tr -d '?' | str snake-case )
  [ $r $suffix ] | str join
}

# Run a query on the server
def 'main query' [
  --file: string # Path to the query file, if absent read from stdin
] {
  let start = $in
  let sock = $"UNIX-CONNECT:(korr socket)"
  if ($file | is-empty) {
    $start | socat $sock -
  } else {
    open $file | socat $sock -
  }
}

# Find an entry by its name
def 'main find' [] {
  ui select | to json
}

# Create a new entry
def 'main create' [
  --annot: bool # Annotate the newly created entry
] {
  let entry = (ui create)
  if $annot {
    $entry | ui annotate
  }
  $entry | to json
}

# Create a subclass
def 'main create subclass' [
  --annot: bool # Annotate the newly created entry
] {
  let entry = (ui create subclass)
  if $annot {
    $entry | ui annotate
  }
  $entry | to json
}

# Create a relation
def 'main create relation' [
  name: string  # The name of the relation to create
  --annot: bool # Annotate the relation
  --rules: bool # Add rules to the relation
] {
  let entry = (ui create relation $name)
  if $annot {
    $entry | ui annotate
  }
  if $rules {
    $entry | ui add rules
  }
  $entry | to json
}

# Export the class tree in graphviz format
def 'main export classes tree' [] {
  ( "query(A, B) :- is-a(U, V), name(U, A), name(V, B)."
  | korr query 
  | each { |in| $"  \"($in.0)\" -> \"($in.1)\";" }
  | prepend "digraph {" | append "}"
  | to text )
}

# Export the types of the relations
def 'main export relations def' [] {
  ui export relations def | to json -r
}

# Export rules of the relations
def 'main export relations rules' [] {
  ui export relations rules | to json -r
}

# Print summary of relations
def 'main export relations summary' [] {
  ui export relations summary | print
}

# Create notes for an entry
def 'main annotate' [] {
  ui select | ui annotate | to json
}

# Add rules for a relation
def 'main add rules' [] {
  let rels = (korr get relation)
  (
    $"query\(R, N) :- instance-of\(R, '($rels | korr to datalog)'), name\(R, N)."
    | korr query
    | each { [ ($in.0 | korr to datalog) $in.1 ] }
    | ui filter | get key
    | korr from datalog
    | ui add rules
    | to json
  )
}

def 'main remove' [] {
  ui remove
}

def main [] {
  print "Use subcommand (--help to list them)"
}

