
use ui.nu
use korr.nu

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
  ui fuzzy select | to json
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
  ui fuzzy select | ui annotate | to json
}

# Add rules for a relation
def 'main add rules' [] {
  let rels = (korr query relation)
  (
    $"query\(R, N) :- instance-of\(R, '($rels | korr to datalog)'), name\(R, N)."
    | korr query
    | each { [ ($in.0 | korr to datalog) $in.1 ] }
    | ui fuzzy filter | get key
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

