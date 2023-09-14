
use ui.nu
use korr.nu
use notes.nu
use ontology.nu

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
  let entry = (ontology create entry)
  if $annot {
    $entry | notes attach
  }
  $entry | to json
}

# Create a subclass
def 'main create subclass' [
  --annot: bool # Annotate the newly created entry
] {
  let entry = (ontology create subclass)
  if $annot {
    $entry | notes attach
  }
  $entry | to json
}

# Create a relation
def 'main create relation' [
  name: string  # The name of the relation to create
  --annot: bool # Annotate the relation
  --rules: bool # Add rules to the relation
] {
  let entry = (ontology create relation $name)
  if $annot {
    $entry | notes attach
  }
  if $rules {
    $entry | ontology edit rules
  }
  $entry | to json
}

# Export the class tree in graphviz format
def 'main export classes tree' [] {
  ontology export classes tree
}

# Export the class tree with relations in graphviz format
def 'main export classes graph' [] {
  ontology export classes graph
}

# Export the types of the relations
def 'main export relations def' [] {
  ontology export relations def | to json -r
}

# Export rules of the relations
def 'main export relations rules' [] {
  ontology export relations rules | to json -r
}

# Print summary of relations
def 'main export relations summary' [] {
  ontology export relations summary | print
}

# Create notes for an entry
def 'main annotate' [] {
  ui fuzzy select | notes attach | to json
}

# Add rules for a relation
def 'main add rules' [] {
  ontology create rules | to json
}

def 'main remove' [] {
  ui remove
}

def main [] {
  print "Use subcommand (--help to list them)"
}

