
use ui.nu
use korr.nu
use notes.nu
use ontology.nu
use date.nu
use files.nu
use person.nu

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

# Create a format
def 'main create format' [
  --annot: bool # Annotate the relation
  --choose: bool # Use a more specific class than Data Format Specification
] {
  let entry = (if $choose {
    (files ui create format --choose)
  } else {
    (files ui create format)
  })
  if $annot {
    $entry | notes attach
  }
  $entry | to json
}

# Add a new person
def 'main create person' [
  --annot: bool # Annotate the person
] {
  let name = (gum input --placeholder "Person's name")
  let entry = ($name | person create person)
  if $annot {
    $entry | notes attach
  }
  $entry | to json
}

# Add a new name to a person
def 'main person add name' [] {
  person add name | to json
}

# Set the birth date of someone
def 'main person set birth' [] {
  person set birth | to json
}

# Set the death date of someone
def 'main person set death' [] {
  person set death | to json
}

# Find a person by name
def 'main person find' [] {
  (
    person resolve persons
    | each { |p| [ ($p.person | korr to datalog) $p.name ] }
    | ui fuzzy filter
    | get key
    | korr from datalog
    | to json
  )
}

# Annotate a person
def 'main person annotate' [] {
  let person = (
    person resolve persons
    | each { |p| [ ($p.person | korr to datalog) $p.name ] }
    | ui fuzzy filter
  )
  let person = ($person.key | korr from datalog | insert name $person.value)
  (
    $person
    | notes attach
    | to json
  )
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

# Add a date to a timepoint
def 'main date point' [] {
  date attach date
}

def 'main remove' [] {
  ui remove
}

def main [] {
  print "Use subcommand (--help to list them)"
}

