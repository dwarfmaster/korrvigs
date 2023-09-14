
use utils.nu
use korr.nu
use ui.nu

# Create a korrvigs module attached to an entry
export def 'create module' [] {
  let entry = $in
  let dtl = ($entry | korr to datalog)
  let entryName = ($entry | korr query name)
  let sub = ($entryName | utils to-filename --suffix "_rules.pl")
  let name = ([ $entryName " rules" ] | str join)
  let instance = ( "query(U) :- name(U, \"Source code file\")."
                 | korr query | get 0.0.uuid )
  let temp = (mktemp)
  let kmodule = ($entry | korr create sub $temp --sub $sub $instance $name)
  $entry | korr meta add $"relation-rules, self, self/'($sub)'"
  $kmodule
}

export def 'create entry' [] {
  let class = (query classes | each { [ $in.Entity $in.Name ] } | ui fuzzy filter)
  let title = (gum input --placeholder $"Name for new ($class.value)")
  korr create entry $class.key $title
}

export def 'create subclass' [] {
  let class = (query classes | each { [ $in.Entity $in.Name ] } | ui fuzzy filter)
  let title = (gum input --placeholder $"Name for new subclass of ($class.value)")
  let class_class = (query class | get uuid)
  ( korr create entry $class_class $title
  | korr meta add $"is-a, self, '($class.key)'" )
}

export def 'create relation' [name: string] {
  let class_rel = (query relation | get uuid)
  let arity = (gum input --placeholder $"Arity for ($name)" | into int)
  let classes = ( query classes | append (query types)
                | each { [ $in.Entity $in.Name ] })
  let args = ( 
    1..($arity) 
    | each ({ |id| 
      let class = ($classes | ui fuzzy filter)
      $"relation-type-at, self, ($id), '($class.key)'"
    })
    | str join "\n"
  )
  ( korr create entry $class_rel $name 
  | korr meta add $args )
}

export def 'create rules' [] {
  let rels = (query relation)
  (
    $"query\(R, N) :- instance-of\(R, '($rels | korr to datalog)'), name\(R, N)."
    | korr query
    | each { [ ($in.0 | korr to datalog) $in.1 ] }
    | ui fuzzy filter | get key
    | korr from datalog
    | edit rules
  )
}

export def 'edit rules' [] {
  let entry = $in
  let korrs = ($entry | resolve module)
  let korr = (if ($korrs | is-empty) {
    $entry | create module
  } else {
    $korrs | get 0
  })
  run-external $env.EDITOR ($korr | korr resolve file)
  $korr
}

# Get the class of classes
export def 'query class' [] {
  (
    "query(T) :- instance-of(T, T)."
    | korr query
    | get 0.0
    | upsert name "Ontological class"
  )
}

# List all classes
export def 'query classes' [] {
  let classes = ("query(N, C) :- instance-of(T, T), instance-of(C, T), name(C, N)."
                | korr query)
  $classes | each { { Name: $in.0, Entity: $in.1.uuid } }
}

# The the korrvigs types
export def 'query types' [] {
  (
    "query(N, C) :- name(T, \"Korrvigs type\"), instance-of(C, T), name(C, N)."
    | korr query
    | each { { Name: $in.0, Entity: $in.1.uuid } }
  )
}

# Get the class of relations
export def 'query relation' [] {
  ( "query(R) :- name(R, \"Ontology relation\"), instance-of(R, T), instance-of(T,T)."
  | korr query
  | get 0.0
  | upsert name "Ontology relation" )
}

export def 'resolve module' [] {
  let entry = ($in | korr to datalog)
  $"query\(K) :- relation-rules\('($entry)', K)." | korr query | each { get 0 }
}

export def 'export relations def' [] {
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

export def 'export relations summary' [] {
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

export def 'export relations rules' [] {
  (
    "query(R) :- relation-rules(C, R)."
    | korr query
    | each { get 0 | korr resolve file | open | lines }
    | flatten
  )
}

export def 'export classes tree' [] {
  ( "query(A, B) :- is-a(U, V), name(U, A), name(V, B)."
  | korr query 
  | each { |in| $"  \"($in.0)\" -> \"($in.1)\";" }
  | prepend "digraph {" | append "}"
  | to text )
}

export def 'export classes graph' [] {
  let tree = (
    "query(A, B) :- is-a(U, V), name(U, A), name(V, B)."
    | korr query 
    | each { |in| $"  \"($in.0)\" -> \"($in.1)\";" }
  )
  let rels = (
    'query(N, A, B) :- 
        relation-type-at(R, 1, U),
        relation-type-at(R, 2, V),
        class-of(V, "Ontology class"),
        name(R, N), name(U, A), name(V, B).'
    | korr query
    | each { |in| $"  \"($in.1)\" -> \"($in.2)\" [color=\"blue\",label=\"($in.0)\"];" }
  )
  (
    [ "digraph {", $tree, $rels, "}" ]
    | flatten
    | to text
  )
}

