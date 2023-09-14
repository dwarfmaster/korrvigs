
use korr.nu

export def 'fuzzy filter' [] {
  (
    each { |l| $"($l.0) ($l.1)" } | to text
    | fzf --with-nth=2..
    | str trim
    | parse "{key} {value}"
    | get 0
  )
}

export def 'fuzzy select' [] {
  let named = ("query(C, N, NC) :- name(C, N), class-of(C, NC)." | korr query)
  let named = ($named | each { [ ($in.0 | korr to datalog) $"[($in.2)] ($in.1)" ] } )
  let selected = ($named | fuzzy filter)
  let name = ($selected | get value | parse "[{class}] {name}" | get 0 | get name)
  $selected | get key | korr from datalog | upsert name $name
}

export def 'create' [] {
  let class = (korr query classes | each { [ $in.Entity $in.Name ] } | fuzzy filter)
  let title = (gum input --placeholder $"Name for new ($class.value)")
  korr create entry $class.key $title
}

export def 'create subclass' [] {
  let class = (korr query classes | each { [ $in.Entity $in.Name ] } | fuzzy filter)
  let title = (gum input --placeholder $"Name for new subclass of ($class.value)")
  let class_class = (korr query class | get uuid)
  ( korr create entry $class_class $title
  | korr meta add $"is-a, self, '($class.key)'" )
}

export def 'create relation' [name: string] {
  let class_rel = (korr query relation | get uuid)
  let arity = (gum input --placeholder $"Arity for ($name)" | into int)
  let classes = ( korr query classes | append (korr query types)
                | each { [ $in.Entity $in.Name ] })
  let args = ( 
    1..($arity) 
    | each ({ |id| 
      let class = ($classes | fuzzy filter)
      $"relation-type-at, self, ($id), '($class.key)'"
    })
    | str join "\n"
  )
  ( korr create entry $class_rel $name 
  | korr meta add $args )
}

export def 'annotate' [] {
  let entry = $in
  let notes = ($entry | korr query notes)
  let note = (if ($notes | is-empty) {
    $entry | korr create note
  } else {
    $notes | get 0
  });
  run-external $env.EDITOR ($note | korr resolve file)
  $note
}

export def 'add rules' [] {
  let entry = $in
  let korrs = ($entry | korr query korrs)
  let korr = (if ($korrs | is-empty) {
    $entry | korr create korr
  } else {
    $korrs | get 0
  })
  run-external $env.EDITOR ($korr | korr resolve file)
  $korr
}

export def 'remove' [] {
  let entry = (fuzzy select)
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

