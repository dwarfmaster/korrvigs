
use utils.nu
use korr.nu
use ui.nu
use ontology.nu
use date.nu

export def 'create role' [name : string] {
  let expected = $in
  let class = (resolve role)
  let entry = (korr create entry $class.uuid $name)
  (
    $entry
    | korr meta add $"agent-expected-class, self, ($expected | korr to datalog)"
  )
}

export def 'create template' [name: string] {
  let roles = $in
  let class = (resolve template)
  let entry = (korr create entry $class.uuid $name)
  (
    $roles
    | each { |rl| $entry | korr meta add $"involves, self, ($rl | korr to datalog)" }
  )
  $entry
}

export def 'implement template' [] {
  let instruction = $in
  let template = $instruction.template
  let actors = $instruction.actors
  let process_class = (resolve process)
  let tr = (date resolve time-region)
  let process = (korr create entry process_class.uuid "")
  (
    $process
    | korr meta add $"implements, self, ($template | korr to datalog)"
    | korr meta add $"instance-of, self#'span', ($tr | korr to datalog)"
    | korr meta add "temporally-projects-onto, self, self#'span'"
  )
  (
    $actors
    | each { |act| $process | korr meta add $"has-participant-at, self, ($act.participant | korr to datalog), self#'span', ($act.role | korr to datalog)" }
  )
  $process
}

export def 'resolve role' [] {
  "Generic role" | korr resolve class
}

export def 'resolve roles' [] {
  (
    "query(C, N) :- name(C, N), class-of(C, \"Generic role\")."
    | korr query
    | each { |rl| $rl.0 | upsert name $rl.1 }
  )
}

export def 'resolve template' [] {
  "Process template" | korr resolve class
}

export def 'resolve process' [] {
  "Process" | korr resolve class
}

export def 'resolve role candidates' [] {
  let role = $in
  let role_class = (
    $"query\(C, N) :- agent-expected-class\(($role | korr to datalog), C), name\(C, N)."
    | korr query
    | each { |q| $q.0 | insert name $q.1 }
    | get 0
  )
  let name = ($role_class | korr query name)
  if $name == "Person" {
    (
      "query(P, N) :- name-value(Nm, N), inheres-in(Nm, P)."
      | korr query
      | each { |q| [ ($q.0 | insert name $q.1) $q.1 ] }
    )
  } else if $name == "Entity" {
    (
      "query(C, N, NC) :- name(C, N), class-of(C, NC)."
      | korr query
      | each { |q| [ ($q.0 | insert name $q.1) $"[($q.2)] $q.1" ] }
    )
  } else {
    (
      $"query\(C, N) :- name\(C, N), instance-of_\(C, ($role_class | korr to datalog))."
      | korr query
      | each { |q| [ ($q.0 | insert name $q.1) $q.1 ] }
    )
  }
}

export def 'resolve role query' [query: string] {
  let role = $in
  let role_class = (
    $"query\(C, N) :- agent-expected-class\(($role | korr to datalog), C), name\(C, N)."
    | korr query
    | each { |q| $q.0 | insert name $q.1 }
    | get 0
  )
  let name = ($role_class | korr query name)
  let query_dtl = (if $name == "Person" {
    [ $"query\(P) :- name-value\(Nm, \"($query)\"), inheres-in\(Nm, P).", $query ]
  } else if $name == "Entity" {
    let query = ($query | parse "[{class}] {name}")
    [
      $"query\(C) :- name\(C, \"($query.name)\"), class-of\(C, \"($query.class)\").",
      $query.name
    ]
  } else {
    [
      $"query\(C) :-
          name\(C, \"($query)\"),
          instance-of_\(C, ($role_class | korr to datalog))."
      $query
    ]
  })
  let r = ($query_dtl.0 | korr query | each { get 0 })
  let l = ($r | length)
  if $l == 0 {
    null
  } else if $l > 1 {
    print $"Ambiguous resolution for ($name) : \"($query)\""
    exit
  } else {
    $r | get 0 | insert name $query_dtl.1
  }
}

export def 'ui create role' [] {
  let name = (gum input --placeholder "Generic role name")
  let expected = (
    ontology query classes
    | each { [ $in.Entity $in.Name ] }
    | ui fuzzy filter
  )
  (
    $expected 
    | format "'{key}'" 
    | korr from datalog 
    | insert name $expected.value
    | create role $name
  )
}

export def 'ui create template' [] {
  let name = (gum input --placeholder "Process template name")
  let candidates = (resolve roles)
  (
    resolve roles
    | each { [ ($in | korr to datalog) $in.name ] }
    | ui fuzzy check
    | each { ui merge name }
    | create template $name
  )
}
