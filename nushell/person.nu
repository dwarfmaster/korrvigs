
use utils.nu
use korr.nu
use ui.nu
use date.nu

export def 'create person' [] {
  let name = $in
  let class = (resolve person)
  let name_class = (resolve name)
  let tr_class = (date resolve time-segment)
  let pt_class = (date resolve time-point)
  let entry = (korr create entry $class.uuid "")
  (
    $entry
    | korr meta add $"instance-of, self#'lifespan', ($tr_class | korr to datalog)"
    | korr meta add $"instance-of, self#'birth', ($pt_class | korr to datalog)"
    | korr meta add $"instance-of, self#'death', ($pt_class | korr to datalog)"
    | korr meta add "occupies-temporal-region, self, self#'lifespan'"
    | korr meta add "temporal-region-start, self#'lifespan', self#'birth'"
    | korr meta add "temporal-region-end, self#'lifespan', self#'death'"
    | korr meta add $"instance-of, self#'name', ($name_class | korr to datalog)"
    | korr meta add $"name-value, self#'name', \"($name)\""
    | korr meta add "inheres-in, self#'name', self"
  )
}

export def 'resolve persons' [] {
  (
    "query(P, N) :- name-value(Nm, N), inheres-in(Nm, P)."
    | korr query
    | each { { person: $in.0 name: $in.1 } }
  )
}

export def 'set birth' [] {
  let person = (
    resolve persons
    | each { |p| [ ($p.person | korr to datalog) $p.name ] }
    | ui fuzzy filter
  )
  (
    $person.key 
    | korr from datalog 
    | update query "birth"
    | date attach subdate --prompt $"($person.value)'s birth date"
  )
}

export def 'set death' [] {
  let person = (
    resolve persons
    | each { |p| [ ($p.person | korr to datalog) $p.name ] }
    | ui fuzzy filter
  )
  (
    $person.key 
    | korr from datalog 
    | update query "death"
    | date attach subdate --prompt $"($person.value)'s death date"
  )
}

export def 'add name' [] {
  let person = (
    resolve persons
    | each { |p| [ ($p.person | korr to datalog) $p.name ] }
    | ui fuzzy filter
  )
  let name = (gum input --placeholder $"New name for ($person.value)")
  let person = ($person.key | korr from datalog)
  mut num = 0
  let meta = ($person | korr resolve meta)
  loop {
    if (grep $"self#'name($num)'" $meta | is-empty) {
      break
    } else {
      $num = $num + 1
    }
  }
  let name_class = (resolve name)
  (
    $person
    | korr meta add $"instance-of, self#'name($num)', ($name_class | korr to datalog)"
    | korr meta add $"name-value, self#'name($num)', \"($name)\""
    | korr meta add $"inheres-in, self#'name($num)', self"
  )
}

export def 'resolve person' [] {
  "Person" | korr resolve class
}

export def 'resolve name' [] {
  "Name" | korr resolve class
}
