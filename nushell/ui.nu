
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
