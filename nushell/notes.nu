
use korr.nu
use utils.nu

# Create notes for entry
export def 'create' [] {
  let entry = $in
  let dtl = ($entry | korr to datalog)
  let entryName = ($entry | korr query name)
  let sub = ($entryName | utils to-filename --suffix "_notes.md")
  let name = ([ $entryName " notes"] | str join)
  let instance = ( "query(U) :- name(U, \"Markup document\")." 
                 | korr query | get 0.0.uuid )
  let format = ( 
    "query(F) :- name(F, \"Pandoc markdown\"), class-of(F, \"Markup language\")."
    | korr query | get 0.0 | korr to datalog
  )
  let temp = (mktemp)
  $"---
title: ($name)
---
" | save --force $temp
  let note = (
    $entry 
    | korr create sub $temp --sub $sub $instance $name
    | korr meta add $"format, self, '($format)'"
  )
  $entry | korr meta add $"notes, self, self/'($sub)'"
  $note
}

# Get the notes of an entry
export def 'resolve' [] {
  let entry = ($in | korr to datalog)
  let notes = ($"query\(N) :- notes\(($entry), N)." | korr query)
  $notes | each { get 0 }
}

export def 'attach' [] {
  let entry = $in
  let notes = ($entry | resolve)
  let note = (if ($notes | is-empty) {
    $entry | create
  } else {
    $notes | get 0
  });
  run-external $env.EDITOR ($note | korr resolve file)
  $note
}

