use utils.nu
use korr.nu
use ui.nu

export def 'create format' [
  class: string
  --mime: string
] {
  let name = $in
  let format_class = $class
  let mime_class = (resolve mime)
  let mime_time = (resolve time-region)
  let fmt_entry = (korr create entry $format_class.uuid $name)
  if (not ($mime | is-empty)) {
    (
      $fmt_entry
      | korr meta add $"instance-of, self#'mime', ($mime_class | korr to datalog)"
      | korr meta add $"text-identifier, self#'mime', \"($mime)\""
      | korr meta add $"denotes-at, self#'mime', self, ($mime_time | korr to datalog)"
    )
  }
  $fmt_entry
}

export def 'ui create format' [
  --choose: bool
] {
  let class = (if $choose {
    let formats = (resolve formats)
    let result = (
      $formats 
      | each { |f| [ ($f | korr to datalog) $f.name ] }
      | ui fuzzy filter
    )
    $result.key | korr from datalog | upsert name $result.value
  } else {
    (resolve format)
  })
  let name = (gum input --placeholder "Format name")
  let mime = (gum input --placeholder $"MIME type for ($name)")
  $name | create format $class --mime $mime
}

export def 'resolve format' [] {
  "Data format specification" | korr resolve class
}

export def 'resolve formats' [] {
  let format = (resolve format)
  (
    $"query\(C, N) :- name\(C, N), is-a_\(C, ($format | korr to datalog))."
    | korr query
    | each { |f| $f.0 | upsert name $f.1 }
  )
}

export def 'resolve mime' [] {
  "MIME type" | korr resolve class
}

export def 'resolve time-region' [] {
  (
    "query(TR) :- 
        name(TR, \"Mime type existence\"),
        class-of(TR, \"Continuous temporal region\")."
    | korr query
    | get 0.0
    | upsert name "Mime type existence"
  )
}
