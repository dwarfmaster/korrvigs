
use korr.nu

export def 'table' [] {
  (
    open --raw files
    | lines
    | each { parse "{seq} - {serie} T{vol} {title}.pdf" | get 0 }
    | update seq { into int }
    | update vol { into int }
    | update serie { |s| if $s.serie == "Orcs & Gobelins" { "Orcs" } else { $s.serie } }
  )
}

export def 'serie' [name: string] {
  (
    $"query\(C) :- name\(C, \"($name)\"), class-of\(C, \"Serie\")."
    | korr query
    | get 0.0
    | insert name $name
  )
}
export def 'sub' [name: string] {
  let parent = $in
  (
    $"query\(C) :-
        name\(C, \"($name)\"),
        class-of\(C, \"Sequence\"),
        continuant-part-of-at\(C, ($parent | korr to datalog), R)."
    | korr query
    | get 0.0
    | insert name $name
  )
}

export def 'nains' [] {
  serie "BDs Nains"
}
export def 'nains forge' [] {
  nains | sub "Nains - Ordre de la forge"
}
export def 'nains talion' [] {
  nains | sub "Nains - Ordre du talion/malt"
}
export def 'nains temple' [] {
  nains | sub "Nains - Ordre du temple"
}
export def 'nains errants' [] {
  nains | sub "Nains - Ordre des errants"
}
export def 'nains bouclier' [] {
  nains | sub "Nains - Ordre du bouclier"
}
export def 'nains select' [tome: int] {
  let tome = $tome - 1
  if $tome mod 5 == 0 {
    nains forge
  } else if $tome mod 5 == 1 {
    nains talion
  } else if $tome mod 5 == 2 {
    nains temple
  } else if $tome mod 5 == 3 {
    nains errants
  } else {
    nains bouclier
  }
}
export def 'nains add' [] {
  let entry = $in
  let comic = (find comic)
  let arran = (find arran)
  let arran_comic = (find global)
  let nains = (nains)
  let sub = (nains select $entry.vol)
  let sub_id = (($entry.vol - 1) // 5) + 1
  (
    korr create entry $comic.uuid $"Terres d'Arran ($entry.seq) - Nains ($entry.vol) - ($entry.title)"
    | korr meta add $"is-about, self, ($arran | korr to datalog)"
    | korr meta add $"in-sequence-number, self, ($arran_comic | korr to datalog), ($entry.seq)"
    | korr meta add $"in-sequence-number, self, ($nains | korr to datalog), ($entry.vol)"
    | korr meta add $"in-sequence-number, self, ($sub | korr to datalog), ($sub_id)"
  )
}

export def 'elfes' [] {
  serie "BDs Elfes"
}
export def 'elfes bleus' [] {
  elfes | sub "Elfes - Elfes bleus"
}
export def 'elfes sylvains' [] {
  elfes | sub "Elfes - Elfes sylvain"
}
export def 'elfes blancs' [] {
  elfes | sub "Elfes - Elfes blancs"
}
export def 'elfes semis' [] {
  elfes | sub "Elfes - Semi elfes & elfes rouges"
}
export def 'elfes noirs' [] {
  elfes | sub "Elfes - Elfes noirs"
}
export def 'elfes select' [tome: int] {
  let tome = $tome - 1
  if $tome mod 5 == 0 {
    elfes bleus
  } else if $tome mod 5 == 1 {
    elfes sylvains
  } else if $tome mod 5 == 2 {
    elfes blancs
  } else if $tome mod 5 == 3 {
    elfes semis
  } else {
    elfes noirs
  }
}
export def 'elfes add' [] {
  let entry = $in
  let comic = (find comic)
  let arran = (find arran)
  let arran_comic = (find global)
  let elfes = (elfes)
  let sub = (elfes select $entry.vol)
  let sub_id = (($entry.vol - 1) // 5) + 1
  (
    korr create entry $comic.uuid $"Terres d'Arran ($entry.seq) - Elfes ($entry.vol) - ($entry.title)"
    | korr meta add $"is-about, self, ($arran | korr to datalog)"
    | korr meta add $"in-sequence-number, self, ($arran_comic | korr to datalog), ($entry.seq)"
    | korr meta add $"in-sequence-number, self, ($elfes | korr to datalog), ($entry.vol)"
    | korr meta add $"in-sequence-number, self, ($sub | korr to datalog), ($sub_id)"
  )
}

export def 'mages' [] {
  serie "BDs Mages"
}
export def 'mages elem' [] {
  mages | sub "Mages - Élémentalistes"
}
export def 'mages rune' [] {
  mages | sub "Mages - Runiques"
}
export def 'mages necro' [] {
  mages | sub "Mages - Nécromanciens"
}
export def 'mages alchi' [] {
  mages | sub "Mages - Alchimistes"
}
export def 'mages select' [tome: int] {
  let tome = $tome - 1
  if $tome mod 4 == 0 {
    mages elem
  } else if $tome mod 4 == 1 {
    mages rune
  } else if $tome mod 4 == 2 {
    mages necro
  } else {
    mages alchi
  }
}
export def 'mages add' [] {
  let entry = $in
  let comic = (find comic)
  let arran = (find arran)
  let arran_comic = (find global)
  let mages = (mages)
  let sub = (mages select $entry.vol)
  let sub_id = (($entry.vol - 1) // 4) + 1
  (
    korr create entry $comic.uuid $"Terres d'Arran ($entry.seq) - Mages ($entry.vol) - ($entry.title)"
    | korr meta add $"is-about, self, ($arran | korr to datalog)"
    | korr meta add $"in-sequence-number, self, ($arran_comic | korr to datalog), ($entry.seq)"
    | korr meta add $"in-sequence-number, self, ($mages | korr to datalog), ($entry.vol)"
    | korr meta add $"in-sequence-number, self, ($sub | korr to datalog), ($sub_id)"
  )
}


export def 'orcs' [] {
  serie "BDs Orcs et Gobelins"
}
export def 'orcs add' [] {
  let entry = $in
  let comic = (find comic)
  let arran = (find arran)
  let arran_comic = (find global)
  let orcs = (orcs)
  (
    korr create entry $comic.uuid $"Terres d'Arran ($entry.seq) - Orcs & Gobelins ($entry.vol) - ($entry.title)"
    | korr meta add $"is-about, self, ($arran | korr to datalog)"
    | korr meta add $"in-sequence-number, self, ($arran_comic | korr to datalog), ($entry.seq)"
    | korr meta add $"in-sequence-number, self, ($orcs | korr to datalog), ($entry.vol)"
  )
}

export def 'add all' [] {
  (
    table 
    | each { |entry| (
      if $entry.serie == "Nains" {
        $entry | nains add
      } else if $entry.serie == "Elfes" {
        $entry | elfes add
      } else if $entry.serie == "Mages" {
        $entry | mages add
      } else {
        $entry | orcs add
      } 
    )})
}

export def 'find comic' [] {
  "Comic" | korr resolve class
}
export def 'find arran' [] {
  (
    "query(C) :- name(C, \"Terres d'Arran\"), class-of(C, \"Fictional universe\")."
    | korr query
    | get 0.0
    | insert name "Terres d'Arran"
  )
}
export def 'find global' [] {
  (
    "query(C) :- name(C, \"BDs terres d'arran\"), class-of(C, \"Serie\")."
    | korr query
    | get 0.0
    | insert name "BDs terres d'arran"
  )
}
