# Convert string to valid filename
export def 'to-filename' [
  --suffix: string # Suffix to add to generated string
] {
  let r = ( $in 
          | iconv -f utf-8 -t ascii//TRANSLIT 
          | tr -d '?' | str snake-case )
  [ $r $suffix ] | str join
}

