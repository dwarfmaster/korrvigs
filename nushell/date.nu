
use utils.nu
use korr.nu
use ui.nu

export def 'find points' [] {
  (
    "query(T, N) :- class-of(T, \"Time point\"), name(T, N)."
    | korr query 
    | each { |tp| $tp.0 | upsert name $tp.1 }
  )
}

export def 'select point' [] {
  let points = (find points)
  let point = (
    $points 
    | each { |tp| [ ($tp | korr to datalog), ($tp | korr query name) ] }
    | ui fuzzy filter
  )
  $point.key | korr from datalog | upsert name $point.value
}

export def 'select date' [] {
  let name = $in
  let date = (gum input --placeholder $"Date value for ($name)")
  let date = (
    run-external --redirect-stdout "date" "+%Y:%j:%m:%d:%V:%u:%H:%M:%S:%N:%z" "-d" $date 
    | parse "{year}:{dayOfYear}:{month}:{day}:{week}:{dayOfWeek}:{hour}:{minute}:{second}:{nanosecond}:{timezone}"
    | get 0
  )
  let tz = (($date.timezone | str replace "30$" "50" | into decimal) / 100)
  (
    $date 
    | reject timezone
    | transpose key val
    | each { |i| $i | update val ($i.val | into int) }
    | transpose -r -d
    | insert timezone $tz
  )
}

export def 'attach subdate' [] {
  let entry = $in
  let query = (
    [ $entry.query "time-datum" ] 
    | filter { |s| not ($s | is-empty) }
    | str join "-"
  )
  let dateEntry = ($entry | upsert query $query)
  let date = ($entry | korr query name | select date)
  $entry | korr meta add $"time-point-datum, self, self#'($query)'"
  $dateEntry | korr meta add $"time-datum-year, self#'($query)', ($date.year)"
  $dateEntry | korr meta add $"time-datum-day-in-year, self#'($query)', ($date.dayOfYear)"
  $dateEntry | korr meta add $"time-datum-month, self#'($query)', ($date.month)"
  $dateEntry | korr meta add $"time-datum-day-in-month, self#'($query)', ($date.day)"
  $dateEntry | korr meta add $"time-datum-week, self#'($query)', ($date.week)"
  $dateEntry | korr meta add $"time-datum-day-in-week, self#'($query)', ($date.dayOfWeek)"
  $dateEntry | korr meta add $"time-datum-day-in-hour, self#'($query)', ($date.hour)"
  $dateEntry | korr meta add $"time-datum-day-in-minute, self#'($query)', ($date.minute)"
  $dateEntry | korr meta add $"time-datum-day-in-second, self#'($query)', ($date.second)"
  $dateEntry | korr meta add $"time-datum-day-in-nanosecond, self#'($query)', ($date.nanosecond)"
  $dateEntry | korr meta add $"time-datum-utc-offset, self#'($query)', ($date.timezone)"
}

export def 'attach date' [] {
  select point | attach subdate
}

