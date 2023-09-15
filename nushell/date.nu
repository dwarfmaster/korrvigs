
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

export def 'attach subdate' [
  --prompt: string
] {
  let entry = $in
  let query = (
    [ $entry.query "time-datum" ] 
    | filter { |s| not ($s | is-empty) }
    | str join "-"
  )
  let time_class = (resolve time-datum)
  let prompt = (if ($prompt | is-empty) {
    $entry | korr query name
  } else {
    $prompt
  })
  let date = ($prompt | select date)
  let time_entry = ($entry | upsert query $query)
  let time_dt = ($time_entry | korr to datalog --self)
  (
    $entry 
    | korr meta add $"instance-of, ($time_dt), ($time_class | korr to datalog)"
    | korr meta add $"time-point-datum, ($entry | korr to datalog --self), ($time_dt)"
    | korr meta add $"time-datum-year, ($time_dt), ($date.year)"
    | korr meta add $"time-datum-day-in-year, ($time_dt), ($date.dayOfYear)"
    | korr meta add $"time-datum-month, ($time_dt), ($date.month)"
    | korr meta add $"time-datum-day-in-month, ($time_dt), ($date.day)"
    | korr meta add $"time-datum-week, ($time_dt), ($date.week)"
    | korr meta add $"time-datum-day-in-week, ($time_dt), ($date.dayOfWeek)"
    | korr meta add $"time-datum-day-in-hour, ($time_dt), ($date.hour)"
    | korr meta add $"time-datum-day-in-minute, ($time_dt), ($date.minute)"
    | korr meta add $"time-datum-day-in-second, ($time_dt), ($date.second)"
    | korr meta add $"time-datum-day-in-nanosecond, ($time_dt), ($date.nanosecond)"
    | korr meta add $"time-datum-utc-offset, ($time_dt), ($date.timezone)"
  )
  $time_entry
}

export def 'attach date' [] {
  select point | attach subdate
}

export def 'resolve time-datum' [] {
  "Time datum" | korr resolve class
}

export def 'resolve time-point' [] {
  "Time point" | korr resolve class
}

export def 'resolve time-region' [] {
  "Temporal region" | korr resolve class
}

export def 'resolve time-segment' [] {
  "Continuous temporal region" | korr resolve class
}
