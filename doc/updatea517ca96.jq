#!/usr/bin/env -S jq -cf

# Commit a517ca960c6dd628e480473c4f5680045c1a7b8a changed the way metadata is
# stored for most entries. The following script should be run on
# files/**/*.meta to update the storage the right way.

.mime as $mime
| .metadata as $oldmtdt
| .extracted as $extracted
| .extracted.parents as $parents
| .extracted.date? as $date
| .extracted.duration? as $duration
| .extracted.geometry? as $geometry
| .extracted.textContent? as $text
| $oldmtdt + $extracted as $mtdt
| { mime: $mime, 
    metadata: ($mtdt | del(.parents, .date, .duration, .geometry, .textContent)),
    parents: (if $parents == null then [] else $parents end),
    date: $date,
    duration: $duration,
    geometry: $geometry,
    textContent: $text
  }
| del(..|nulls)
