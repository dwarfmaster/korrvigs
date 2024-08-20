#!/usr/bin/env bash

psql --csv korrvigs << QUERY | tail -n +2 | fzf -d, --with-nth=2 --preview "bat --color=always {1}" | cut -d, -f1
  SELECT format('/home/luc/repos/korrvigs/wiki/%s/%s', entry_id, entry_notes),entry_name FROM entries;
QUERY

