#!/bin/env bash
# usage: PATCH_LVL=lvl process infile outfile <optional prefix>

[ -z "${PATCH_LVL}" ] && echo "Error: No patch specified" && exit 1

scope=$(head -n1 "$1" | sed 's/^\/\/\s*scope\s\+//')

cpp -DPATCH_LVL="$PATCH_LVL" "$1" \
  | sed 's/^#/\/\//' \
  | sed "s/\\(\\w\\+\\)\\(#\\|@\\)/${3}\\1/g" \
  | sed "s/\\b_/${3}${scope}_/g" > "$2"
  
