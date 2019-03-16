#!/bin/env bash
# usage: process infile outfile <optional prefix>

scope=$(head -n1 "$1" | sed 's/^\/\/\s*scope\s\+//')

cpp "$1" \
  | sed 's/^#/\/\//' \
  | sed "s/\\(\\w\\+\\)\\(#\\|@\\)/${3}\\1/g" \
  | sed "s/\\b_/${3}${scope}_/g" > "$2"
  
