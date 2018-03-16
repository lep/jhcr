#!/bin/env bash
# usage: process infile outfile <optional prefix>

scope=$(head -n1 "$1" | sed 's/^\/\/\s*scope\s\+//')

cpp "$1" | sed 's/^#/\/\//' > "$2"
sed -i "s/\\b_/${3}${scope}_/g" "$2"