#!/usr/bin/env bash

# Exit on error, undefined variables, and pipe failures
set -euo pipefail

INPUT_FILE="../flake.lock"
OUTPUT_FILE="pinned-nixpkgs-for-nixshell.json"

# Check if input file exists
if [[ ! -f "$INPUT_FILE" ]]; then
  echo "Error: Input file '$INPUT_FILE' not found" >&2
  exit 1
fi

# Check if input file is readable
if [[ ! -r "$INPUT_FILE" ]]; then
  echo "Error: Input file '$INPUT_FILE' is not readable" >&2
  exit 1
fi

# Check if output directory is writable
if [[ ! -w "$(dirname "$OUTPUT_FILE")" ]]; then
  echo "Error: Directory '$(dirname "$OUTPUT_FILE")' is not writable" >&2
  exit 1
fi

# Check if the input file contains valid JSON
if ! jq empty "$INPUT_FILE" 2>/dev/null; then
  echo "Error: Input file '$INPUT_FILE' is not valid JSON" >&2
  exit 1
fi

# If all checks pass, create the output file
jq '
  . as $root |
  ["nixpkgs", "nixpkgs-unstable", "devenv"] as $packages |
  reduce $packages[] as $pkg ({};
    . + {
      ($pkg): {
        rev: $root.nodes[$pkg].locked.rev,
        sha256: $root.nodes[$pkg].locked.narHash
      }
    }
  )
' "$INPUT_FILE" >"$OUTPUT_FILE"

# Verify the output is valid JSON
if ! jq empty "$OUTPUT_FILE" 2>/dev/null; then
  echo "Error: Failed to create valid JSON output" >&2
  rm -f "$OUTPUT_FILE" # Clean up invalid output
  exit 1
fi

echo "Successfully created $OUTPUT_FILE with pinned package information"
