#!/usr/bin/env bash
#
# Define the expected remote URL.
EXPECTED_REMOTE_URL="https://github.com/saygo-png/nixos.git"
EXPECTED_REMOTE_URL_SSH="git@github.com:saygo-png/nixos.git"

# Get the remote URL of the repository.
REMOTE_URL=$(git config --get remote.origin.url)

# Find the root directory of the Git repository.
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)

# If not inside a git repository, print an error and exit.
if [ -z "$REPO_ROOT" ]; then
  echo "Error: Not inside a Git repository."
  exit 1
fi

# Check if the remote URL matches the expected one.
if [ "$REMOTE_URL" != "$EXPECTED_REMOTE_URL" ] && [ "$REMOTE_URL" != "$EXPECTED_REMOTE_URL_SSH" ]; then
  echo "Error: This script only works in the specific repository: $EXPECTED_REMOTE_URL"
  exit 1
fi

# Navigate to the root of the repository.
cd "$REPO_ROOT" || exit

# List all tracked files in the repository and format.
git ls-files | grep -E '\.sh$|\.py$|\.nix$|\.lua$|\.md$' | while read -r file; do
  nvim "$file" -c ":retab" -c ":lua require('conform').format({ timeout_ms = 500 })" -c ":wq"
done
