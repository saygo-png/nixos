import argparse
import re
import sys
from pathlib import Path

from send2trash import send2trash

def load_valid_numbers(file_path):
  try:
    with open(file_path, 'r') as f:
      return set(line.strip() for line in f if line.strip())
  except FileNotFoundError:
    print(f"Error: Numbers file '{file_path}' not found")
    sys.exit(1)

def process_files(directory, numbers_file, dry_run=True):
  # Load valid numbers
  valid_numbers = load_valid_numbers(numbers_file)

  # Prepare regex pattern
  pattern = re.compile(r'^(\d+)\.')

  # Track statistics
  files_processed = 0
  files_to_trash = 0

  # Process each file
  for file in Path(directory).iterdir():
    if match := pattern.match(file.name):
      files_processed += 1
      number = match.group(1)

      if number not in valid_numbers:
        files_to_trash += 1
        print(f"{'Would move to trash' if dry_run else 'Moving to trash'}: {file.name}")
        if not dry_run:
          try:
            send2trash(str(file.absolute()))
          except PermissionError:
            print(f"Error: Permission denied when trying to trash {file.name}")
          except Exception as e:
            print(f"Error moving {file.name} to trash: {e}")

  # Print summary
  print(f"\nSummary:")
  print(f"Files processed: {files_processed}")
  print(f"Files {'to move' if dry_run else 'moved'} to trash: {files_to_trash}")

def main():
  parser = argparse.ArgumentParser(
    description='Move files whose numbers are not in the specified list to trash.'
  )
  parser.add_argument('numbers_file', help='File containing valid numbers, one per line')
  parser.add_argument(
    '-d', '--directory', default='.', help='Directory to process (default: current directory)'
  )
  parser.add_argument(
    '--execute',
    action='store_true',
    help='Actually move files to trash (without this, just shows what would be moved)'
  )

  args = parser.parse_args()

  process_files(args.directory, args.numbers_file, dry_run=not args.execute)

if __name__ == '__main__':
  main()
