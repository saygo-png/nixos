#!/usr/bin/env python
import csv
from datetime import datetime

# Define the absolute path of the CSV file
filename = "/home/samsepi0l/Documents/owdata.csv"

# Function to get the current timestamp in ISO 8601 format
def get_timestamp():
  return datetime.now().isoformat()

# Function to append data to the CSV file
def append_to_csv(value):
  timestamp = get_timestamp()
  with open(filename, mode="a", newline="") as file:
    writer = csv.writer(file)
    writer.writerow([timestamp, value])

# Main function to get user input and append the appropriate value
def main():
  user_input = input("Enter 'a' or 'g' (or 'q' to quit): ").strip().lower()
  if user_input == "a":
    append_to_csv("ams")
    print("Appended 'ams' to the CSV file.")
    print("")
  elif user_input == "g":
    append_to_csv("gen")
    print("Appended 'gen' to the CSV file.")
    print("")
  elif user_input == "q":
    print("Quitting the program.")
  else:
    print("Invalid input. Please enter 'a', 'g', or 'q'.")

if __name__ == "__main__":
  main()
