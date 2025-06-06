import argparse
import asyncio
import os
import signal
import subprocess
import sys
from datetime import datetime
from typing import List, Tuple

# Configuration
TOGGLE_FILE = "/tmp/keypress_toggle"
PID_FILE = "/tmp/keypress_pid"

def log(msg: str) -> None:
  """Print timestamped debug message."""
  timestamp = datetime.now().strftime('%H:%M:%S.%f')[:-3]
  print(f"[{timestamp}] {msg}", flush=True)

async def get_active_window() -> str:
  """Get the currently active window name."""
  try:
    proc = await asyncio.create_subprocess_exec(
      "xdotool", "getwindowfocus", "getwindowname", stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    stdout, stderr = await proc.communicate()
    if stderr:
      log(f"Window detection error: {stderr.decode()}")
    return stdout.decode().strip()
  except Exception as e:
    log(f"Error getting window: {e}")
    return ""

async def press_key(key: str) -> None:
  """Press a key using xdotool."""
  try:
    cmd = ["xdotool", "key", "--clearmodifiers", key]
    log(f"Executing: {' '.join(cmd)}")
    proc = await asyncio.create_subprocess_exec(
      *cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    _, stderr = await proc.communicate()
    if stderr:
      log(f"Key press error: {stderr.decode()}")
    elif proc.returncode != 0:
      log(f"Key press failed with return code: {proc.returncode}")
    else:
      log(f"Successfully pressed key: {key}")
  except Exception as e:
    log(f"Error pressing key {key}: {e}")

async def key_presser(window_name: str, key: str, interval: float) -> None:
  """Async task to handle pressing a specific key at regular intervals."""
  log(f"Starting key presser for {key} every {interval} seconds")
  while os.path.exists(TOGGLE_FILE):
    try:
      current_window = await get_active_window()
      log(f"Current window: '{current_window}'")
      log(f"Target window: '{window_name}'")

      if window_name.lower() in current_window.lower():
        log(f"Window matched, pressing {key}")
        await press_key(key)
      else:
        log("Window didn't match, skipping key press")

      log(f"Sleeping for {interval} seconds")
      await asyncio.sleep(interval)
    except Exception as e:
      log(f"Error in key_presser loop: {e}")
      await asyncio.sleep(1)

async def shutdown(tasks):
  """Gracefully shutdown all tasks."""
  log("Shutting down...")
  if os.path.exists(TOGGLE_FILE):
    os.remove(TOGGLE_FILE)
  if os.path.exists(PID_FILE):
    os.remove(PID_FILE)
  subprocess.run(["notify-send", "Key Press Script", "Stopped"])

  for task in tasks:
    task.cancel()

  await asyncio.gather(*tasks, return_exceptions=True)
  log("Shutdown complete")

def stop_running_instance() -> bool:
  """Stop running instance if it exists by sending SIGTERM to the process."""
  try:
    if os.path.exists(PID_FILE):
      with open(PID_FILE, 'r') as f:
        pid = int(f.read().strip())
      try:
        os.kill(pid, signal.SIGTERM)
        log(f"Sent SIGTERM to process {pid}")
        # Give the process a moment to clean up
        asyncio.get_event_loop().run_until_complete(asyncio.sleep(0.5))
        # If process still exists, force kill it
        try:
          os.kill(pid, signal.SIGKILL)
          log(f"Sent SIGKILL to process {pid}")
        except ProcessLookupError:
          pass # Process already terminated
      except ProcessLookupError:
        log("Process not found, cleaning up files")

      # Clean up files
      os.remove(PID_FILE)
      if os.path.exists(TOGGLE_FILE):
        os.remove(TOGGLE_FILE)
      subprocess.run(["notify-send", "Key Press Script", "Stopped"])
      return True
    return False
  except Exception as e:
    log(f"Error stopping instance: {e}")
    return False

async def main():
  parser = argparse.ArgumentParser(description="Automate keypresses for a specific window.")
  parser.add_argument("--exit", action="store_true", help="Stop running instance")
  parser.add_argument("window_name", nargs="?", help="The name of the target window.")
  parser.add_argument("keys", nargs="*", help="List of key:interval pairs (e.g., q:0.2 r:9).")
  args = parser.parse_args()

  # Handle exit flag
  if args.exit:
    if stop_running_instance():
      sys.exit(0)
    log("No running instance found")
    sys.exit(1)

  # Validate required arguments for normal operation
  if not args.window_name or not args.keys:
    parser.error("Window name and key:interval pairs are required when not using --exit")

  # Check if already running
  if os.path.exists(PID_FILE):
    log("Another instance is already running. Use --exit to stop it.")
    sys.exit(1)

  # Store PID
  with open(PID_FILE, 'w') as f:
    f.write(str(os.getpid()))

  # Parse key configurations
  try:
    KEY_CONFIGS: List[Tuple[str, float]] = [
      (key, float(interval)) for key, interval in (arg.split(":") for arg in args.keys)
    ]
  except ValueError:
    parser.error("Key:interval pairs must be formatted correctly (e.g., q:0.2).")

  # Create toggle file and start script
  log("Creating toggle file and starting script")
  open(TOGGLE_FILE, 'w').close()
  subprocess.run(["notify-send", "Key Press Script", "Started"])

  # Create tasks for each key
  tasks = [
    asyncio.create_task(key_presser(args.window_name, key, interval))
    for key, interval in KEY_CONFIGS
  ]

  # Add a dummy task to keep the event loop running
  tasks.append(asyncio.create_task(asyncio.sleep(float('inf'))))

  try:
    # Wait for Ctrl+C or other termination signal
    await asyncio.gather(*tasks)
  except asyncio.CancelledError:
    log("Received cancellation")
  except KeyboardInterrupt:
    log("Received keyboard interrupt")
  finally:
    await shutdown(tasks)

if __name__ == "__main__":
  try:
    asyncio.run(main())
  except KeyboardInterrupt:
    log("Keyboard interrupt received at top level")
    if os.path.exists(TOGGLE_FILE):
      os.remove(TOGGLE_FILE)
    if os.path.exists(PID_FILE):
      os.remove(PID_FILE)
