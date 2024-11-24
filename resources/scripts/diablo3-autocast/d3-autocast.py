import argparse
import asyncio
import os
import subprocess
import sys
from datetime import datetime
from typing import List, Tuple

# Configuration
TOGGLE_FILE = "/tmp/keypress_toggle"

def log(msg: str) -> None:
    """Print timestamped debug message."""
    timestamp = datetime.now().strftime('%H:%M:%S.%f')[:-3]
    print(f"[{timestamp}] {msg}", flush=True)

async def get_active_window() -> str:
    """Get the currently active window name."""
    try:
        proc = await asyncio.create_subprocess_exec(
            "xdotool", "getwindowfocus", "getwindowname",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
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
            *cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
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
    subprocess.run(["notify-send", "Key Press Script", "Stopped"])

    for task in tasks:
        task.cancel()

    await asyncio.gather(*tasks, return_exceptions=True)
    log("Shutdown complete")

async def main():
    # Handle toggle
    if os.path.exists(TOGGLE_FILE):
        log("Toggle file exists, stopping script")
        os.remove(TOGGLE_FILE)
        subprocess.run(["notify-send", "Key Press Script", "Stopped"])
        sys.exit(0)

    parser = argparse.ArgumentParser(description="Automate keypresses for a specific window.")
    parser.add_argument("window_name", help="The name of the target window.")
    parser.add_argument(
        "keys",
        nargs="+",
        help="List of key:interval pairs (e.g., q:0.2 r:9)."
    )
    args = parser.parse_args()

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
