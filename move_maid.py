#!/usr/bin/env python3

from pathlib import Path
import shutil

base_dir = Path("/media/k/whatever/Musique/")
target_dir = base_dir / "Maidcore"

target_dir.mkdir(exist_ok=True)

for item in base_dir.iterdir():
    if item.is_dir() and item != target_dir:
        if "maid" in item.name.lower():
            shutil.move(str(item), str(target_dir / item.name))
            print(f"Moved directory: {item.name}")

print("Done.")
