#!/usr/bin/env python3
import json
import sys
import tempfile
from pathlib import Path


def main():
    path, = sys.argv[1:]
    files = json.loads(Path(path).resolve().read_text())

    with tempfile.TemporaryDirectory() as tmpdir:
        for relpath, content in files.items():
            file = Path(tmpdir) / relpath
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(content)

        print(f"Loaded at: {tmpdir}")
        input("Press Enter to clean up...")

if __name__ == "__main__":
    main()
