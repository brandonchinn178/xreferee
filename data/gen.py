#!/usr/bin/env python3
from __future__ import annotations

import collections
import dataclasses
import json
import random
import string
from pathlib import Path
from typing import Callable, Iterable

HERE = Path(__file__).resolve().parent
FIXTURES_DIR = HERE / "fixtures"

def main():
    for test_case in TEST_CASES:
        for i in range(10):
            params = "_".join(f"{k}={v}" for k, v in dataclasses.asdict(test_case).items())
            files = dict(gen_files(test_case, seed=i))
            (FIXTURES_DIR / f"{params}_{i}.json").write_text(
                json.dumps(files, indent=4)
            )

def gen_files(test_case: TestCase, *, seed: int) -> Iterable[tuple[str, str]]:
    r = Random(seed=hash(test_case) + seed)
    labels = list(r.gen_set(lambda: r.gen_string(size=(1, 20)), size=test_case.num_labels))
    files = sorted(gen_file_tree(r, test_case, max_depth=test_case.max_file_tree_depth))
    file_to_anchors = gen_reverse_map(labels, lambda: r.choice(files))

    for f in files:
        anchors = file_to_anchors.get(f, [])
        refs = r.gen_list(lambda: r.choice(labels), size=(0, 10))
        markers = [f"#(ref:{s})" for s in anchors] + [f"@(ref:{s})" for s in refs]
        r.shuffle(markers)
        contents = gen_file_contents(r, test_case, markers=markers)
        yield (f, contents)

def gen_file_tree(r: Random, test_case: TestCase, *, max_depth: int) -> Iterable[str]:
    if max_depth == 0:
        return []

    num_dirs = r.gen_int(size=(0, test_case.max_num_subdirs))
    num_files = r.gen_int(size=(0, test_case.max_files_per_dir))

    entries = r.gen_set(lambda: r.gen_string(size=(1, 10)), size=num_files + num_dirs)
    # Throw out entries that differ only by case, for case-insensitive systems
    entries = filter_case_insensitive(entries)

    dirs, files = entries[:num_dirs], entries[num_dirs:]

    yield from files
    for dir in dirs:
        for f in gen_file_tree(r, test_case, max_depth=max_depth - 1):
            yield f"{dir}/{f}"

def filter_case_insensitive(strs: Iterable[str]) -> list[str]:
    d = collections.defaultdict(list)
    for s in strs:
        d[s.lower()].append(s)
    return [v[0] for v in d.values()]

def gen_file_contents(r: Random, test_case: TestCase, *, markers: list[str]) -> str:
    num_lines = r.gen_int(size=(1, test_case.max_file_lines))
    line_to_markers = gen_reverse_map(markers, lambda: r.gen_int(size=(1, num_lines)))

    def gen_line(line: int) -> str:
        line_markers = line_to_markers.get(line, [])
        gen = lambda: r.gen_string(size=(0, 50))
        return gen() + "".join(marker + gen() for marker in line_markers)

    return "\n".join(gen_line(i + 1) for i in range(num_lines))

def gen_reverse_map[K, V](keys: Iterable[K], genVal: Callable[[], V]) -> dict[V, list[K]]:
    result = collections.defaultdict(list)
    for k in keys:
        v = genVal()
        result[v].append(k)
    return result

type Range = int | tuple[int, int]

class Random:
    def __init__(self, seed: int) -> None:
        self.random = random.Random(seed)

    def choice[T](self, vals: list[T]) -> T:
        return self.random.choice(vals)

    def shuffle[T](self, vals: list[T]) -> None:
        self.random.shuffle(vals)

    def gen_list[T](self, gen: Callable[[], T], *, size: Range) -> list[T]:
        n = self.gen_int(size=size)
        return [gen() for _ in range(n)]

    def gen_set[T](self, gen: Callable[[], T], *, size: Range) -> set[T]:
        n = self.gen_int(size=size)
        result = set()
        while len(result) < n:
            result.add(gen())
        return result

    def gen_int(self, *, size: Range) -> int:
        return size if isinstance(size, int) else self.random.randint(*size)

    def gen_string(self, *, size: Range) -> str:
        n = self.gen_int(size=size)
        return "".join(self.random.choices(string.ascii_letters + string.digits, k=n))

@dataclasses.dataclass(frozen=True)
class TestCase:
    num_labels: int
    max_file_lines: int
    max_file_tree_depth: int
    max_num_subdirs: int
    max_files_per_dir: int

TEST_CASES = [
    TestCase(
        num_labels=1000,
        max_file_lines=1000,
        max_file_tree_depth=5,
        max_num_subdirs=2,
        max_files_per_dir=1000,
    ),
    TestCase(
        num_labels=1000,
        max_file_lines=1000,
        max_file_tree_depth=5,
        max_num_subdirs=5,
        max_files_per_dir=20,
    ),
]

if __name__ == "__main__":
    main()
