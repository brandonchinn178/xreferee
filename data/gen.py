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
            params = "_".join(
                f"{k}={v}" for k, v in dataclasses.asdict(test_case).items()
            )
            data = dataclasses.asdict(Fixture.generate(test_case=test_case, seed=i))
            (FIXTURES_DIR / f"{params}_{i}.json").write_text(
                json.dumps(data, indent=4, sort_keys=True)
            )


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


@dataclasses.dataclass(frozen=True)
class Loc:
    file: str
    line_num: int


@dataclasses.dataclass(frozen=True)
class Fixture:
    anchors: dict[str, Loc]
    refs: dict[str, list[Loc]]
    files: dict[str, str]

    @classmethod
    def generate(cls, test_case: TestCase, *, seed: int) -> Fixture:
        return FixtureGenerator(
            random=random.Random(hash(test_case) + seed),
            test_case=test_case,
        ).generate()


@dataclasses.dataclass(frozen=True)
class FixtureGenerator:
    random: random.Random
    test_case: TestCase

    def generate(self) -> Fixture:
        labels = list(
            self.gen_set(
                lambda: self.gen_string(size=(1, 20)), size=self.test_case.num_labels
            )
        )
        files = list(self.gen_file_tree(max_depth=self.test_case.max_file_tree_depth))
        file_to_len = {
            file: self.gen_int(size=(1, self.test_case.max_file_lines))
            for file in files
        }

        # Assign each anchor to a file
        anchor_to_loc = {}
        file_to_anchors = collections.defaultdict(dict)
        for anchor in labels:
            file = self.choice(files)
            line_num = self.gen_int(size=(1, file_to_len[file]))
            anchor_to_loc[anchor] = Loc(file=file, line_num=line_num)
            file_to_anchors[file][anchor] = line_num

        # Generate references in each file
        ref_to_locs = collections.defaultdict(list)
        file_to_refs = {}
        for file in files:
            refs = self.gen_list(lambda: self.choice(labels), size=(0, 10))
            file_to_refs[file] = collections.defaultdict(list)
            for ref in refs:
                line_num = self.gen_int(size=(1, file_to_len[file]))
                ref_to_locs[ref].append(Loc(file=file, line_num=line_num))
                file_to_refs[file][ref].append(line_num)

        return Fixture(
            anchors=anchor_to_loc,
            refs=ref_to_locs,
            files={
                file: self.gen_file_content(
                    num_lines=file_to_len[file],
                    anchors=file_to_anchors[file],
                    refs=file_to_refs[file],
                )
                for file in files
            },
        )

    def gen_file_tree(self, *, max_depth: int) -> Iterable[str]:
        if max_depth == 0:
            return []

        num_dirs = self.gen_int(size=(0, self.test_case.max_num_subdirs))
        num_files = self.gen_int(size=(0, self.test_case.max_files_per_dir))

        entries = self.gen_set(
            lambda: self.gen_string(size=(1, 10)), size=num_files + num_dirs
        )
        # Throw out entries that differ only by case, for case-insensitive systems
        entries = self.filter_case_insensitive(entries)

        dirs, files = entries[:num_dirs], entries[num_dirs:]

        yield from files
        for dir in dirs:
            for f in self.gen_file_tree(max_depth=max_depth - 1):
                yield f"{dir}/{f}"

    def gen_file_content(
        self,
        *,
        num_lines: int,
        anchors: dict[str, int],
        refs: dict[str, list[int]],
    ) -> str:
        line_to_markers = collections.defaultdict(list)
        for anchor, line in anchors.items():
            line_to_markers[line].append(f"#(ref:{anchor})")
        for ref, lines in refs.items():
            for line in lines:
                line_to_markers[line].append(f"@(ref:{ref})")
        for markers in line_to_markers.values():
            self.shuffle(markers)

        def gen_line(line: int) -> str:
            line_markers = line_to_markers.get(line, [])
            gen = lambda: self.gen_string(size=(0, 50))
            return gen() + "".join(marker + gen() for marker in line_markers)

        return "\n".join(gen_line(i + 1) for i in range(num_lines))

    def filter_case_insensitive(self, strs: Iterable[str]) -> list[str]:
        d = collections.defaultdict(list)
        for s in strs:
            d[s.lower()].append(s)
        return [v[0] for v in d.values()]

    # ----- Primitives ----- #

    type Range = int | tuple[int, int]

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


if __name__ == "__main__":
    main()
