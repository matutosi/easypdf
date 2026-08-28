"""各ディレクトリへ写した共通処理が，ずれていないかを見る.

共通コードは1か所へまとめず，各ツールのディレクトリに同じものを置く方針
(2026-08-28 ユーザ確定)．写した先がずれると直したつもりで直っていない箇所ができるので，
ここで中身が同じであることを機械的に確かめる．
"""
import inspect
import os
import textwrap

import pytest

from conftest import load

MODULES = {
    "highlight_pdf": load("highlight_pdf/highlight_pdf.py"),
    "extract_tables": load("extract_x/extract_tables.py"),
    "extract_texts": load("extract_x/extract_texts.py"),
    "extract_images": load("extract_x/extract_images.py"),
    "combine_pdf": load("combine_pdf/combine_pdf.py"),
    "combine_pdf2": load("combine_pdf/combine_pdf2.py"),
}

# 関数名 -> それを持つべき module 名
# `read_excel`・`input_files`・`convert_color_hex`・`COLORS` は，
# 2026-08-28 に highlight_xlsx を convex へ寄せてから highlight_pdf だけのものになった．
# 写しが2つ以上になったら，ここへ戻して一致を見張る．
SHARED = {
    "out_path": ["highlight_pdf", "extract_tables",
                 "extract_texts", "extract_images"],
    "extract_file_names": ["combine_pdf", "combine_pdf2"],
    "get_digit": ["extract_texts", "extract_images"],
}


def source(module, name):
    return textwrap.dedent(inspect.getsource(getattr(module, name)))


@pytest.mark.parametrize("name, owners", sorted(SHARED.items()))
def test_shared_function_is_identical(name, owners):
    """同じ名前の関数は，どのファイルでも同じ中身にする."""
    sources = {o: source(MODULES[o], name) for o in owners}
    first = sources[owners[0]]
    for owner, src in sources.items():
        assert src == first, f"{owner} の {name} が {owners[0]} とずれている"


def test_color_table_has_the_expected_names():
    """色の表は，README に書いた名前をすべて持ち，値は 6桁の16進にする."""
    colors = MODULES["highlight_pdf"].COLORS
    names = ["white", "purple", "yellow", "red", "sky", "blue", "green", "gray"]
    assert sorted(colors) == sorted(names)
    for value in colors.values():
        assert len(value) == 6
        assert value == value.upper()
        int(value, 16)


class TestOutPath:
    def test_keeps_the_directory(self):
        """出力は入力と同じディレクトリへ置く."""
        out_path = MODULES["highlight_pdf"].out_path
        assert out_path("01.pdf", "_highlighted") == "01_highlighted.pdf"
        assert out_path("a.pdf.d/01.pdf", "_highlighted").endswith("01_highlighted.pdf")
        assert "a.pdf.d" in out_path("a.pdf.d/01.pdf", "_highlighted")

    def test_changes_the_extension(self):
        """ext を渡すと拡張子を替える."""
        out_path = MODULES["extract_tables"].out_path
        assert out_path("x/mtcars.pdf", "_1_1", ".csv", "csv").endswith("mtcars_1_1.csv")
        assert out_path("x/mtcars.pdf", "_1_1", ".csv", "csv").startswith("csv")

    def test_out_dir_drops_the_input_directory(self):
        """out_dir を渡すと，入力のディレクトリは使わない."""
        out_path = MODULES["extract_texts"].out_path
        assert out_path("d/a.pdf", "_1", ".txt", "pages") == "pages" + chr(92) + "a_1.txt" \
            or out_path("d/a.pdf", "_1", ".txt", "pages") == "pages/a_1.txt"


class TestInputFiles:
    def test_skips_the_previous_outputs(self, tmp_path, monkeypatch):
        """前に作った _highlighted は入力として拾わない."""
        input_files = MODULES["highlight_pdf"].input_files
        for name in ["01.pdf", "01_highlighted.pdf", "02.pdf"]:
            (tmp_path / name).write_text("")
        monkeypatch.chdir(tmp_path)
        assert input_files("*.pdf") == ["01.pdf", "02.pdf"]

    def test_skips_the_setting_file(self, tmp_path, monkeypatch):
        """設定ファイル自身は入力として拾わない."""
        input_files = MODULES["highlight_pdf"].input_files
        for name in ["data.pdf", "highlight_pdf.xlsx"]:
            (tmp_path / name).write_text("")
        monkeypatch.chdir(tmp_path)
        assert input_files("*.pdf", "highlight_pdf.xlsx") == ["data.pdf"]
