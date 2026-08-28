"""combine_pdf のテスト (pypdf 版と PyMuPDF 版の両方)."""
import os
import shutil

import fitz
import pandas as pd
import pytest

from conftest import load

modules = {
    "pypdf": load("combine_pdf/combine_pdf.py"),
    "pymupdf": load("combine_pdf/combine_pdf2.py"),
}


@pytest.fixture(params=sorted(modules))
def cp(request):
    """2つの実装を同じテストにかける."""
    return modules[request.param]


class TestExtractFileNames:
    def test_groups_inputs_by_output(self, cp):
        """outputs が同じ入力をまとめる."""
        df = pd.DataFrame(
            {
                "inputs": ["01.pdf", "02.pdf", "03.pdf", "04.pdf"],
                "outputs": ["aa.pdf", "aa.pdf", "bb.pdf", "bb.pdf"],
            }
        )
        result = cp.extract_file_names(df)
        assert [r[0] for r in result] == ["aa.pdf", "bb.pdf"]
        assert list(result[0][1]) == ["01.pdf", "02.pdf"]
        assert list(result[1][1]) == ["03.pdf", "04.pdf"]

    def test_keeps_the_order_of_outputs(self, cp):
        """outputs は最初に出てきた順に並ぶ."""
        df = pd.DataFrame(
            {"inputs": ["a.pdf", "b.pdf"], "outputs": ["z.pdf", "y.pdf"]}
        )
        assert [r[0] for r in cp.extract_file_names(df)] == ["z.pdf", "y.pdf"]


class TestCombinePdf:
    def test_combines_two_pdfs(self, cp, tmp_path, root):
        """2つの PDF を1つにまとめる."""
        for name in ["01.pdf", "02.pdf"]:
            shutil.copy(root / "pdf" / name, tmp_path / name)
        out = str(tmp_path / "aa.pdf")
        result = cp.combine_pdf([str(tmp_path / "01.pdf"), str(tmp_path / "02.pdf")], out)
        assert result == out
        with fitz.open(out) as doc:
            assert doc.page_count == 2

    def test_missing_input_returns_none(self, cp, tmp_path, monkeypatch):
        """入力が無ければ None を返す."""
        monkeypatch.setattr("builtins.input", lambda *a: "")
        out = str(tmp_path / "aa.pdf")
        assert cp.combine_pdf([str(tmp_path / "no_such.pdf")], out) is None
        assert not os.path.exists(out)


class TestMain:
    def test_reads_setting_and_combines(self, cp, tmp_path, root, monkeypatch):
        """設定 xlsx を読んで結合する."""
        for name in ["01.pdf", "02.pdf"]:
            shutil.copy(root / "pdf" / name, tmp_path / name)
        pd.DataFrame(
            {"inputs": ["01.pdf", "02.pdf"], "outputs": ["aa.pdf", "aa.pdf"]}
        ).to_excel(tmp_path / "combine_pdf.xlsx", index=False)
        monkeypatch.chdir(tmp_path)
        assert cp.main() == 0
        with fitz.open("aa.pdf") as doc:
            assert doc.page_count == 2

    def test_missing_setting_returns_1(self, cp, tmp_path, monkeypatch):
        """設定 xlsx が無ければ 1 を返して終える (NameError にしない)."""
        monkeypatch.chdir(tmp_path)
        monkeypatch.setattr("builtins.input", lambda *a: "")
        assert cp.main() == 1
