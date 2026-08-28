"""highlight_pdf のテスト."""
import os
import shutil

import fitz
import pytest

from conftest import load

hp = load("highlight_pdf/highlight_pdf.py")


class TestConvertColorName:
    @pytest.mark.parametrize(
        "name, expected",
        [
            ("white", (1, 1, 1)),
            ("purple", (1, 0, 1)),
            ("yellow", (1, 1, 0)),
            ("red", (1, 0, 0)),
            ("sky", (0, 1, 1)),
            ("blue", (0, 0, 1)),
            ("green", (0, 1, 0)),
        ],
    )
    def test_color_name(self, name, expected):
        """色名を RGB へ変換する."""
        assert hp.convert_color_name(name) == expected

    def test_unknown_color_falls_back_to_yellow(self):
        """未知の色名は黄色になる."""
        assert hp.convert_color_name("no_such_color") == (1, 1, 0)

    def test_hex_color(self):
        """16進の色指定を 0-1 の範囲へ変換する."""
        assert hp.convert_color_name("#FF0000") == (1.0, 0.0, 0.0)
        assert hp.convert_color_name("#000000") == (0.0, 0.0, 0.0)

    def test_gray_is_gray(self):
        """gray は灰色 (白でも黒でもない)."""
        col = hp.convert_color_name("gray")
        assert col[0] == col[1] == col[2]
        assert 0 < col[0] < 1


class TestHighlightPdf:
    def test_matched_text_gets_annotation(self, tmp_path, pdf_01):
        """該当する文字列に注釈が付く."""
        work = tmp_path / "01.pdf"
        shutil.copy(pdf_01, work)
        out = hp.highlight_pdf(str(work), ["1"], ["red"])
        assert os.path.exists(out)
        with fitz.open(out) as doc:
            annots = [a for page in doc for a in page.annots()]
        assert len(annots) > 0

    def test_unmatched_text_gets_no_annotation(self, tmp_path, pdf_01):
        """該当しない文字列では注釈が付かない."""
        work = tmp_path / "01.pdf"
        shutil.copy(pdf_01, work)
        out = hp.highlight_pdf(str(work), ["該当しない文字列"], ["red"])
        with fitz.open(out) as doc:
            annots = [a for page in doc for a in page.annots()]
        assert len(annots) == 0

    def test_output_name(self, tmp_path, pdf_01):
        """出力名には _highlighted が付く."""
        work = tmp_path / "01.pdf"
        shutil.copy(pdf_01, work)
        out = hp.highlight_pdf(str(work), ["1"], ["red"])
        assert out.endswith("01_highlighted.pdf")

    def test_output_name_with_pdf_in_directory(self, tmp_path, pdf_01):
        """途中に .pdf を含むパスでも出力名が壊れない."""
        d = tmp_path / "a.pdf.d"
        d.mkdir()
        work = d / "01.pdf"
        shutil.copy(pdf_01, work)
        out = hp.highlight_pdf(str(work), ["1"], ["red"])
        assert out.endswith("01_highlighted.pdf")


class TestMain:
    def test_reads_setting_and_highlights(self, tmp_path, pdf_01, monkeypatch):
        """設定 xlsx を読んで強調表示する."""
        import pandas as pd

        shutil.copy(pdf_01, tmp_path / "01.pdf")
        pd.DataFrame({"keywords": ["1"], "colors": ["red"]}).to_excel(
            tmp_path / "highlight_pdf.xlsx", index=False
        )
        monkeypatch.chdir(tmp_path)
        assert hp.main() == 0
        assert os.path.exists("01_highlighted.pdf")

    def test_missing_setting_returns_1(self, tmp_path, monkeypatch):
        """設定 xlsx が無ければ 1 を返して終える."""
        monkeypatch.chdir(tmp_path)
        monkeypatch.setattr("builtins.input", lambda *a: "")
        assert hp.main() == 1

    def test_blank_rows_are_ignored(self, tmp_path, pdf_01, monkeypatch):
        """設定の空行は読み飛ばす (nan を探しに行かない)."""
        import pandas as pd

        shutil.copy(pdf_01, tmp_path / "01.pdf")
        pd.DataFrame(
            {"keywords": ["1", None], "colors": ["red", None]}
        ).to_excel(tmp_path / "highlight_pdf.xlsx", index=False)
        monkeypatch.chdir(tmp_path)
        assert hp.main() == 0
        with fitz.open("01_highlighted.pdf") as doc:
            annots = [a for page in doc for a in page.annots()]
        assert len(annots) > 0
