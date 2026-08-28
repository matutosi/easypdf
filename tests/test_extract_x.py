"""extract_x (画像・表・文字の抽出) のテスト."""
import glob
import os
import shutil
import zipfile

import pytest

from conftest import load

et = load("extract_x/extract_tables.py")
ex = load("extract_x/extract_texts.py")
ei = load("extract_x/extract_images.py")


class TestDepth:
    def test_depth(self):
        """入れ子の深さを数える (空のリストは 0)."""
        assert et.depth([]) == 0
        assert et.depth([1, 2]) == 1
        assert et.depth([[1], [2]]) == 2
        assert et.depth([[[1]]]) == 3

    def test_depth_of_non_list(self):
        """リストでなければ 0."""
        assert et.depth(1) == 0


class TestGetDigit:
    @pytest.mark.parametrize("x, expected", [(1, 1), (9, 1), (10, 2), (99, 2), (100, 3)])
    def test_get_digit(self, x, expected):
        """桁数を返す (ゼロ埋めの幅に使う)."""
        assert ei.get_digit(x) == expected


class TestExtractTexts:
    def test_page_count(self, pdf_mtcars):
        """ページ数だけの文字列が返る."""
        texts = ex.extract_texts(pdf_mtcars)
        assert len(texts) == 3
        assert "Mazda RX4" in texts[0]

    def test_save_texts(self, tmp_path, pdf_mtcars, monkeypatch):
        """ページごとに txt を書き出す."""
        monkeypatch.chdir(tmp_path)
        texts = ex.extract_texts(pdf_mtcars)
        ex.save_texts("mtcars.pdf", texts)
        written = sorted(os.path.basename(p) for p in glob.glob("pages/*.txt"))
        assert written == ["mtcars_1.txt", "mtcars_2.txt", "mtcars_3.txt"]


class TestPdfTables2ZipCsv:
    def test_makes_zip_of_csv(self, tmp_path, pdf_mtcars, monkeypatch):
        """表を csv にして zip へまとめる (PDF は作業ディレクトリに置く)."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        zip_file = et.pdf_tables2zip_csv(["mtcars.pdf"])
        assert os.path.exists(zip_file)
        with zipfile.ZipFile(zip_file) as z:
            names = z.namelist()
        assert len(names) > 0
        assert all(n.endswith(".csv") for n in names)

    def test_accepts_path_with_directory(self, tmp_path, pdf_mtcars, monkeypatch):
        """ディレクトリを含むパスを渡しても動く."""
        monkeypatch.chdir(tmp_path)
        zip_file = et.pdf_tables2zip_csv([pdf_mtcars])
        with zipfile.ZipFile(zip_file) as z:
            names = z.namelist()
        assert all(n.startswith("mtcars_") for n in names)

    @pytest.mark.xfail(
        strict=True,
        reason="csv ディレクトリを消さずに使い回すため，前回の結果が zip に残る",
    )
    def test_zip_has_no_leftover(self, tmp_path, pdf_mtcars, monkeypatch):
        """2回目の実行に，前回の結果が混ざらない."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        et.pdf_tables2zip_csv(["mtcars.pdf"])
        os.rename("csv", "csv_first")
        os.makedirs("csv")
        for name in os.listdir("csv_first"):
            os.rename(os.path.join("csv_first", name), os.path.join("csv", "old_" + name))
        zip_file = et.pdf_tables2zip_csv(["mtcars.pdf"])
        with zipfile.ZipFile(zip_file) as z:
            names = z.namelist()
        assert not any(n.startswith("old_") for n in names)


class TestExtractImages:
    @pytest.mark.xfail(
        strict=True,
        reason="画像の無い PDF では pages が空になり，max(pages) で落ちる",
    )
    def test_pdf_without_images(self, tmp_path, pdf_mtcars, monkeypatch):
        """画像の無い PDF でも落ちない."""
        monkeypatch.chdir(tmp_path)
        ei.extract_imgs(pdf_mtcars)
