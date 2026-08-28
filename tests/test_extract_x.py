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

    def test_page_without_text(self, tmp_path, root, monkeypatch):
        """文字の無いページでも落ちない (None を書こうとしない)."""
        monkeypatch.chdir(tmp_path)
        texts = [None, "a"]
        ex.save_texts("x.pdf", texts)
        assert open(os.path.join("pages", "x_1.txt"), encoding="utf-8").read() == ""

    def test_save_texts(self, tmp_path, pdf_mtcars, monkeypatch):
        """ページごとに txt を書き出す."""
        monkeypatch.chdir(tmp_path)
        texts = ex.extract_texts(pdf_mtcars)
        ex.save_texts("mtcars.pdf", texts)
        written = sorted(os.path.basename(p) for p in glob.glob("pages/*.txt"))
        assert written == ["mtcars_1.txt", "mtcars_2.txt", "mtcars_3.txt"]


class TestPdfTables2Xlsx:
    def test_writes_xlsx(self, tmp_path, pdf_mtcars, monkeypatch):
        """表を1つの xlsx にまとめる."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        out = et.pdf_tables2xlsx("mtcars.pdf")
        assert out == "mtcars_tables.xlsx"
        assert os.path.exists(out)

    def test_pdf_without_table(self, tmp_path, root, monkeypatch):
        """表の無い PDF では None を返し，ファイルを作らない."""
        shutil.copy(root / "pdf" / "01.pdf", tmp_path / "01.pdf")
        monkeypatch.chdir(tmp_path)
        assert et.pdf_tables2xlsx("01.pdf") is None
        assert not os.path.exists("01_tables.xlsx")

    def test_main_without_pdf(self, tmp_path, monkeypatch):
        """PDF が1つも無ければ 1 を返して終える."""
        monkeypatch.chdir(tmp_path)
        monkeypatch.setattr("builtins.input", lambda *a: "")
        assert et.main() == 1

    def test_main(self, tmp_path, pdf_mtcars, monkeypatch):
        """作業ディレクトリの PDF をまとめて処理する."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        assert et.main() == 0
        assert os.path.exists("mtcars_tables.xlsx")


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

    def test_zip_has_no_leftover(self, tmp_path, pdf_mtcars, monkeypatch):
        """作業用のディレクトリが残っていても，zip に混ざらない."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        os.makedirs("csv")
        with open(os.path.join("csv", "old_x.csv"), "w") as con:
            con.write("a")
        zip_file = et.pdf_tables2zip_csv(["mtcars.pdf"])
        with zipfile.ZipFile(zip_file) as z:
            names = z.namelist()
        assert names
        assert not any(n.startswith("old_") for n in names)

    def test_leaves_no_working_directory(self, tmp_path, pdf_mtcars, monkeypatch):
        """作業用のディレクトリを残さない."""
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        monkeypatch.chdir(tmp_path)
        et.pdf_tables2zip_csv(["mtcars.pdf"])
        assert not os.path.exists("csv")


class TestExtractImages:
    def test_pdf_without_images(self, tmp_path, pdf_mtcars, monkeypatch):
        """画像の無い PDF でも落ちず，0 を返す."""
        monkeypatch.chdir(tmp_path)
        assert ei.extract_imgs(pdf_mtcars) == 0
        assert not os.path.exists("images")

    def test_pdf_with_images(self, tmp_path, root, monkeypatch):
        """画像のある PDF から画像を書き出す."""
        monkeypatch.chdir(tmp_path)
        n_pages = ei.extract_imgs(str(root / "pdf" / "README.pdf"))
        if n_pages == 0:
            pytest.skip("画像のある PDF が手元に無い")
        assert glob.glob("images/*")


class TestExtractTable:
    def test_returns_one_row_per_table(self, pdf_mtcars):
        """表ごとに1行の DataFrame を返す (page と no が付く)."""
        tables = ex.extract_table(pdf_mtcars)
        assert len(tables) > 0
        assert list(tables.columns) == ["page", "no", "table"]
        assert tables["page"].min() >= 1
        assert tables["no"].min() >= 1

    def test_pdf_without_table(self, root):
        """表の無い PDF では空の DataFrame を返す."""
        tables = ex.extract_table(str(root / "pdf" / "01.pdf"))
        assert len(tables) == 0


class TestSaveTables:
    def test_writes_one_sheet_per_table(self, tmp_path, pdf_mtcars, monkeypatch):
        """表ごとに1シートの xlsx を書き出す."""
        import openpyxl

        opened = []
        monkeypatch.setattr(ex.os, "startfile", opened.append, raising=False)
        monkeypatch.chdir(tmp_path)
        shutil.copy(pdf_mtcars, tmp_path / "mtcars.pdf")
        tables = ex.extract_table("mtcars.pdf")
        out = ex.save_tables("mtcars.pdf", tables)
        assert out == "mtcars_tables.xlsx"
        assert opened == [out]  # 書き出したあとに開く
        wb = openpyxl.load_workbook(out)
        assert len(wb.sheetnames) == len(tables)

    def test_no_table_writes_nothing(self, tmp_path, root, monkeypatch):
        """表が無ければ None を返し，ファイルを作らない."""
        monkeypatch.setattr(ex.os, "startfile", lambda *a: None, raising=False)
        monkeypatch.chdir(tmp_path)
        shutil.copy(root / "pdf" / "01.pdf", tmp_path / "01.pdf")
        tables = ex.extract_table("01.pdf")
        assert ex.save_tables("01.pdf", tables) is None
        assert not os.path.exists("01_tables.xlsx")
