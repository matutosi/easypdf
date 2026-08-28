"""overlay_pdf (ページ番号・講演番号の重ね合わせ) のテスト."""
import fitz
import pytest

from conftest import load

reportlab = pytest.importorskip("reportlab", reason="reportlab が要る")
op = load("overlay_pdf/overlay_pdf.py")


class TestCreateNumberPages:
    def test_page_count(self, tmp_path):
        """start から end までのページ数だけ作る."""
        out = str(tmp_path / "numbers.pdf")
        op.create_number_pages(out, start=1, end=5)
        with fitz.open(out) as doc:
            assert doc.page_count == 5

    def test_text(self, tmp_path):
        """pre と post を付けた番号が入る."""
        out = str(tmp_path / "numbers.pdf")
        op.create_number_pages(out, start=7, end=7)
        with fitz.open(out) as doc:
            assert "- 7 -" in doc[0].get_text()


class TestCreateSessionNumbers:
    def test_text_is_zero_padded(self, tmp_path):
        """講演番号は2桁でゼロ埋めされる."""
        out = str(tmp_path / "session.pdf")
        op.create_session_numbers(out, start=3, end=3, pre="A")
        with fitz.open(out) as doc:
            assert "A03" in doc[0].get_text()


class TestOverlayPdf:
    def test_overlay_keeps_both_texts(self, tmp_path):
        """重ねた結果に，両方の文字が残る."""
        background = str(tmp_path / "bg.pdf")
        overlay = str(tmp_path / "ov.pdf")
        out = str(tmp_path / "out.pdf")
        op.create_number_pages(background, start=1, end=2)
        op.create_session_numbers(overlay, start=1, end=2, pre="A")
        op.overlay_pdf(background, overlay, out)
        with fitz.open(out) as doc:
            text = doc[0].get_text()
        assert "- 1 -" in text
        assert "A01" in text

    def test_page_count_follows_the_shorter(self, tmp_path):
        """ページ数が違うときは，短いほうまでしか重ならない."""
        background = str(tmp_path / "bg.pdf")
        overlay = str(tmp_path / "ov.pdf")
        out = str(tmp_path / "out.pdf")
        op.create_number_pages(background, start=1, end=5)
        op.create_session_numbers(overlay, start=1, end=2, pre="A")
        op.overlay_pdf(background, overlay, out)
        with fitz.open(out) as doc:
            assert doc.page_count == 5
            assert "A" not in doc[4].get_text()


class TestFontSize:
    def test_font_size_alone_takes_effect(self, tmp_path):
        """font_name を渡さなくても font_size が効く."""
        small = str(tmp_path / "small.pdf")
        large = str(tmp_path / "large.pdf")
        op.create_number_pages(small, start=1, end=1, font_size=6)
        op.create_number_pages(large, start=1, end=1, font_size=48)
        with fitz.open(small) as doc:
            w_small = doc[0].get_text("words")[0][2] - doc[0].get_text("words")[0][0]
        with fitz.open(large) as doc:
            w_large = doc[0].get_text("words")[0][2] - doc[0].get_text("words")[0][0]
        assert w_large > w_small
