"""Streamlit の web 版 (`*_web.py`) のテスト.

streamlit の AppTest で，ブラウザを開かずにスクリプトを走らせる.
アップロードの再現まではできないので，ここで見るのは
「読み込みで落ちないか」と「想定したウィジェットが出るか」まで.
"""
import importlib.util

import pytest

from conftest import ROOT

AppTest = pytest.importorskip("streamlit.testing.v1", reason="streamlit が要る").AppTest

WEB_APPS = {
    "combine_pdf": "combine_pdf/combine_pdf_web.py",
    "highlight_pdf": "highlight_pdf/highlight_pdf_web.py",
    "extract_x": "extract_x/extract_x_web.py",
}

# combine_pdf_web はサムネイルの作成に opencv を使う
needs_cv2 = pytest.mark.skipif(
    importlib.util.find_spec("cv2") is None, reason="opencv-python が要る"
)


def run_app(rel_path, tmp_path, monkeypatch):
    """作業ディレクトリを移してからアプリを走らせる (書き出しを散らかさない)."""
    monkeypatch.chdir(tmp_path)
    return AppTest.from_file(str(ROOT / rel_path), default_timeout=60).run()


class TestRunsWithoutException:
    def test_highlight_pdf_web(self, tmp_path, monkeypatch):
        """強調表示の web 版が落ちずに立ち上がる."""
        at = run_app(WEB_APPS["highlight_pdf"], tmp_path, monkeypatch)
        assert not at.exception

    def test_extract_x_web(self, tmp_path, monkeypatch):
        """抽出の web 版が落ちずに立ち上がる."""
        at = run_app(WEB_APPS["extract_x"], tmp_path, monkeypatch)
        assert not at.exception

    @needs_cv2
    def test_combine_pdf_web(self, tmp_path, monkeypatch):
        """結合の web 版が落ちずに立ち上がる."""
        at = run_app(WEB_APPS["combine_pdf"], tmp_path, monkeypatch)
        assert not at.exception


class TestWidgets:
    def test_highlight_pdf_web_widgets(self, tmp_path, monkeypatch):
        """PDF の受け口・キーワード欄・色の選択が出る."""
        at = run_app(WEB_APPS["highlight_pdf"], tmp_path, monkeypatch)
        assert len(at.get("file_uploader")) == 1
        assert len(at.get("text_input")) == 1
        assert len(at.get("color_picker")) == 1

    def test_extract_x_web_widgets(self, tmp_path, monkeypatch):
        """PDF の受け口が出る (複数選択できる)."""
        at = run_app(WEB_APPS["extract_x"], tmp_path, monkeypatch)
        assert len(at.get("file_uploader")) == 1

    @needs_cv2
    def test_combine_pdf_web_widgets(self, tmp_path, monkeypatch):
        """PDF の受け口が出る."""
        at = run_app(WEB_APPS["combine_pdf"], tmp_path, monkeypatch)
        assert len(at.get("file_uploader")) == 1


class TestSource:
    @pytest.mark.parametrize("rel_path", sorted(WEB_APPS.values()))
    def test_is_valid_python(self, rel_path):
        """opencv が無い環境でも，少なくとも構文は確かめる."""
        import ast

        ast.parse((ROOT / rel_path).read_text(encoding="utf-8"))
