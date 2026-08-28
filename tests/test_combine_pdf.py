"""combine_pdf のテスト.

combine_pdf.py と combine_pdf2.py は `if __name__ == "__main__":` で囲われておらず，
読み込むだけで xlsx を読みに行く処理が走る．そのため関数だけを試せない．
ここでは，その状態を xfail として残しておく (囲えば通るようになる)．
"""
import pytest

from conftest import load


@pytest.mark.parametrize(
    "path", ["combine_pdf/combine_pdf.py", "combine_pdf/combine_pdf2.py"]
)
@pytest.mark.xfail(
    strict=True,
    reason="main の処理が module の直下にあるため，読み込むだけで実行される",
)
def test_module_can_be_imported(path, tmp_path, monkeypatch):
    """関数を試すために，読み込みだけで副作用が起きないようにしたい."""
    monkeypatch.chdir(tmp_path)
    load(path)
