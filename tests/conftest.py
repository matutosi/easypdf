"""テストからリポジトリ内のスクリプトを読み込むための共通処理.

各ツールはパッケージになっていないので，ファイルの場所を指定して読み込む.
"""
import importlib.util
import pathlib
import sys

import pytest

ROOT = pathlib.Path(__file__).resolve().parent.parent


def load(rel_path):
    """リポジトリ内の .py をモジュールとして読み込む."""
    path = ROOT / rel_path
    name = path.stem
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="session")
def root():
    return ROOT


@pytest.fixture(scope="session")
def pdf_mtcars():
    return str(ROOT / "extract_x" / "mtcars.pdf")


@pytest.fixture(scope="session")
def pdf_01():
    return str(ROOT / "pdf" / "01.pdf")
