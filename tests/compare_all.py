"""複数の PDF について，R 移植版と pdfplumber の突き合わせをまとめて回す.

使い方:
    python tests/compare_all.py                 # pdf/ と extract_x/ の PDF を全部
    python tests/compare_all.py a.pdf b.pdf     # 指定した PDF だけ

各 PDF の1ページ目を JSON に落とし，Rscript tests/compare_r_python.R にかける.
段階がすべて一致すれば 0，1つでも食い違えば 1 で終わる.
"""
import glob
import pathlib
import re
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
SUMMARY = re.compile(r"(\d+) / (\d+) 段階が一致")


def compare(path_pdf, path_json):
    """1つの PDF について JSON を作り，R 側と突き合わせる."""
    subprocess.run(
        [sys.executable, str(ROOT / "tests" / "dump_pdfplumber.py"), str(path_pdf), "1", path_json],
        cwd=ROOT, check=True, capture_output=True,
    )
    done = subprocess.run(
        ["Rscript", str(ROOT / "tests" / "compare_r_python.R"), path_json],
        cwd=ROOT, capture_output=True, text=True, encoding="utf-8", errors="replace",
    )
    out = (done.stdout or "") + (done.stderr or "")
    found = SUMMARY.search(out)
    if not found:
        return None, out
    return (int(found.group(1)), int(found.group(2))), out


def main(pdfs):
    if not pdfs:
        pdfs = sorted(glob.glob(str(ROOT / "pdf" / "*.pdf"))) + \
            sorted(glob.glob(str(ROOT / "extract_x" / "*.pdf")))
    n_ok = 0
    with tempfile.TemporaryDirectory() as work:
        path_json = str(pathlib.Path(work) / "page.json")
        for pdf in pdfs:
            result, out = compare(pdf, path_json)
            name = pathlib.Path(pdf).name
            if result is None:
                print(f"{name:<16} 比較できず")
                print(out.strip()[-500:])
            else:
                got, total = result
                print(f"{name:<16} {got} / {total} 段階が一致")
                n_ok += got == total
    print(f"-- {n_ok} / {len(pdfs)} の PDF ですべての段階が一致")
    return 0 if n_ok == len(pdfs) else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
