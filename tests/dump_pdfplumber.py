"""pdfplumber の表抽出を段階ごとに JSON へ書き出す (R 移植版との突き合わせ用).

使い方:
    python tests/dump_pdfplumber.py [PDF] [ページ番号(1始まり)] [出力先JSON]

既定は extract_x/mtcars.pdf の1ページ目を tests/fixtures/ へ書き出す.
書き出す段階は TableFinder の処理順に対応する.
    edges_raw     … page.edges (生のエッジ)
    edges_final   … merge_edges と filter_edges を通したあと (交点計算の入力)
    intersections … edges_to_intersections の結果 (座標のみ)
    cells         … intersections_to_cells の結果
    tables        … cells_to_tables の結果とセルの文字列
    chars         … page.chars (文字抽出の入力)
    words         … page.extract_words() の結果 (文字抽出の答え合わせ用)
"""
import json
import sys

import pdfplumber
from pdfplumber.table import TableFinder


def f(x):
    """Decimal や None を JSON にできる形へそろえる."""
    return None if x is None else float(x)


# 文字の抽出で使う項目だけを取り出す (R 側が見るもの)
CHAR_KEYS = ("text", "x0", "x1", "top", "bottom", "doctop", "y0", "y1",
             "width", "height", "size", "upright", "fontname", "object_type")
WORD_KEYS = ("text", "x0", "x1", "top", "bottom", "doctop", "upright")


def pick(obj, keys):
    """必要な項目だけを JSON にできる形で取り出す."""
    out = {}
    for k in keys:
        v = obj.get(k)
        out[k] = float(v) if isinstance(v, (int, float)) and not isinstance(v, bool) else v
    return out


def edge_to_dict(e):
    return {
        "x0": f(e["x0"]),
        "x1": f(e["x1"]),
        "top": f(e["top"]),
        "bottom": f(e["bottom"]),
        "width": f(e.get("width")),
        "height": f(e.get("height")),
        "orientation": e.get("orientation"),
        "object_type": e.get("object_type"),
    }


def dump(path_pdf, page_no, path_out):
    with pdfplumber.open(path_pdf) as pdf:
        page = pdf.pages[page_no - 1]
        finder = TableFinder(page)
        data = {
            "source": {"pdf": path_pdf, "page": page_no,
                       "pdfplumber": pdfplumber.__version__},
            "page_bbox": [f(v) for v in page.bbox],
            "edges_raw": [edge_to_dict(e) for e in page.edges],
            "edges_final": [edge_to_dict(e) for e in finder.edges],
            "intersections": sorted(
                [f(x), f(y)] for (x, y) in finder.intersections.keys()
            ),
            "cells": [[f(v) for v in c] for c in finder.cells],
            "chars": [pick(c, CHAR_KEYS) for c in page.chars],
            "words": [pick(w, WORD_KEYS) for w in page.extract_words()],
            "tables": [
                {
                    "bbox": [f(v) for v in t.bbox],
                    "n_rows": len(t.rows),
                    "n_cells": len(t.cells),
                    "cells": [[f(v) for v in c] for c in t.cells],
                    "extract": t.extract(),
                }
                for t in finder.tables
            ],
        }
    with open(path_out, "w", encoding="utf-8") as con:
        json.dump(data, con, ensure_ascii=False, indent=1)
    print(f"wrote {path_out}")
    print(f"  edges_raw     : {len(data['edges_raw'])}")
    print(f"  edges_final   : {len(data['edges_final'])}")
    print(f"  intersections : {len(data['intersections'])}")
    print(f"  cells         : {len(data['cells'])}")
    print(f"  tables        : {len(data['tables'])}")
    print(f"  chars         : {len(data['chars'])}")
    print(f"  words         : {len(data['words'])}")


if __name__ == "__main__":
    pdf = sys.argv[1] if len(sys.argv) > 1 else "extract_x/mtcars.pdf"
    page = int(sys.argv[2]) if len(sys.argv) > 2 else 1
    out = sys.argv[3] if len(sys.argv) > 3 else "tests/fixtures/mtcars_page1.json"
    dump(pdf, page, out)
