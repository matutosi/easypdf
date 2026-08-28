# easypdf プロジェクト

PDF の結合・強調表示などを簡単に行うためのツール (簡単PDF / easyPDF)．
Python スクリプトと，それを PyInstaller 等で固めた exe，設定用の xlsx を配布する形式．
Streamlit の web 版 (`*_web.py`) もある．
**動作確認は Windows 11 のみ**．

## 主なファイル

- `combine_pdf/` … PDF の結合 (`combine_pdf.py`，`combine_pdf2.py`，`combine_pdf_web.py`)
- `highlight_pdf/` … PDF の強調表示 (`highlight_pdf.py`，`highlight_pdf_web.py`)
- `highlight_xlsx/` … Excel の強調表示 (`highlight_xlsx.py`)
- `overlay_pdf/` … PDF の重ね合わせ (`overlay_pdf.py`)
- `extract_x/` … PDF からの抽出 (`extract_images.py`，`extract_tables.py`，`extract_texts.py`，`extract_x_web.py`)
- `R/` … pdfplumber の表抽出を R へ移植したもの (`table.R`，`utils.R`)
- `pdf/` … 動作確認用の PDF

## 決めごと

- 利用者は **exe と xlsx を任意のディレクトリに置くだけ**で使える状態を保つ
  (インストール作業を要求しない)．
- 免責事項を README に明記してある．連絡先は matutosi@gmail.com．
- `__pycache__` は追跡しない (`.gitignore` 済み)．

## 進捗状況

### 現在の状態

- 2026-08-28 11:15
  **README に6機能の対応表を入れ，`.gitignore` の `__pycache__` 漏れを直した**
  (追跡されていた `highlight_pdf/__pycache__/*.pyc` を追跡から外した)．
  **R 移植版と Python 版の表抽出を段階ごとに突き合わせた** (`tests/`)．
  交点 (39/39) と表の組み立て (1表24セル) は一致，セル検出は関数の未定義で失敗する．

- 2026-08-20 08:39
  プロジェクト管理用の `.claude/CLAUDE.md` を新規に設置した．
  実装の最終更新は 2025-11-11 で，pdfplumber の R 移植版 (`R/table.R`) の
  README 追加と，表抽出まわりの関数整理までが入っている．

## R 移植版 (`R/`) の状態

**移植は未完成で，`TableFinder` は動かない**．pdfplumber の `utils.py` にある
次の6つが移植されておらず，呼び出しだけが残っている (2026-08-28 に確認)．

| 未定義の関数 | 呼び出し元 | 影響 |
|---|---|---|
| `snap_objects` | `snap_edges` | エッジのスナップができない |
| `resize_object` | `join_edge_group` | エッジの結合ができない |
| `get_bbox_overlap` | `words_to_edges_v` | text 戦略が使えない |
| `obj_to_bbox` | `intersections_to_cells` | **セル検出が落ちる** |
| `filter_edges` | `TableFinder` | lines 戦略が使えない |
| `obj_to_edges` | `TableFinder` | explicit 戦略が使えない |

突き合わせの手順 (Python の各段階の出力を JSON にして R へ渡す)．

```
python tests/dump_pdfplumber.py      # tests/fixtures/*.json を作る
Rscript tests/compare_r_python.R     # 段階ごとに比較する
```

### 次にやること

- 未定義の6関数を `R/utils.R` へ移植する (まず `obj_to_bbox`．これだけでセル検出が通る)．
- そのあと `tests/compare_r_python.R` を通し，エッジの段階の比較も足す．
