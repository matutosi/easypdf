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

- 2026-08-28 11:45
  **リファクタリングを 1 → 3 → 5 → 4 → 6 の順に入れた** (共通コードの置き方は案 B)．
  main のガード，出力名の `out_path()`，例外を握り潰さない，入力と出力の分離，
  **R の未定義6関数の移植**，色の表の共通化 (PDF 側の `gray` が黒だったのも直った)．
  テストは 72 passed / 3 xfailed，**突き合わせは PDF 13個すべてで 5/5 一致**．

- 2026-08-28 11:15
  **README に6機能の対応表を入れ，`.gitignore` の `__pycache__` 漏れを直した**
  (追跡されていた `highlight_pdf/__pycache__/*.pyc` を追跡から外した)．
  **R 移植版と Python 版の表抽出を段階ごとに突き合わせた** (`tests/`)．
  交点 (39/39) と表の組み立て (1表24セル) は一致，セル検出は関数の未定義で失敗する．

- 2026-08-20 08:39
  プロジェクト管理用の `.claude/CLAUDE.md` を新規に設置した．
  実装の最終更新は 2025-11-11 で，pdfplumber の R 移植版 (`R/table.R`) の
  README 追加と，表抽出まわりの関数整理までが入っている．

## 残っているバグ (2026-08-28 時点．3件)

すべて `tests/` に `xfail` (strict) で固定してある．**直すと `XPASS` になって失敗する**ので，
直したらその印を消す．走らせ方は [tests/README.md](../tests/README.md)．

| 場所 | 中身 |
|---|---|
| `highlight_xlsx.py` | 範囲を `chr(max_col + 64)` で作るので，27列以上で列名が壊れる |
| `extract_images.py` | 画像の無い PDF で `max(pages)` が落ちる |
| `overlay_pdf.py` | `font_name` を渡さないと `font_size` が効かない |

テストを書いていないもの．

- `extract_tables.py` の `__main__` が `path_in`/`path_out` 未定義で落ちる．
- `extract_texts.py` は文字の無いページで `write(None)` になる．`next` は `continue` の書き間違い．

**直したもの** (2026-08-28)．main が読み込むだけで走る / 設定 xlsx が読めなくても続行して
NameError / `path.replace(".pdf", ...)` で出力名が壊れる / `extract_tables` がパス付きの
ファイル名で落ちる / `csv` の使い回し / 出力を入力として拾う / 設定ファイル自身を対象にする /
`load_workbook(xlsx)` が大域を見ている / `R/table.R` が動かない．

## リファクタリング (2026-08-27 に案を出し，2026-08-28 に 1・3・4・5・6 を実施)

**共通コードは1か所へまとめず，各ディレクトリに同じ中身を置く (案 B．ユーザ確定)**．
写した先がずれると直したつもりで直っていない箇所ができるので，
`tests/test_common_helpers.py` が中身の一致を機械的に見ている
(`out_path`・`input_files`・`read_excel`・`get_digit`・`extract_file_names`・
`convert_color_hex`・色の表 `COLORS`)．

| | 中身 | 状態 |
|---|---|---|
| 1 | main を `if __name__ == "__main__":` で囲う | 済 |
| 2 | 共通処理を1か所へ (案 B: 各ディレクトリに同じ中身) | 済 |
| 3 | 出力名の作り方を `out_path()` にそろえる | 済 |
| 4 | 入力と出力を混ぜない (`input_files()`，作業用は tempdir) | 済 |
| 5 | 例外を握り潰さない．設定の空行を読み飛ばす | 済 |
| 6 | R の未定義6関数を移植する | 済 |

## R 移植版 (`R/`) の状態

**2026-08-28 に動くようにした**．pdfplumber の `utils.py` から未移植だった6関数
(`snap_objects`・`resize_object`・`get_bbox_overlap`・`obj_to_bbox`・`filter_edges`・
`obj_to_edges`．付随して `move_object`・`line_to_edge`・`rect_to_edges`・`curve_to_edges`)
を `R/utils.R` へ移し，呼び出し側の食い違いも直した．

- `intersections_to_cells` の `edges_to_set` が bbox の行列を `intersect` していた
  (数値ごとに比べてしまう) → 1辺を1つの文字列キーにした．
- `find_smallest_cell` が最後から2番目の点を飛ばしていた → 外した．
- `orientation` が空のエッジ (曲線由来) で落ちていた → Python と同じ扱いにした．
- 表の無い PDF で落ちていた → 空を返すようにした．

**手元の PDF 13個すべてで，5段階とも pdfplumber と一致する**
(`edges` → `intersections` → `cells` → `tables` → `TableFinder`)．

```
python tests/compare_all.py     # まとめて突き合わせる
```

### 次にやること

- **リファクタリングの案 6つは 2026-08-28 にすべて入れた** (1 → 3 → 5 → 4 → 6 → 2)．
- **残っているバグ3件** (テストの xfail)．27列以上で範囲が壊れる /
  画像の無い PDF で `max(pages)` が落ちる / `font_name` 無しで `font_size` が効かない．
- `extract_tables.py` の `__main__` が `path_in`/`path_out` 未定義で落ちる (テスト未作成)．
