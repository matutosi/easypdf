# easypdf プロジェクト

PDF の結合・強調表示などを簡単に行うためのツール (簡単PDF / easyPDF)．
Python スクリプトと，それを PyInstaller 等で固めた exe，設定用の xlsx を配布する形式．
Streamlit の web 版 (`*_web.py`) もある．
**動作確認は Windows 11 のみ**．

## 主なファイル

- `combine_pdf/` … PDF の結合 (`combine_pdf.py`，`combine_pdf2.py`，`combine_pdf_web.py`)
- `highlight_pdf/` … PDF の強調表示 (`highlight_pdf.py`，`highlight_pdf_web.py`)
- `overlay_pdf/` … PDF の重ね合わせ (`overlay_pdf.py`)
- `extract_x/` … PDF からの抽出 (`extract_images.py`，`extract_tables.py`，`extract_texts.py`，`extract_x_web.py`)
- `R/` … pdfplumber の表抽出を R へ移植したもの (`table.R`，`utils.R`)
- `pdf/` … 動作確認用の PDF

## 決めごと

- **扱うのは PDF だけ**．**Excel (xlsx) の強調表示は [convex](../convex) が持つ**
  (2026-08-28 ユーザ確定．重複していた `highlight_xlsx/` を easypdf から消した)．
  Excel まわりの相談が来たら convex 側で直す．
- 利用者は **exe と xlsx を任意のディレクトリに置くだけ**で使える状態を保つ
  (インストール作業を要求しない)．
- **exe は git で管理せず，[GitHub Releases](https://github.com/matutosi/easypdf/releases)
  で配る** (2026-08-28 から)．`.gitignore` に `*.exe` を入れてある．
  `.py` を直したら exe を作り直し，**新しいタグで Release を作って添付する**．
- 免責事項を README に明記してある．連絡先は matutosi@gmail.com．
- `__pycache__` は追跡しない (`.gitignore` 済み)．

## 進捗状況

### 現在の状態

- 2026-08-28 12:52
  **`highlight_xlsx/` を消し，easyPDF を PDF 専用にした** (convex と重複していたため)．
  一致テストの対象から外し，README に convex への案内を入れた．テストは 77 passed / 2 skipped
  (skip は opencv 未導入の web 版で，以前から同じ)．

- 2026-08-28 11:54
  **残っていたバグ5件を直し，テストを足した** (27列以上の範囲 / 画像の無い PDF /
  `font_size` が効かない / `extract_tables` の `__main__` / 文字の無いページ)．
  **xfail は 0 件になり，81 passed**．
  そのうえで **`combine_pdf.exe` と `highlight_pdf.exe` を作り直し** (動作確認済み)，
  **exe を git から外して Release `v2026.08.28` で配る形にした**．
  履歴からも消したので `.git` は 303MB → 約 1MB．

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

## exe の配り方 (2026-08-28 に変えた)

- **exe は git で管理せず，[Releases](https://github.com/matutosi/easypdf/releases) で配る**．
  `.gitignore` に `*.exe` がある．手元の exe は消していない (無視されるだけ)．
- **古い exe は履歴からも消した**．`git filter-repo --path-glob '*.exe' --invert-paths` で
  7個 (計 266MB) を落とし，**`.git` は 303MB → 約 1MB** になった．
  全コミットのハッシュが変わったので，`git log` の古い記録や外の参照とは合わない．
  Release のタグ `v2026.08.28` も新しい側へ付け替えてある．
  **GitHub 側の使用量は，向こうの掃除が回るまで減らない**ことがある．
- **3台の PC には Dropbox 経由で書き換え後の `.git` が届く**．
  もし他の PC で古い履歴のまま作業してしまったら，そちらを捨てて同期を待つ．

### 新しい版を出す手順

1. `.py` を直す → `python -m pytest tests -q` を通す．
2. ツールごとに仮想環境を作り，`pyinstaller --onefile --icon <絶対パス>` で固める
   (`--icon` は spec の場所から解決されるので**絶対パスで渡す**)．
3. できた exe を一時ディレクトリで実際に動かして確かめる．
4. `gh release create <タグ> <exe...> --title ... --notes-file ...`．

## exe の作り直し

- **2026-08-28 に `combine_pdf.exe` (31.6MB) と `highlight_pdf.exe` (49.3MB) を作り直した**．
  ツールごとに仮想環境を作り，`pyinstaller --onefile --icon` で固める (README の手順どおり)．
  入れる版は `combine_pdf`: pandas・openpyxl・pypdf / `highlight_pdf`: pandas・openpyxl・PyMuPDF
  (**PIL は要らなくなった**)．
- **`.py` を直したら exe も作り直し，Release に添付する**．
  そうしないと配布物だけ古い動きのまま残る．

## バグ (2026-08-28 に全部直した)

**テストの xfail は 0 件**．新しく見つけたら，まず `xfail(strict=True)` で固定してから直す
(手順は [tests/README.md](../tests/README.md))．

直したもの: main が読み込むだけで走る (2件) / 設定 xlsx が読めなくても続行して NameError /
`path.replace(".pdf", ...)` で出力名が壊れる / `extract_tables` がパス付きの名前で落ちる /
`csv` の使い回し / 出力を入力として拾う / 設定ファイル自身を対象にする /
`load_workbook(xlsx)` が大域を見ている / `gray` が黒 / 27列以上で範囲が壊れる /
画像の無い PDF で `max(pages)` が落ちる / `font_name` 無しで `font_size` が効かない /
`extract_tables` の `__main__` が未定義変数で落ちる / 文字の無いページで `write(None)` /
`next` が `continue` の書き間違い / `R/table.R` が動かない．

**`overlay_pdf.py` の `create_session_number` は 2026-08-28 に消した** (どこからも
呼ばれていなかった)．講演番号もページ番号と同じ `create_number_page` (中央そろえ) で描く．

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

**2026-08-28 に文字抽出 (`extract_words`) も突き合わせ，そこでも2つ直した**．

- `iter_extract_tuples` が `sapply` の結果を行列と決めつけていた
  (`extra_attrs` が無いと `upright` だけになり，行列にならず落ちる)．
- 空白の判定が `grepl("^\s+$", text)` で，**U+00A0 (改行しない空白) を空白と見なさず**，
  語がつながっていた → PCRE に `(*UCP)` を付けた．
  `split_at_punctuation` が空のときに `"[]"` という不正な正規表現を作る件も直した．

**手元の PDF 13個すべてで，6段階とも pdfplumber と一致する**
(`edges` → `intersections` → `cells` → `tables` → `TableFinder` → `words`)．

```
python tests/compare_all.py     # まとめて突き合わせる
```

### 次にやること

- **【要判断】Releases の `highlight_xlsx.exe` をどうするか**．
  `v2026.08.28` に添付したままで，リポジトリには元のコードが無い状態になっている．
  外すなら `gh release delete-asset v2026.08.28 highlight_xlsx.exe`．
  **手元の `highlight_xlsx/highlight_xlsx.exe` と `__pycache__` も残っている**
  (git 管理外なので `git rm` では消えない)．
- **急ぎのものは無い** (2026-08-28 時点)．バグは全部直し，テストは 81 passed で xfail は 0 件．
リファクタリングの案6つもすべて入れた．

- **`combine_pdf_web.py` は `opencv-python` が要る** (サムネイルの作成に使っている)．
  入っていない環境ではテストが skip される．requirements.txt も無い．
  `st.image` に置き換えれば依存を減らせる (未着手)．
- `R/` に移植したのは表抽出と文字抽出まで．`extract_text(layout = TRUE)` は突き合わせていない．
