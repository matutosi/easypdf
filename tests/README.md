# tests

easypdf のテスト．**`xfail` は「見つかっているバグ」を固定したもの**で，
直すと `XPASS` になって失敗する (`strict=True`)．直したらその印を消す．

## 走らせ方

```
pip install pytest
python -m pytest tests -q
```

R 移植版と Python の突き合わせは別に走らせる (R と jsonlite が要る)．

```
python tests/dump_pdfplumber.py      # tests/fixtures/*.json を作る
Rscript tests/compare_r_python.R     # 段階ごとに比較する
```

## 中身

| ファイル | 見ているもの |
|---|---|
| `test_highlight_pdf.py` | 色名の変換，注釈が付くか，出力名 |
| `test_extract_x.py` | 入れ子の深さ，桁数，文字の抽出，表の csv/zip 化 |
| `test_overlay_pdf.py` | ページ番号・講演番号の生成と重ね合わせ |
| `test_highlight_xlsx.py` | 色名の変換，関数として呼べるか，27列以上の範囲 |
| `test_combine_pdf.py` | 読み込むだけで main が走ってしまう状態 |
| `dump_pdfplumber.py` | pdfplumber の各段階を JSON へ書き出す |
| `compare_r_python.R` | 同じ段階を R 移植版で走らせて比べる |

`conftest.py` の `load()` で，パッケージになっていないスクリプトを
ファイルの場所から読み込む．

## xfail で固定してあるバグ (10件)

- `highlight_pdf`: `gray` が黒 (0,0,0) になっている．
- `highlight_pdf`: 出力名を `path.replace(".pdf", ...)` で作るので，
  途中に `.pdf` を含むパスで壊れる．
- `extract_tables`: ファイル名にパスが入ると，出力先が `csv/<パス>_1_1.csv` になって落ちる．
- `extract_tables`: `csv` ディレクトリを消さずに使い回すので，前回の結果が zip に残る．
- `extract_images`: 画像の無い PDF で `max(pages)` が落ちる．
- `overlay_pdf`: `font_name` を渡さないと `font_size` が効かない．
- `combine_pdf.py` / `combine_pdf2.py`: main の処理が module の直下にあり，
  読み込むだけで実行される (2件)．
- `highlight_xlsx`: `load_workbook(xlsx)` が引数ではなく大域の `xlsx` を見ている．
- `highlight_xlsx`: 範囲を `chr(max_col + 64)` で作るので，27列以上で列名が壊れる．
