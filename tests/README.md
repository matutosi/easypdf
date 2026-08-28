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
python tests/compare_all.py          # pdf/ と extract_x/ の PDF をまとめて突き合わせる
```

1つの PDF だけ見るときは，JSON を作ってから R にかける．

```
python tests/dump_pdfplumber.py      # tests/fixtures/mtcars_page1.json を作る
Rscript tests/compare_r_python.R     # 段階ごとに比較する
```

比べる段階は5つ (`edges` → `intersections` → `cells` → `tables` → `TableFinder`)．
前の4つは Python の出力を入力に与えるので，**どの段階で食い違うかが分かる**．
最後の `TableFinder` だけは通しで動かす．

## 中身

| ファイル | 見ているもの |
|---|---|
| `test_highlight_pdf.py` | 色名の変換，注釈が付くか，出力名 |
| `test_extract_x.py` | 入れ子の深さ，桁数，文字の抽出，表の csv/zip 化 |
| `test_overlay_pdf.py` | ページ番号・講演番号の生成と重ね合わせ |
| `test_highlight_xlsx.py` | 色名の変換，関数として呼べるか，27列以上の範囲 |
| `test_common_helpers.py` | 各ディレクトリへ写した共通処理・色の表がずれていないか |
| `test_combine_pdf.py` | 設定の読み込み，結合，main の戻り値 |
| `dump_pdfplumber.py` | pdfplumber の各段階を JSON へ書き出す |
| `compare_r_python.R` | 同じ段階を R 移植版で走らせて比べる |
| `compare_all.py` | 複数の PDF について上の2つをまとめて回す |

`conftest.py` の `load()` で，パッケージになっていないスクリプトを
ファイルの場所から読み込む．

## xfail について

**いまは xfail が1件も無い** (2026-08-28 に全部直した)．
新しくバグを見つけたら，まず `@pytest.mark.xfail(strict=True, reason=...)` で固定してから直す．
`strict=True` なので，直ると `XPASS` になって失敗し，印の消し忘れに気づける．
