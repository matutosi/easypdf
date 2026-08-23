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

- 2026-08-20 08:39
  プロジェクト管理用の `.claude/CLAUDE.md` を新規に設置した．
  実装の最終更新は 2025-11-11 で，pdfplumber の R 移植版 (`R/table.R`) の
  README 追加と，表抽出まわりの関数整理までが入っている．

### 次にやること

- R 移植版 (`R/table.R`) と Python 版の表抽出の結果を突き合わせる．
- 機能が増えてきたので，README の対応表 (機能・スクリプト・exe) を更新する．
