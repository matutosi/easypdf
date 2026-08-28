# 簡単PDF(easyPDF)の使い方  
# How to use easyPDF  

## 簡単PDFとは   
## What is easyPDF?   

簡単PDFは，PDFの結合・強調表示・重ね合わせ・抽出などを簡単に行うためのソフトです．
扱うのは PDF だけです(Excel の強調表示は convex にあります．下の対応表を見てください)．
exe を配布しているのは結合(combine_pdf)と強調表示(highlight_pdf)の2つで，
そのほかは Python スクリプトとして使います．
Streamlit を使った web 版(`*_web.py`)もあります．

easyPDF is a software for easy manipulations of PDF such as combining, highlighting,
overlaying and extracting. It handles PDF only
(highlighting Excel files is in convex; see the feature table below).
Execute files (exe) are distributed for combine (combine_pdf) and highlight (highlight_pdf);
the others are used as Python scripts.
Web versions (`*_web.py`) with Streamlit are also available.

### 機能の対応表
### Feature table

| 機能 / Feature | ディレクトリ | スクリプト | exe | 設定 xlsx | web 版 |
| ---------------------------------- | -------------- | ----------------------------------------------------- | ------------------ | -------------------- | ---------------------- |
| 結合 / Combine                     | `combine_pdf/` | `combine_pdf.py` (pypdf), `combine_pdf2.py` (PyMuPDF)  | `combine_pdf.exe`  | `combine_pdf.xlsx`   | `combine_pdf_web.py`   |
| 強調表示 (PDF) / Highlight PDF     | `highlight_pdf/` | `highlight_pdf.py`                                  | `highlight_pdf.exe` | `highlight_pdf.xlsx` | `highlight_pdf_web.py` |
| 重ね合わせ / Overlay               | `overlay_pdf/` | `overlay_pdf.py`                                       | -                  | -                    | -                      |
| 抽出 / Extract                     | `extract_x/`   | `extract_images.py`, `extract_tables.py`, `extract_texts.py` | -            | -                    | `extract_x_web.py` (表のみ / tables only) |
| 表抽出の R 移植 (試作) / R port    | `R/`           | `table.R`, `utils.R`                                   | -                  | -                    | -                      |

- **重ね合わせ(overlay_pdf)** は，ページ番号や講演番号(`A-1` など)の PDF を作り，
  もとの PDF に重ねます(reportlab + PyMuPDF)．
- **抽出(extract_x)** は，PDF から画像・表・文字を取り出します(PyMuPDF + pdfplumber)．
- **R 移植(`R/`)** は pdfplumber の表抽出を R へ移す試作で，まだ完成していません
  (`R/README.txt` を見てください)．
- web 版は `streamlit run <スクリプト名>` で動かします．
- **exe は [Releases](https://github.com/matutosi/easypdf/releases) にあります** (リポジトリには置いていない)．
- **Excel (xlsx) の強調表示は [convex](https://github.com/matutosi/convex) にあります**
  (2026-08-28 に easyPDF から外しました)．簡単PDF は PDF だけを扱います．
  convex は web でも使えます: <https://matutosi.shinyapps.io/convex/>

- **overlay_pdf** creates a PDF of page numbers or session numbers (e.g. `A-1`)
  and overlays it on the original PDF (reportlab + PyMuPDF).
- **extract_x** extracts images, tables and texts from a PDF (PyMuPDF + pdfplumber).
- **R port** (`R/`) is a work in progress to port table extraction of pdfplumber to R
  (see `R/README.txt`).
- Run the web versions with `streamlit run <script>`.
- The exe files are in [Releases](https://github.com/matutosi/easypdf/releases), not in this repository.
- **Highlighting Excel (xlsx) files moved to [convex](https://github.com/matutosi/convex)**
  (removed from easyPDF on 2026-08-28). easyPDF handles PDF only.
  convex also runs on the web: <https://matutosi.shinyapps.io/convex/>


**注意**
実行ファイルの動作確認は，Win11のみで行っています．
他のOSでは，Pythonを使うか自分でビルドしてください．

**WARNING**
easyPDF work partly on Win 11. 
Use Python or build by your self in other OS.

## 免責事項   
## Disclaimer   

簡単PDFの使用による不利益への責任は負えませんので，自己責任でご利用ください．
不具合がありましたら，松村(matutosi@gmail.com)にご連絡ください．

I am not responsible for any disadvantages caused by the use of this software easyPDF, 
so please use it at your own risk.
If you have any problems, please contact Matsumura (matutosi@gmail.com).

## 導入方法   
## Installation   

**exe は [Releases](https://github.com/matutosi/easypdf/releases) から入手してください**
(リポジトリには置いていません)．設定ファイルの `*.xlsx` はこのリポジトリにあります．

*_pdf.exe と *_pdf.xlsx を任意のディレクトリに保存するだけです．   

**Download the exe from [Releases](https://github.com/matutosi/easypdf/releases)**
(they are not in this repository). The setting files (`*.xlsx`) are in this repository.

copy *_pdf.exe and *_pdf.xlsx to one directory.

## 使い方   
## How to use   

### Combine PDF

- ファイルの結合
    - conbime_pdf.py   ソースコード(pypdf版)
    - conbime_pdf2.py  ソースコード(PyMuPDF版)
    - conbime_pdf.exe  実行ファイル(Windows)
    - conbime_pdf.xlsx 設定ファイル

1. 結合するPDFファイルを combine_pdf.exe と同じディレクトリに保存．   
2. combine_pdf.xlsx のinputs列とoutputs列にファイル名を入力．
    - inputs: 入力ファイル名
    - outputs: 出力ファイル名
    - outputs列のファイル名が同じものを上から順に結合．
3. combine_pdf.exe をクリックして実行．

combine_pdf.xlsx が次のように入力されており，[01.pdf, 02.pdf, 03.pdf, 04.pdf]の4つのPDFファイルがあるとき，
01.pdf と 02.pdf を結合した aa.pdf および 03.pdf と 04.pdf を結合したbb.pdf が生成されます．

| inputs | outputs |
| ------ | ------- |
| 01.pdf | aa.pdf  |
| 02.pdf | aa.pdf  |
| 03.pdf | bb.pdf  |
| 04.pdf | bb.pdf  |


- combine pdf files
    - conbime_pdf.py   code (pypdf version)
    - conbime_pdf2.py  code (PyMuPDF version)
    - conbime_pdf.exe  execute file in windows
    - conbime_pdf.xlsx setting file

This is how to use conbime_pdf.py. 
How to use other tools are almost similar.

1. Save the PDF files to be combined in the same directory with combine_pdf.exe.   
2. Enter the file names in the inputs and outputs columns of combine_pdf.xlsx.   
    - inputs: input file names   
    - outputs: output file name   
    - The files with the same 
    Input files will be combined according the name in the outputs column from the top. 
3. Click on combine_pdf.exe to run.  

If combine_pdf.xlsx is entered as follows and there are four PDF files [01.pdf, 02.pdf, 03.pdf, 04.pdf], 
then aa.pdf is generated by combining 01.pdf and 02.pdf, and bb.pdf by combining 03.pdf and 04.pdf.

| inputs | outputs |
| ------ | ------- |
| 01.pdf | aa.pdf  |
| 02.pdf | aa.pdf  |
| 03.pdf | bb.pdf  |
| 04.pdf | bb.pdf  |

### Hightligt PDF

- 強調表示
    - hightligt_pdf.py
    - hightligt_pdf.exe  実行ファイル
    - hightligt_pdf.xlsx 設定ファイル

1. 強調表示するPDFファイルを hightligt_pdf.exe と同じディレクトリに保存．   
2. hightligt_pdf.xlsx のkeywords列とcolors列に入力．
    - keywords: 強調表示する文字列
    - colors:   強調表示で使う色(以下が利用可能)
        - purple: 紫
        - yellow: 黄
        - red   : 赤
        - sky   : 空
        - blue  : 青
        - green : 緑
        - gray  : 灰
3. hightligt_pdf.exe をクリックして実行．


- Hightligt
    - hightligt_pdf.py   code
    - hightligt_pdf.exe  execute file
    - hightligt_pdf.xlsx setting file

1. Save pdf file(s) to be hightligted in the same directory with hightligt_pdf.exe.
2. Enter the keywords and colors columns of hightligt_pdf.xlsx.   
    - keywords: strings to be hightligted
    - colors:   color name (can use the color name as below)
        - purple   
        - yellow   
        - red      
        - sky      
        - blue     
        - green    
        - gray     

3. Click on hightligt_pdf.exe to run.  

## Use on python (>= 3.9.0)

### Clone easypdf

```
git clone https://github.com/matutosi/easypdf.git
```

Library openpyxl is necessary to read xlsx file (indirectly used in combine_pdf.py through pandas).

```
python.exe -m pip install --upgrade pip
pip install pandas
pip install openpyxl
pip install pypdf
pip install PyMuPDF
pip install pyinstaller
```

### Run

```
python combine_pdf.py
python hightligt_pdf.py
```

## How to build with pyinstaller

This is how to build combine_pdf.exe. 

### Clone easypdf

```
git clone https://github.com/matutosi/easypdf.git
```

### Vertial environment

Opstional but RECOMMENDED to reduce execute file.

```
python -m venv combine_pdf
.\combine_pdf\Scripts\Activate
cd combine_pdf
```


### Libraries

Library openpyxl is necessary to read xlsx file (indirectly used in combine_pdf.py through pandas).

```
python.exe -m pip install --upgrade pip
pip install pandas
pip install openpyxl
pip install pypdf
pip install pyinstaller
```

### pyinstaller

Create setting file (*.spec).

```
pyi-makespec combine_pdf.py -n combine_pdf --onefile
```

Create execute file.

```
pyinstaller combine_pdf.spec
```

Wait for moments to finish.
