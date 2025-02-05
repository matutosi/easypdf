# 簡単PDF(easyPDF)の使い方  
# How to use easyPDF  

## 簡単PDFとは   
## What is easyPDF?   

簡単PDFは，PDFの結合などを簡単に行うためのソフトです．
2025年2月現在では結合(pdf_combine)のみです．

easyPDF is a software for easy manipulations such as combining PDFs.
Only combine (pdf_combine) is available at 2025 Feb.


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

ファイルの移動(コピー)だけで完了します．

1. combine_pdf.exe と combine_pdf.xlsx を任意のディレクトリに保存．   

## 使い方   
## How to use   

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
pip install pyinstaller
```

### Run

```
python combine_pdf.py
```

## How to build combine_pdf with Python

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
