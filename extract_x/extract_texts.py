import os
import math
import pandas as pd
import numpy as np
import pdfplumber

# utils
def get_digit(x):
    n_digit = math.ceil(np.log10(x + 1))
    return n_digit

# extract texts from PDF
def extract_texts(path_pdf):
    pdf = pdfplumber.open(path_pdf)
    texts = [page.extract_text() for page in pdf.pages]
    pdf.close()
    return texts

def save_texts(path_pdf, texts):
    dig_p = get_digit(len(texts))
    for i, text in enumerate(texts, start=1):
        page = str(i).zfill(dig_p)
        page_dir = 'pages'
        if not os.path.exists(page_dir):
            os.makedirs(page_dir)
        path_text = f'{page_dir}/{path_pdf}_{page}.txt'
        with open(path_text, "w", encoding='utf-8') as f:
            f.write(text)

"""
path_pdf = 'README.pdf'
texts = extract_texts(path_pdf)
save_texts(path_pdf, texts)
"""

# extract tables from PDF
def extract_table(path_pdf):
    pdf = pdfplumber.open(path_pdf)
    all_tables = []
    for p, page in enumerate(pdf.pages, start = 1):
        tables = page.find_tables()
        if not tables: # no table
            next
        for i, table in enumerate(tables, start = 1):
            table = table.extract()
            df = pd.DataFrame(table)
            all_tables.append({"page": p, "no": i, "table": df})
    pdf.close()
    return pd.DataFrame(all_tables)

def save_tables(path_pdf, tables):
    xlsx_tables = path_pdf.replace(".pdf", "_tables.xlsx")
    dig_p = get_digit(max(tables['page']))
    dig_n = get_digit(max(tables['no']))
    with pd.ExcelWriter(xlsx_tables) as writer:
        for p, n, table in zip(tables['page'], tables['no'], tables['table']):
            name = str(p).zfill(dig_p) + "_" + str(n).zfill(dig_n)
            table.to_excel(writer, sheet_name=name, index=False)
    os.startfile(xlsx_tables)
    return xlsx_tables

"""
tables = pd.DataFrame(tables)
path_pdf = 'README.pdf'
tables = extract_table(path_pdf)
save_tables(path_pdf, tables)
"""
