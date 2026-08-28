import os
import math
import pandas as pd
import numpy as np
import pdfplumber

def out_path(path, suffix="", ext=None, out_dir=None):
    """Makes an output path from an input path.
    Args:
        path (str): The input file path (with or without directories).
        suffix (str): The string to be added to the file body (e.g. "_highlighted").
        ext (str, optional): The output extension (e.g. ".csv"). Keeps the input one if None.
        out_dir (str, optional): The output directory. Uses the input one if None.
    Returns:
        str: The output file path.
    Example:
        >>> out_path("a.pdf.d/01.pdf", "_highlighted")
        "a.pdf.d/01_highlighted.pdf"
        >>> out_path("x/mtcars.pdf", "_1_1", ".csv", "csv")
        "csv/mtcars_1_1.csv"
    """
    dir_name, base = os.path.split(path)
    body, org_ext = os.path.splitext(base)
    name = f"{body}{suffix}{ext or org_ext}"
    if out_dir is not None:
        return os.path.join(out_dir, name)
    return os.path.join(dir_name, name) if dir_name else name


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
        path_text = out_path(path_pdf, f'_{page}', '.txt', page_dir)
        with open(path_text, "w", encoding='utf-8') as f:
            f.write(text or "")  # 文字の無いページは None が返る

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
            continue
        for i, table in enumerate(tables, start = 1):
            table = table.extract()
            df = pd.DataFrame(table)
            all_tables.append({"page": p, "no": i, "table": df})
    pdf.close()
    return pd.DataFrame(all_tables)

def save_tables(path_pdf, tables):
    xlsx_tables = out_path(path_pdf, "_tables", ".xlsx")
    if len(tables) == 0:  # 表が1つも無ければ何も書かない
        return None
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
