import os
import shutil
import tempfile

import pandas as pd

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


def depth(lst):
    dep = -1
    if isinstance(lst,list):
        for item in lst:
            dep = max(dep,depth(item))
    return dep + 1

def pdf_tables2zip_csv(pdf_files, zip_file="tables.zip"):
    """
    複数のpdfファイルから表をcsvとして抽出し，zipファイルに圧縮
    
    streamlit用の関数
    """
    with tempfile.TemporaryDirectory() as dir:
        _write_csv(pdf_files, dir)
        shutil.make_archive(os.path.splitext(zip_file)[0], format='zip', root_dir=dir)
    return(zip_file)


def _write_csv(pdf_files, dir):
    """Writes the tables in the PDFs as csv files into dir."""
    for up_file in pdf_files:
        name = up_file if isinstance(up_file, str) else up_file.name
        with pdfplumber.open(up_file) as pdf:
            for i, page in enumerate(pdf.pages):
                tbls = page.extract_tables()
                if(tbls == []):
                    pass
                else:
                    for j, tb in enumerate(tbls):
                        pd.DataFrame(tb).to_csv(out_path(name, f'_{i+1}_{j+1}', '.csv', dir), header=False, index=False)

if __name__ == "__main__":
    # path_in = "mtcars.pdf"
    # path_out = 'mtcars.xlsx'
    tables = []
    with pdfplumber.open(path_in) as pdf:
        for page in pdf.pages:
            tables.append(page.extract_tables())

    with pd.ExcelWriter(path_out) as writer:
        for i, table in enumerate(tables):
            if(table == []):
                pass
            else:
                for j, tb in enumerate(table):
                    pd.DataFrame(tb).to_excel(writer, sheet_name=f'{i+1}_{j+1}')


