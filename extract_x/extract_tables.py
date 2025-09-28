import os
import pandas as pd
import shutil

import pdfplumber

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
    dir = "csv"
    if not os.path.exists(dir):
        os.makedirs(dir)
    for up_file in pdf_files:
        if not isinstance(up_file, str):
            file_body = os.path.splitext(up_file.name)[0]
        else:
            file_body = os.path.splitext(up_file)[0]
        with pdfplumber.open(up_file) as pdf:
            for i, page in enumerate(pdf.pages):
                tbls = page.extract_tables()
                if(tbls == []):
                    pass
                else:
                    for j, tb in enumerate(tbls):
                        pd.DataFrame(tb).to_csv(f'{dir}/{file_body}_{i+1}_{j+1}.csv', header=False, index=False)
    shutil.make_archive(os.path.splitext(zip_file)[0], format='zip', root_dir=dir)
    return(zip_file)

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


