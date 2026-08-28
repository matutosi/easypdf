import os
import glob
import pandas as pd
import openpyxl # Need to read xlsx
from openpyxl.formatting.rule import FormulaRule
from openpyxl.styles import PatternFill

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


def convert_color_name(color):
    """
    Converts a color name to its RGB value.
    Args:
        color (str): The name of the color.
    Returns:
        tuple: The Color code (e.g., 'FF0000' for red).
    Example:
        >>> convert_color_name("red")
        'FF0000'
        >>> convert_color_name("unknown")
        'FFFF00'  # default to yellow
    """
    COLORS = {
        "white" : 'FFFFFF',
        "purple": 'FF00FF',
        "yellow": 'FFFF00',
        "red"   : 'FF0000',
        "sky"   : '00FFFF',
        "blue"  : '0000FF',
        "green" : '00FF00',
        "gray"  : 'cccccc' 
    }
    try:
        col = COLORS[color]
    except:
        col = 'FFFF00'
    return col


def read_excel(path):
    """
    Reads an Excel file.
    Args:
        path (str): The path to the Excel file.
    Returns:
        pandas.DataFrame: The loaded DataFrame.
        None: If the file is not found or an error occurs.
    Raises:
        FileNotFoundError: If the file is not found.
        Exception: If any other error occurs.
    """
    try:
        df = pd.read_excel(path)
        return df
    except FileNotFoundError as e:
        print(f"File Not Found Error: {e}")
        input("Press Any Key")
    except Exception as e:
        print(f"Error: {e}")
        input("Press Any Key")


def highlight_xlsx(path_xlsx, keywords, colors, opacity = 0.3):
    out_xlsx = out_path(path_xlsx, "_highlighted")
    wb = openpyxl.load_workbook(xlsx)
    sheets = wb.worksheets
    offset = 64 # need to convert number to character
    for sheet in sheets:
        max_row = sheet.max_row
        max_col = sheet.max_column
        range_str = "".join([chr(1 + offset), str(1), ":", chr(max_col + offset), str(max_row)])
        for kwd, clr in zip(keywords, colors):
            highlight_cell(sheet, range_str, str(kwd), convert_color_name(clr))
    wb.save(out_xlsx)
    return out_xlsx

def highlight_cell(sheet, range_str, keyword, color):
    color_fill = PatternFill(start_color=color, end_color=color, fill_type='solid')
    condition = f'EXACT("{keyword}", A1)'
    rule = FormulaRule(formula=[condition], fill=color_fill)
    sheet.conditional_formatting.add(range_str, rule)

if __name__ == "__main__":
    input_xlsxs = glob.glob("*.xlsx")
    df = read_excel("highlight_xlsx.xlsx")
    keywords = df.keywords
    colors = df.colors
    for xlsx in input_xlsxs:
        out_xlsx = highlight_xlsx(xlsx, keywords, colors)
        os.startfile(out_xlsx)
