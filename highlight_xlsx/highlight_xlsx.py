import os
import glob
import pandas as pd
import openpyxl # Need to read xlsx
from openpyxl.formatting.rule import FormulaRule
from openpyxl.styles import PatternFill

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
    out_xlsx = path_xlsx.replace(".xlsx", "_highlighted.xlsx")
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



#ワークブックを上書き保存



# main
input_xlsxs = glob.glob("*.xlsx")
df = read_excel("highlight_xlsx.xlsx")
keywords = df.keywords
colors = df.colors
for xlsx in input_xlsxs:
    highlight_xlsx(xlsx, keywords, colors)

