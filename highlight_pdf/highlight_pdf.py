import os
import sys
import glob
import pandas as pd
import openpyxl # Need to read xlsx
import fitz     # PyMuPDF

SUFFIX = "_highlighted"  # 出力に付ける印 (入力として拾わないために使う)


COLORS = {
    "white" : 'FFFFFF',
    "purple": 'FF00FF',
    "yellow": 'FFFF00',
    "red"   : 'FF0000',
    "sky"   : '00FFFF',
    "blue"  : '0000FF',
    "green" : '00FF00',
    "gray"  : 'CCCCCC',
}
DEFAULT_COLOR = 'FFFF00'  # 知らない色名はこれ (黄)


def convert_color_hex(color):
    """
    Converts a color name into a 6 digit hex string.
    Args:
        color (str): The name of the color (e.g. "red"), or "#RRGGBB".
    Returns:
        str: The 6 digit hex string (e.g. 'FF0000').
    Example:
        >>> convert_color_hex("red")
        'FF0000'
        >>> convert_color_hex("#ff0000")
        'FF0000'
        >>> convert_color_hex("unknown")
        'FFFF00'
    """
    if isinstance(color, str) and color.startswith("#"):
        return color[1:].upper()
    try:
        return COLORS[color]
    except (KeyError, TypeError):
        return DEFAULT_COLOR


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


def highlight_pdf(path_pdf, keywords, colors, opacity = 0.3):
    """
    Highlights specified keywords in a PDF file.
    Args:
        path_pdf (str): The path to the PDF file.
        keywords (list): A list of keywords to highlight.
        colors (list): A list of colors corresponding to each keyword.
        opacity (float, optional): The opacity of the highlight (0.0-1.0). Defaults to 0.3.
    Returns:
        str: The path to the highlighted PDF file.
    Raises:
        FileNotFoundError: If the PDF file is not found.
        ValueError: If the lengths of keywords and colors lists are different.
        Exception: If any other error occurs.
    Example:
        >>> highlight_pdf("input.pdf", ["keyword1", "keyword2"], ["red", "blue"])
        "input_highlighted.pdf"
    """
    if isinstance(path_pdf, str):
        out_pdfs = out_path(path_pdf, SUFFIX)
        doc = fitz.open(path_pdf)
    else: # streamlit
        out_pdfs = out_path(path_pdf.name, SUFFIX)
        doc = fitz.open(stream = path_pdf.read(), filetype = "pdf")
    for kwd, clr in zip(keywords, colors):
        highlight_text(doc, str(kwd), convert_color_name(clr), opacity = opacity)
    doc.save(out_pdfs)
    return out_pdfs

def highlight_text(doc, keyword, color, opacity = 0.3):
    """
    Highlights a specified keyword in a PDF document.
    Args:
        doc (fitz.Document): The PyMuPDF document object.
        keyword (str): The keyword to highlight.
        color (tuple): The highlight color (RGB tuple).
        opacity (float, optional): The opacity of the highlight (0.0-1.0). Defaults to 0.3.
    Returns:
        fitz.Document: The updated PyMuPDF document object.
    """
    for page in doc:
        text_instances = page.search_for(keyword)
        for inst in text_instances:
            rect = inst.irect  # get rectangle inst
            annot = page.add_rect_annot(rect)
            annot.set_colors(stroke = (1,1,1), fill = color) # (1,1,1): white
            annot.update(opacity = opacity)
    return doc

def convert_color_name(color):
    """
    Converts a color name to its RGB value.
    Args:
        color (str): The name of the color (e.g. "red"), or "#RRGGBB".
    Returns:
        tuple: The RGB value in 0-1 (e.g., (1.0, 0.0, 0.0) for red).
    Example:
        >>> convert_color_name("red")
        (1.0, 0.0, 0.0)
        >>> convert_color_name("unknown")
        (1.0, 1.0, 0.0)  # default to yellow
    """
    col = convert_color_hex(color)
    return tuple(int(col[i:i + 2], 16) / 255.0 for i in (0, 2, 4))


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

def input_files(pattern, path_setting=None):
    """Lists the input files, without the outputs of the previous runs.
    Args:
        pattern (str): The glob pattern (e.g. "*.pdf").
        path_setting (str, optional): The setting file to be excluded.
    Returns:
        list: The paths to be processed.
    """
    files = []
    for path in sorted(glob.glob(pattern)):
        if SUFFIX in os.path.splitext(os.path.basename(path))[0]:
            continue
        if path_setting is not None and os.path.abspath(path) == os.path.abspath(path_setting):
            continue
        files.append(path)
    return files


def main(path_xlsx="highlight_pdf.xlsx"):
    """Reads the setting xlsx and highlights the PDFs in the current directory.
    Args:
        path_xlsx (str): The path to the setting xlsx file.
    Returns:
        int: 0 on success, 1 if the setting cannot be used.
    """
    df = read_excel(path_xlsx)
    if df is None:
        return 1
    df = df.dropna(subset=["keywords", "colors"])
    if len(df) == 0:
        print(f"No keywords in {path_xlsx}")
        input("Press Any Key")
        return 1
    for pdf in input_files("*.pdf"):
        highlight_pdf(pdf, df.keywords, df.colors)
    return 0


if __name__ == "__main__":
    sys.exit(main())
