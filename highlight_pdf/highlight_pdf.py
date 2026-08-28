import os
import glob
import pandas as pd
import openpyxl # Need to read xlsx
import fitz     # PyMuPDF
from PIL import ImageColor

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
        out_pdfs = out_path(path_pdf, "_highlighted")
        doc = fitz.open(path_pdf)
    else: # streamlit
        out_pdfs = out_path(path_pdf.name, "_highlighted")
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
    print(f'{color=}')
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
        color (str): The name of the color.
    Returns:
        tuple: The RGB value (e.g., (1, 0, 0) for red).
    Example:
        >>> convert_color_name("red")
        (1, 0, 0)
        >>> convert_color_name("unknown")
        (1, 1, 0)  # default to yellow
    """
    COLORS = {
        "white" : (1, 1, 1),
        "purple": (1, 0, 1),
        "yellow": (1, 1, 0),
        "red"   : (1, 0, 0),
        "sky"   : (0, 1, 1),
        "blue"  : (0, 0, 1),
        "green" : (0, 1, 0),
        "gray"  : (0, 0, 0)
    }
    if color[0] == "#":
        rgb_color = ImageColor.getcolor(color, "RGB")
        return tuple(c / 255.0 for c in rgb_color)
    try:
        return COLORS[color]
    except:
        return (1, 1, 0)

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

if __name__ == "__main__":

    input_pdfs = glob.glob("*.pdf")
    df = read_excel("highlight_pdf.xlsx")
    keywords = df.keywords
    colors = df.colors
    for pdf in input_pdfs:
        highlight_pdf(pdf, keywords, colors)
