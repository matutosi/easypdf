import os
import glob
import pandas as pd
import openpyxl # Need to read xlsx
import fitz     # PyMuPDF

def highlight_pdf(pdf_path, keywords, colors, opacity = 0.3):
    """
    Highlights specified keywords in a PDF file.
    Args:
        pdf_path (str): The path to the PDF file.
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
    out_pdfs = pdf_path.replace(".pdf", "_highlighted.pdf")
    doc = fitz.open(pdf_path)
    for i in range(0, len(keywords)):
        highlight_text(doc, str(keywords[i]), conver_color_name(colors[i]), opacity = 0.3)
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

def conver_color_name(color):
    """
    Converts a color name to its RGB value.
    Args:
        color (str): The name of the color.
    Returns:
        tuple: The RGB value (e.g., (1, 0, 0) for red).
    Example:
        >>> conver_color_name("red")
        (1, 0, 0)
        >>> conver_color_name("unknown")
        (1, 1, 0)  # default to yellow
    """
    COLORS = {
        "purple": (1, 0, 1),
        "yellow": (1, 1, 0),
        "red"   : (1, 0, 0),
        "sky"   : (0, 1, 1),
        "blue"  : (0, 0, 1),
        "green" : (0, 1, 0),
        "gray"  : (0, 0, 0)
    }
    try:
        col = COLORS[color]
    except:
        col = (1, 1, 0)
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

# main

in_pdfs = glob.glob("*.pdf")
df = read_excel("highlight_pdf.xlsx")
keywords = df.keywords
colors = df.colors
for pdf in in_pdfs:
    highlight_pdf(pdf, keywords, colors)
