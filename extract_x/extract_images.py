import io
import os
import math
import numpy as np
import fitz
from PIL import Image

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


def get_digit(x):
    n_digit = math.ceil(np.log10(x + 1))
    return n_digit

def extract_imgs(path):
    pdf = fitz.open(path)
    xrefs = []
    pages = []
    for i in range(pdf.page_count):
        xref = pdf.get_page_images(i)
        if len(xref) > 0:
            xrefs.append(xref)
            pages.append(i)
    if pages:
        save_imgs(pdf, path, pages, xrefs)
    pdf.close()
    return len(pages)

def save_imgs(pdf, path, pages, xrefs, out_dir="images"):
    if not pages:
        return
    dig_p = get_digit(max(pages) + 1)
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    for page, xref in zip(pages, xrefs):
        dig_i = get_digit(len(xref))
        for i, ref in enumerate(xref, start = 1):
            img = pdf.extract_image(ref[0])
            ext = '.' + img['ext']
            suffix = '_' + str(page + 1).zfill(dig_p) + '_' + str(i).zfill(dig_i)
            path_img = out_path(path, suffix, ext, out_dir)
            img_bytes = img["image"]
            img = Image.open(io.BytesIO(img_bytes))
            img.save(path_img)

"""
path_pdf = 'README.pdf'
extract_imgs(path_pdf)
"""
