import io
import os
import math
import numpy as np
import fitz
from PIL import Image

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
    save_imgs(pdf, path, pages, xrefs)
    pdf.close()

def save_imgs(pdf, path, pages, xrefs, out_dir="images"):
    file = os.path.basename(path)
    dig_p = get_digit(max(pages))
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    for page, xref in zip(pages, xrefs):
        dig_i = get_digit(len(xref))
        for i, ref in enumerate(xref, start = 1):
            img = pdf.extract_image(ref[0])
            ext = '.' + img['ext']
            path_img = out_dir + '/' + file + '_' + str(page + 1).zfill(dig_p) + '_' + str(i).zfill(dig_i) + ext
            img_bytes = img["image"]
            img = Image.open(io.BytesIO(img_bytes))
            img.save(path_img)

"""
path_pdf = 'README.pdf'
extract_imgs(path_pdf)
"""
