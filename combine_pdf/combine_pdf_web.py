"""
Tools to combine multiple PDF files into one file with use streamlit and pypdf. 

- upload PDF files with st.file_uploader   
- show file names with st.text_area   
- user can change order of files   
- combine PDF files according to the order of st.text_area   
- convert each page of combined PDF file into multiple png files using PyMuPdf  
- show png thumbnails with st.image
"""

import datetime
from pypdf import PdfWriter
import fitz  # PyMuPdf
import streamlit as st
import cv2

settings = st.sidebar

with settings:
    # upload PDF files with st.file_uploader
    files_pdf = st.file_uploader("Upload PDF files", type=["pdf"], accept_multiple_files=True)

    if files_pdf:
        # show file names with st.text_area
        file_names = [file.name for file in files_pdf]
        sort_files = st.checkbox("Sort by file names", value=True)
        if sort_files:
            file_names.sort()
        pdf_dict = dict(zip(file_names, files_pdf))

        height = max(70, len(file_names) * 32)
        file_names_str = "\n".join(file_names)
        
        # user can change order of files
        file_names_new = st.text_area("Order of files", value=file_names_str, height=height).splitlines()
    
        # save uploaded PDF files to disk
        for file_pdf in files_pdf:
            with open(file_pdf.name, "wb") as f:
                f.write(file_pdf.getbuffer())

        # combine PDF files according to the order of st.text_area
        pdf_writer = PdfWriter()
        for file_name in file_names_new:
            pdf_writer.append(open(file_name, "rb"))

        # save combined PDF file
        now = datetime.datetime.now().strftime("%Y-%m-%d-%H-%M-%S")
        combined_pdf_path = f'combined_{now}.pdf'
        with open(combined_pdf_path, "wb") as f:
            pdf_writer.write(f)

        image_width = st.slider(label="Width of thumbnails", min_value=50, max_value=500, value=100)

if files_pdf:

    # download combined PDF file
    st.download_button("Download combined PDF file", data=open(combined_pdf_path, "rb"), file_name=combined_pdf_path)
    # convert each page of combined PDF file into multiple png files using PyMuPdf
    pdf_document = fitz.open(combined_pdf_path)
    imgs = [f'page_{p}.png' for p in range(len(pdf_document))]
    for p, img in enumerate(imgs):
        page = pdf_document[p]
        pix = page.get_pixmap()
        pix.save(img)
    
    # show png thumbnails with st.image
    imgs = [cv2.imread(img) for img in imgs]
    imgs = [cv2.resize(img, (image_width, int(img.shape[0] * image_width / img.shape[1]))) for img in imgs]
    img_combined = cv2.vconcat(imgs)
    st.image(img_combined)
