import os
import streamlit as st
import glob
import pandas as pd
import openpyxl # Need to read xlsx
import fitz     # PyMuPDF
import highlight_pdf

# st.link_button(label="How to use", url="https://github.com/matutosi/convex/tree/main/python")

uploaded_file = st.file_uploader("Upload excel a file", type="pdf", accept_multiple_files=False)
keywords = st.text_input("Input keyword(s) to be highlighted", placeholder="separate by ; (e.g., kywd1;kywd2)")
colors = st.color_picker("Select a color", "#FFFF00")
if keywords != "":
    keywords = keywords.split(";")
    colors = [colors] * len(keywords)

if uploaded_file is not None and keywords != "":
    path_highlighted = highlight_pdf.highlight_pdf(uploaded_file, keywords, colors)
    st.download_button('DOWNLOAD HIGHLIGHTED FILE', open(path_highlighted, 'br'), path_highlighted)
