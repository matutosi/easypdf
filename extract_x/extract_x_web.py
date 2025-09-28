import os
import glob
import pandas as pd
import shutil

import streamlit as st
import pdfplumber

from extract_tables import pdf_tables2zip_csv
# import extract_images
# import extract_text

uploaded_files = st.file_uploader("Upload excel a file", type="pdf", accept_multiple_files=True)

if uploaded_files != []:
    zip_file = pdf_tables2zip_csv(uploaded_files)
    st.download_button('DOWNLOAD TABLES', open(zip_file, 'br'), zip_file)
