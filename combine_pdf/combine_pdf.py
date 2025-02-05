import os
import pandas as pd
import openpyxl
from pypdf import PdfWriter

def extract_file_names(df):
    """Extract unique output file names and corresponding input file names from a DataFrame.
    Args:
        df: A Pandas DataFrame with 'outputs' and 'inputs' columns.  The 'outputs'
            column represents the output file names, and the 'inputs' column
            represents the corresponding input file names.
    Returns:
        A list of lists.  Each inner list has the format:
        `[output_file_name, array_of_input_file_names]`.  The `output_file_name`
        is a string, and `array_of_input_file_names` is a NumPy array of strings.
        Returns an empty list if the input DataFrame is empty or doesn't contain
        the necessary columns.
    """
    nr, nc = df.shape
    output = df.loc[:,"outputs"].unique()
    result = []
    for out in output:
        inp = df.query('outputs==@out').loc[:,'inputs'].unique()
        out = [out, inp]
        result.append(out)
    return(result)

def pdf_combine(input_pdfs, output_pdf):
    """Combines multiple PDF files into a single PDF.
    Args:
        input_pdfs: A list of paths to the input PDF files.
        output_pdf: The path to the output PDF file.
    Returns:
        The path to the output PDF file (or None if an error occurs).
        Raises FileNotFoundError if an input PDF doesn't exist.
    """
    writer = PdfWriter()
    try:
        for pdf_path in input_pdfs:
            if not os.path.exists(pdf_path):
                raise FileNotFoundError(f"Input PDF '{pdf_path}' not found.")
                input("Press Any Key")
            with open(pdf_path, "rb") as pdf_file:  # "rb": read mode
                writer.append(pdf_file)
    except FileNotFoundError as e:
        print(f"File Not Found Error: {e}")
        input("Press Any Key")
        return None
    except Exception as e:
        print(f"An error occurred during PDF merging: {e}")
        input("Press Any Key")
        return None
    try:
        with open(output_pdf, "wb") as output_file: # "wb": write mode
            writer.write(output_file)
        writer.close()
        return output_pdf
    except Exception as e:
        print(f"An error occurred while writing the combined PDF: {e}")
        return None

# main
try:
    df = pd.read_excel("combine_pdf.xlsx")
except FileNotFoundError as e:
    print(f"File Not Found Error: {e}")
    input("Press Any Key")
except Exception as e:
    print(f"Error: {e}")
    input("Press Any Key")

files = extract_file_names(df) 

for f in files:
    print("combining")
    print(f[1])
    print("    generating: " + f[0])
    pdf_combine(f[1], f[0])
