import os
import sys
import pandas as pd
import openpyxl # Need to read xlsx
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
    output = df.loc[:,"outputs"].unique()
    result = []
    for out in output:
        inp = df.query('outputs==@out').loc[:,'inputs'].unique()
        out = [out, inp]
        result.append(out)
    return(result)

def combine_pdf(input_pdfs, output_pdf):
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

def main(path_xlsx="combine_pdf.xlsx"):
    """Reads the setting xlsx and combines PDFs as it says.
    Args:
        path_xlsx: The path to the setting xlsx file.
    Returns:
        0 on success, 1 if the setting file cannot be read.
    """
    try:
        df = pd.read_excel(path_xlsx)
    except FileNotFoundError as e:
        print(f"File Not Found Error: {e}")
        input("Press Any Key")
        return 1
    except Exception as e:
        print(f"Error: {e}")
        input("Press Any Key")
        return 1

    df = df.dropna(subset=["inputs", "outputs"])
    for output_pdf, input_pdfs in extract_file_names(df):
        print("combining")
        print(input_pdfs)
        print("    generating: " + output_pdf)
        combine_pdf(input_pdfs, output_pdf)
    return 0


if __name__ == "__main__":
    sys.exit(main())
