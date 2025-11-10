PythonのpdfplumberをRに移植したい
  # https://github.com/jsvine/pdfplumber

table.pyとutils.pyの関数を移植すればOKかと思ったが，ちょっと無理っぽい
class設計なども必要だから

with pdfplumber.open(FILENAME) as pdf:
    for page in pdf.pages:
        page.extract_tables()
