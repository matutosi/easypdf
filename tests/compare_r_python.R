# R 移植版 (R/table.R) と Python の pdfplumber の表抽出を段階ごとに突き合わせる．
#
# 先に tests/dump_pdfplumber.py を実行して JSON を作っておく．
#   python tests/dump_pdfplumber.py
#   Rscript tests/compare_r_python.R
#
# 段階ごとに Python の出力を入力として与えるので，どの段階で食い違うかが分かる．
# 落ちた段階は FAIL として記録し，そこで止めずに次の段階へ進む．

suppressMessages({
  library(jsonlite)
  source("R/utils.R")
  source("R/table.R")
})

path_json <- commandArgs(TRUE)[1]
if (is.na(path_json)) path_json <- "tests/fixtures/mtcars_page1.json"
py <- fromJSON(path_json, simplifyVector = FALSE)

results <- list()
record <- function(stage, ok, detail) {
  results[[length(results) + 1]] <<- list(stage = stage, ok = ok, detail = detail)
  cat(sprintf("%-22s %s  %s\n", stage, if (ok) "OK  " else "FAIL", detail))
}

# JSON のエッジを R 側の形 (名前付きリストのリスト) へそろえる
as_edges <- function(x) lapply(x, function(e) e[!sapply(e, is.null)])
edges_final <- as_edges(py$edges_final)

# 座標の集合を比べるための整形 (丸め誤差を吸収する)
key_xy <- function(x, y) paste(round(as.numeric(x), 3), round(as.numeric(y), 3), sep = ",")
key_bbox <- function(b) paste(round(as.numeric(b), 3), collapse = ",")

cat("== 入力 ==\n")
cat(sprintf("  pdf   : %s (page %s)\n", py$source$pdf, py$source$page))
cat(sprintf("  edges : raw %d -> final %d\n", length(py$edges_raw), length(edges_final)))
cat("== 段階ごとの比較 ==\n")

# 1. edges_to_intersections (入力は Python の edges_final)
r_ints <- try(edges_to_intersections(edges_final, 3, 3), silent = TRUE)
if (inherits(r_ints, "try-error")) {
  record("intersections", FALSE, sub("\n.*", "", conditionMessage(attr(r_ints, "condition"))))
  r_ints <- NULL
} else {
  r_keys <- sort(sapply(strsplit(names(r_ints), "_"), function(p) key_xy(p[1], p[2])))
  p_keys <- sort(sapply(py$intersections, function(p) key_xy(p[[1]], p[[2]])))
  record("intersections", identical(r_keys, p_keys),
         sprintf("R %d / Python %d, 共通 %d",
                 length(r_keys), length(p_keys), length(intersect(r_keys, p_keys))))
}

# 2. intersections_to_cells (入力は R 自身の交点．無ければ飛ばす)
r_cells <- NULL
if (!is.null(r_ints)) {
  r_cells <- try(intersections_to_cells(r_ints), silent = TRUE)
  if (inherits(r_cells, "try-error")) {
    record("cells", FALSE, sub("\n.*", "", conditionMessage(attr(r_cells, "condition"))))
    r_cells <- NULL
  } else {
    r_k <- sort(sapply(r_cells, key_bbox))
    p_k <- sort(sapply(py$cells, function(c) key_bbox(unlist(c))))
    record("cells", identical(r_k, p_k),
           sprintf("R %d / Python %d, 共通 %d",
                   length(r_k), length(p_k), length(intersect(r_k, p_k))))
  }
} else {
  record("cells", FALSE, "前段が失敗したため未実行")
}

# 3. cells_to_tables (入力は Python の cells にそろえて，この段階だけを見る)
p_cells <- lapply(py$cells, function(c) as.numeric(unlist(c)))
r_tables <- try(cells_to_tables(p_cells), silent = TRUE)
if (inherits(r_tables, "try-error")) {
  record("tables", FALSE, sub("\n.*", "", conditionMessage(attr(r_tables, "condition"))))
} else {
  n_py <- length(py$tables)
  ok <- length(r_tables) == n_py &&
    all(mapply(function(rt, pt) length(rt) == pt$n_cells, r_tables, py$tables))
  record("tables", ok,
         sprintf("R %d 表 (セル %s) / Python %d 表 (セル %s)",
                 length(r_tables), paste(sapply(r_tables, length), collapse = "/"),
                 n_py, paste(sapply(py$tables, function(t) t$n_cells), collapse = "/")))
}

cat("== まとめ ==\n")
n_ok <- sum(sapply(results, `[[`, "ok"))
cat(sprintf("  %d / %d 段階が一致\n", n_ok, length(results)))
if (n_ok < length(results)) quit(status = 1)
