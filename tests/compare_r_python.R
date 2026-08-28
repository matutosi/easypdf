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
# 空のリストでも character(0) を返す (表の無い PDF で落ちないように)
keys <- function(x, f) if (length(x) == 0) character(0) else sort(vapply(x, f, character(1)))

cat("== 入力 ==\n")
cat(sprintf("  pdf   : %s (page %s)\n", py$source$pdf, py$source$page))
cat(sprintf("  edges : raw %d -> final %d\n", length(py$edges_raw), length(edges_final)))
cat("== 段階ごとの比較 ==\n")

# 1. エッジの整理 (入力は Python の生のエッジ)
#    filter_edges -> merge_edges -> filter_edges(min_length) の順は TableFinder と同じ
edges_raw <- as_edges(py$edges_raw)
r_edges <- try({
  v <- filter_edges(edges_raw, "v")
  h <- filter_edges(edges_raw, "h")
  merged <- merge_edges(c(v, h), snap_x_tolerance = 3, snap_y_tolerance = 3,
                        join_x_tolerance = 3, join_y_tolerance = 3)
  filter_edges(merged, min_length = 3)
}, silent = TRUE)
if (inherits(r_edges, "try-error")) {
  record("edges", FALSE, sub("\n.*", "", conditionMessage(attr(r_edges, "condition"))))
} else {
  r_e <- keys(r_edges, function(e) key_bbox(obj_to_bbox(e)))
  p_e <- keys(edges_final, function(e) key_bbox(obj_to_bbox(e)))
  record("edges", identical(r_e, p_e),
         sprintf("R %d / Python %d, 共通 %d",
                 length(r_e), length(p_e), length(intersect(r_e, p_e))))
}

# 2. edges_to_intersections (入力は Python の edges_final)
r_ints <- try(edges_to_intersections(edges_final, 3, 3), silent = TRUE)
if (inherits(r_ints, "try-error")) {
  record("intersections", FALSE, sub("\n.*", "", conditionMessage(attr(r_ints, "condition"))))
  r_ints <- NULL
} else {
  nm <- names(r_ints)
  if (is.null(nm)) nm <- character(0)
  r_keys <- keys(strsplit(nm, "_"), function(p) key_xy(p[1], p[2]))
  p_keys <- keys(py$intersections, function(p) key_xy(p[[1]], p[[2]]))
  record("intersections", identical(r_keys, p_keys),
         sprintf("R %d / Python %d, 共通 %d",
                 length(r_keys), length(p_keys), length(intersect(r_keys, p_keys))))
}

# 3. intersections_to_cells (入力は R 自身の交点．無ければ飛ばす)
r_cells <- NULL
if (!is.null(r_ints)) {
  r_cells <- try(intersections_to_cells(r_ints), silent = TRUE)
  if (inherits(r_cells, "try-error")) {
    record("cells", FALSE, sub("\n.*", "", conditionMessage(attr(r_cells, "condition"))))
    r_cells <- NULL
  } else {
    r_k <- keys(r_cells, key_bbox)
    p_k <- keys(py$cells, function(c) key_bbox(unlist(c)))
    record("cells", identical(r_k, p_k),
           sprintf("R %d / Python %d, 共通 %d",
                   length(r_k), length(p_k), length(intersect(r_k, p_k))))
  }
} else {
  record("cells", FALSE, "前段が失敗したため未実行")
}

# 4. cells_to_tables (入力は Python の cells にそろえて，この段階だけを見る)
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

# 5. TableFinder を通しで動かす (lines 戦略なので page は edges と bbox だけでよい)
page <- list(edges = edges_raw, bbox = unlist(py$page_bbox))
r_finder <- try(TableFinder(page), silent = TRUE)
if (inherits(r_finder, "try-error")) {
  record("TableFinder", FALSE,
         sub("\n.*", "", conditionMessage(attr(r_finder, "condition"))))
} else {
  n_cells <- as.numeric(sapply(r_finder$tables, function(t) length(t$cells)))
  p_cells <- as.numeric(sapply(py$tables, function(t) t$n_cells))
  record("TableFinder",
         length(r_finder$tables) == length(py$tables) && identical(n_cells, p_cells),
         sprintf("R %d 表 (セル %s) / Python %d 表 (セル %s)",
                 length(r_finder$tables), paste(n_cells, collapse = "/"),
                 length(py$tables), paste(p_cells, collapse = "/")))
}

# 6. extract_words (入力は Python の chars)
chars <- lapply(py$chars, function(c) c[!sapply(c, is.null)])
r_words <- try(extract_words(chars), silent = TRUE)
if (inherits(r_words, "try-error")) {
  record("words", FALSE,
         sub("\n.*", "", conditionMessage(attr(r_words, "condition"))))
} else {
  key_word <- function(w) paste(w$text, key_bbox(obj_to_bbox(w)), sep = "@")
  r_w <- keys(r_words, key_word)
  p_w <- keys(py$words, key_word)
  record("words", identical(r_w, p_w),
         sprintf("R %d / Python %d, 共通 %d",
                 length(r_w), length(p_w), length(intersect(r_w, p_w))))
}

cat("== まとめ ==\n")
n_ok <- sum(sapply(results, `[[`, "ok"))
cat(sprintf("  %d / %d 段階が一致\n", n_ok, length(results)))
if (n_ok < length(results)) quit(status = 1)
