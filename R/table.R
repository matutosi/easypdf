# 補足: `utils`パッケージ
# Pythonの`utils`モジュールに含まれる関数は、Rに移植する際に`utils.`という接頭辞をつけて呼び出すことを想定しています。
# 例: `utils.snap_objects`, `utils.objects_to_rect`
# これらの関数は、Rのデータ構造（リストやデータフレーム）を扱うように再実装する必要があります。

# 以下のimportは実装済みと仮定
# library(utils)
# library(_typing) # T_bbox, T_num, T_objなど、型定義をRのデータ構造に置き換える

# Pythonのitertoolsの代替
# Rのdplyrやpurrr、base::lapplyなどを使用

# デフォルト設定
DEFAULT_SNAP_TOLERANCE <- 3
DEFAULT_JOIN_TOLERANCE <- 3
DEFAULT_MIN_WORDS_VERTICAL <- 3
DEFAULT_MIN_WORDS_HORIZONTAL <- 1

# スナップと結合
# snap_edges: tolerance_x, tolerance_y でエッジをスナップする
snap_edges <- function(edges, x_tolerance = DEFAULT_SNAP_TOLERANCE, y_tolerance = DEFAULT_SNAP_TOLERANCE) {
  by_orientation <- list(v = list(), h = list())
  for (e in edges) {
    if (e$orientation == "v") {
      by_orientation$v <- append(by_orientation$v, list(e))
    } else {
      by_orientation$h <- append(by_orientation$h, list(e))
    }
  }

  snapped_v <- utils.snap_objects(by_orientation$v, "x0", x_tolerance)
  snapped_h <- utils.snap_objects(by_orientation$h, "top", y_tolerance)
  return(c(snapped_v, snapped_h))
}

# join_edge_group: 同一直線上のエッジを結合する
join_edge_group <- function(edges, orientation, tolerance = DEFAULT_JOIN_TOLERANCE) {
  if (orientation == "h") {
    min_prop <- "x0"
    max_prop <- "x1"
  } else if (orientation == "v") {
    min_prop <- "top"
    max_prop <- "bottom"
  } else {
    stop("Orientation must be 'v' or 'h'")
  }

  sorted_edges <- edges[order(sapply(edges, `[[`, min_prop))]
  
  if (length(sorted_edges) == 0) {
    return(list())
  }
  
  joined <- list(sorted_edges[[1]])
  
  for (e in sorted_edges[-1]) {
    last <- joined[[length(joined)]]
    if (e[[min_prop]] <= (last[[max_prop]] + tolerance)) {
      if (e[[max_prop]] > last[[max_prop]]) {
        joined[[length(joined)]] <- utils.resize_object(last, max_prop, e[[max_prop]])
      }
    } else {
      joined <- append(joined, list(e))
    }
  }
  return(joined)
}

# merge_edges: スナップと結合を組み合わせてエッジをマージする
merge_edges <- function(edges, snap_x_tolerance, snap_y_tolerance, join_x_tolerance, join_y_tolerance) {
  
  get_group <- function(edge) {
    if (edge$orientation == "h") {
      return(paste0("h_", edge$top))
    } else {
      return(paste0("v_", edge$x0))
    }
  }
  
  if (snap_x_tolerance > 0 || snap_y_tolerance > 0) {
    edges <- snap_edges(edges, snap_x_tolerance, snap_y_tolerance)
  }
  
  _sorted <- edges[order(sapply(edges, get_group))]
  edge_groups <- split(_sorted, sapply(_sorted, get_group))
  
  joined_edges_list <- lapply(names(edge_groups), function(k) {
    items <- edge_groups[[k]]
    orientation <- substr(k, 1, 1)
    tolerance <- if (orientation == "h") join_x_tolerance else join_y_tolerance
    join_edge_group(items, orientation, tolerance)
  })
  
  return(unlist(joined_edges_list, recursive = FALSE))
}


# words_to_edges: テキストから仮想的なテーブルの境界線を生成
words_to_edges_h <- function(words, word_threshold = DEFAULT_MIN_WORDS_HORIZONTAL) {
  by_top <- utils.cluster_objects(words, function(x) x$top, 1)
  large_clusters <- Filter(function(x) length(x) >= word_threshold, by_top)

  if (length(large_clusters) == 0) {
    return(list())
  }
  
  rects <- lapply(large_clusters, utils.objects_to_rect)
  min_x0 <- min(sapply(rects, `[[`, "x0"))
  max_x1 <- max(sapply(rects, `[[`, "x1"))

  edges <- list()
  for (r in rects) {
    edges <- append(edges, list(
      list(
        x0 = min_x0, x1 = max_x1, top = r$top, bottom = r$top,
        width = max_x1 - min_x0, orientation = "h"
      ),
      list(
        x0 = min_x0, x1 = max_x1, top = r$bottom, bottom = r$bottom,
        width = max_x1 - min_x0, orientation = "h"
      )
    ))
  }
  return(edges)
}


words_to_edges_v <- function(words, word_threshold = DEFAULT_MIN_WORDS_VERTICAL) {
  get_center <- function(word) (word$x0 + word$x1) / 2
  
  by_x0 <- utils.cluster_objects(words, function(x) x$x0, 1)
  by_x1 <- utils.cluster_objects(words, function(x) x$x1, 1)
  by_center <- utils.cluster_objects(words, get_center, 1)
  clusters <- c(by_x0, by_x1, by_center)
  
  sorted_clusters <- clusters[order(-sapply(clusters, length))]
  large_clusters <- Filter(function(x) length(x) >= word_threshold, sorted_clusters)
  
  bboxes <- lapply(large_clusters, utils.objects_to_bbox)
  
  condensed_bboxes <- list()
  for (bbox in bboxes) {
    overlap <- any(sapply(condensed_bboxes, function(c) utils.get_bbox_overlap(bbox, c)))
    if (!overlap) {
      condensed_bboxes <- append(condensed_bboxes, list(bbox))
    }
  }
  
  if (length(condensed_bboxes) == 0) {
    return(list())
  }
  
  condensed_rects <- lapply(condensed_bboxes, utils.bbox_to_rect)
  sorted_rects <- condensed_rects[order(sapply(condensed_rects, `[[`, "x0"))]
  
  min_top <- min(sapply(sorted_rects, `[[`, "top"))
  max_bottom <- max(sapply(sorted_rects, `[[`, "bottom"))
  max_x1 <- max(sapply(sorted_rects, `[[`, "x1"))

  edges_v <- lapply(sorted_rects, function(b) {
    list(
      x0 = b$x0, x1 = b$x0, top = min_top, bottom = max_bottom,
      height = max_bottom - min_top, orientation = "v"
    )
  })
  
  last_edge <- list(
    x0 = max_x1, x1 = max_x1, top = min_top, bottom = max_bottom,
    height = max_bottom - min_top, orientation = "v"
  )
  
  return(c(edges_v, list(last_edge)))
}

# edges_to_intersections: エッジから交点を検出する
edges_to_intersections <- function(edges, x_tolerance = 1, y_tolerance = 1) {
  intersections <- list()
  v_edges <- Filter(function(x) x$orientation == "v", edges)
  h_edges <- Filter(function(x) x$orientation == "h", edges)
  
  v_edges <- v_edges[order(sapply(v_edges, `[[`, "x0"), sapply(v_edges, `[[`, "top"))]
  h_edges <- h_edges[order(sapply(h_edges, `[[`, "top"), sapply(h_edges, `[[`, "x0"))]
  
  for (v in v_edges) {
    for (h in h_edges) {
      if (
        (v$top <= (h$top + y_tolerance)) && (v$bottom >= (h$top - y_tolerance)) &&
        (v$x0 >= (h$x0 - x_tolerance)) && (v$x0 <= (h$x1 + x_tolerance))
      ) {
        vertex <- paste(v$x0, h$top, sep = "_")
        if (!vertex %in% names(intersections)) {
          intersections[[vertex]] <- list(v = list(), h = list())
        }
        intersections[[vertex]]$v <- append(intersections[[vertex]]$v, list(v))
        intersections[[vertex]]$h <- append(intersections[[vertex]]$h, list(h))
      }
    }
  }
  return(intersections)
}


# intersections_to_cells: 交点からセル（四角形）を検出する
intersections_to_cells <- function(intersections) {
  edge_connects <- function(p1, p2) {
    edges_to_set <- function(edges) {
      sapply(edges, utils.obj_to_bbox)
    }
    
    if (p1[1] == p2[1]) {
      common <- intersect(edges_to_set(intersections[[paste(p1[1], p1[2], sep="_")]]$v),
                          edges_to_set(intersections[[paste(p2[1], p2[2], sep="_")]]$v))
      if (length(common) > 0) return(TRUE)
    }
    
    if (p1[2] == p2[2]) {
      common <- intersect(edges_to_set(intersections[[paste(p1[1], p1[2], sep="_")]]$h),
                          edges_to_set(intersections[[paste(p2[1], p2[2], sep="_")]]$h))
      if (length(common) > 0) return(TRUE)
    }
    return(FALSE)
  }
  
  points <- do.call(rbind, strsplit(names(intersections), "_"))
  points <- matrix(as.numeric(points), ncol = 2)
  points <- points[order(points[,1], points[,2]),]
  
  find_smallest_cell <- function(i) {
    if (i == nrow(points) - 1) return(NULL)
    pt <- points[i,]
    rest <- points[(i+1):nrow(points),]
    below <- rest[rest[,1] == pt[1],,drop=FALSE]
    right <- rest[rest[,2] == pt[2],,drop=FALSE]
    
    if (nrow(below) == 0 || nrow(right) == 0) return(NULL)
    
    for (below_pt_row in 1:nrow(below)) {
      below_pt <- below[below_pt_row,]
      if (!edge_connects(pt, below_pt)) next
      
      for (right_pt_row in 1:nrow(right)) {
        right_pt <- right[right_pt_row,]
        if (!edge_connects(pt, right_pt)) next
        
        bottom_right <- c(right_pt[1], below_pt[2])
        if (
          paste(bottom_right[1], bottom_right[2], sep="_") %in% names(intersections) &&
          edge_connects(bottom_right, right_pt) &&
          edge_connects(bottom_right, below_pt)
        ) {
          return(c(pt[1], pt[2], bottom_right[1], bottom_right[2]))
        }
      }
    }
    return(NULL)
  }
  
  cell_gen <- lapply(1:(nrow(points) - 1), find_smallest_cell)
  cells <- Filter(Negate(is.null), cell_gen)
  return(cells)
}


# cells_to_tables: セルをテーブルにまとめる
cells_to_tables <- function(cells) {
  
  bbox_to_corners <- function(bbox) {
    x0 <- bbox[1]
    top <- bbox[2]
    x1 <- bbox[3]
    bottom <- bbox[4]
    return(list(c(x0, top), c(x0, bottom), c(x1, top), c(x1, bottom)))
  }
  
  remaining_cells <- cells
  current_corners <- list()
  current_cells <- list()
  tables <- list()
  
  while (length(remaining_cells) > 0) {
    initial_cell_count <- length(current_cells)
    
    for (i in seq_along(remaining_cells)) {
      cell <- remaining_cells[[i]]
      cell_corners <- bbox_to_corners(cell)
      
      if (length(current_cells) == 0) {
        current_corners <- union(current_corners, cell_corners)
        current_cells <- append(current_cells, list(cell))
        remaining_cells[[i]] <- NULL
        break 
      } else {
        corner_count <- sum(sapply(cell_corners, function(cc) any(sapply(current_corners, function(c) all(c == cc)))))
        
        if (corner_count > 0) {
          current_corners <- union(current_corners, cell_corners)
          current_cells <- append(current_cells, list(cell))
          remaining_cells[[i]] <- NULL
          break 
        }
      }
    }
    
    if (length(current_cells) == initial_cell_count) {
      tables <- append(tables, list(current_cells))
      current_corners <- list()
      current_cells <- list()
    }
  }
  
  if (length(current_cells) > 0) {
    tables <- append(tables, list(current_cells))
  }
  
  _sorted <- tables[order(sapply(tables, function(t) {
    min_coords <- min(sapply(t, function(c) c[2]))
    min_x_for_min_y <- min(sapply(t, function(c) c[1])[sapply(t, function(c) c[2]) == min_coords])
    return(paste(min_coords, min_x_for_min_y))
  }))]
  
  filtered <- Filter(function(t) length(t) > 1, _sorted)
  return(filtered)
}

# CellGroup, Row, Column, Table クラスのR S3/R6オブジェクトへの移植
# RではPythonのようなクラス定義は一般的ではないため、ここでは簡略化してS3クラスとして表現します。
# より複雑なOOPが必要な場合は、R6やS4クラスを使用することもできます。

# CellGroup
CellGroup <- function(cells) {
  bbox_calc <- function(cells) {
    valid_cells <- Filter(Negate(is.null), cells)
    if (length(valid_cells) == 0) {
      return(NULL)
    }
    c(
      min(sapply(valid_cells, `[[`, 1)),
      min(sapply(valid_cells, `[[`, 2)),
      max(sapply(valid_cells, `[[`, 3)),
      max(sapply(valid_cells, `[[`, 4))
    )
  }
  
  structure(
    list(
      cells = cells,
      bbox = bbox_calc(cells)
    ),
    class = "CellGroup"
  )
}

# RowとColumnはCellGroupの特殊なケースとして表現
Row <- function(cells) {
  obj <- CellGroup(cells)
  class(obj) <- c("Row", class(obj))
  return(obj)
}

Column <- function(cells) {
  obj <- CellGroup(cells)
  class(obj) <- c("Column", class(obj))
  return(obj)
}

# Table
Table <- function(page, cells) {
  _get_rows_or_cols <- function(kind) {
    axis <- if (identical(kind, Row)) 0 else 1
    antiaxis <- as.integer(!axis)

    _sorted <- cells[order(sapply(cells, `[[`, antiaxis + 1), sapply(cells, `[[`, axis + 1))]
    
    grouped <- split(_sorted, sapply(_sorted, `[[`, antiaxis + 1))
    
    result <- lapply(names(grouped), function(y) {
      row_cells <- grouped[[y]]
      xs <- unique(sapply(cells, `[[`, axis + 1))
      xdict <- setNames(row_cells, sapply(row_cells, `[[`, axis + 1))
      row_obj <- kind(lapply(xs, function(x) xdict[[as.character(x)]]))
      return(row_obj)
    })
    
    return(result)
  }
  
  extract_content <- function(...) {
    kwargs <- list(...)
    chars <- page$chars
    
    char_in_bbox <- function(char, bbox) {
      v_mid <- (char$top + char$bottom) / 2
      h_mid <- (char$x0 + char$x1) / 2
      x0 <- bbox[1]
      top <- bbox[2]
      x1 <- bbox[3]
      bottom <- bbox[4]
      
      return((h_mid >= x0) && (h_mid < x1) && (v_mid >= top) && (v_mid < bottom))
    }
    
    rows <- _get_rows_or_cols(Row)
    table_arr <- lapply(rows, function(row) {
      arr <- list()
      row_chars <- Filter(function(char) char_in_bbox(char, row$bbox), chars)
      
      for (cell in row$cells) {
        if (is.null(cell)) {
          arr <- append(arr, list(NULL))
        } else {
          cell_chars <- Filter(function(char) char_in_bbox(char, cell), row_chars)
          if (length(cell_chars) > 0) {
            local_kwargs <- kwargs
            if ("layout" %in% names(local_kwargs)) {
              local_kwargs$layout_width <- cell[3] - cell[1]
              local_kwargs$layout_height <- cell[4] - cell[2]
              local_kwargs$layout_bbox <- cell
            }
            cell_text <- do.call(utils.extract_text, c(list(cell_chars), local_kwargs))
            arr <- append(arr, list(cell_text))
          } else {
            arr <- append(arr, list(""))
          }
        }
      }
      return(arr)
    })
    
    return(table_arr)
  }
  
  obj <- list(
    page = page,
    cells = cells,
    bbox = c(
      min(sapply(cells, `[[`, 1)),
      min(sapply(cells, `[[`, 2)),
      max(sapply(cells, `[[`, 3)),
      max(sapply(cells, `[[`, 4))
    ),
    rows = function() _get_rows_or_cols(Row),
    columns = function() _get_rows_or_cols(Column),
    extract = extract_content
  )
  class(obj) <- "Table"
  return(obj)
}


# TableSettings, TableFinder
TABLE_STRATEGIES <- c("lines", "lines_strict", "text", "explicit")
UNSET <- -1 # Rで表現できる'unset'値

# TableSettings
TableSettings <- function(
  vertical_strategy = "lines", horizontal_strategy = "lines",
  explicit_vertical_lines = NULL, explicit_horizontal_lines = NULL,
  snap_tolerance = DEFAULT_SNAP_TOLERANCE, snap_x_tolerance = UNSET,
  snap_y_tolerance = UNSET, join_tolerance = DEFAULT_JOIN_TOLERANCE,
  join_x_tolerance = UNSET, join_y_tolerance = UNSET,
  edge_min_length = 3, min_words_vertical = DEFAULT_MIN_WORDS_VERTICAL,
  min_words_horizontal = DEFAULT_MIN_WORDS_HORIZONTAL,
  intersection_tolerance = 3, intersection_x_tolerance = UNSET,
  intersection_y_tolerance = UNSET, text_settings = list()
) {
  
  if (!is.null(text_settings) && "tolerance" %in% names(text_settings)) {
    if (!("x_tolerance" %in% names(text_settings))) {
      text_settings$x_tolerance <- text_settings$tolerance
    }
    if (!("y_tolerance" %in% names(text_settings))) {
      text_settings$y_tolerance <- text_settings$tolerance
    }
    text_settings$tolerance <- NULL
  }
  
  settings <- list(
    vertical_strategy = vertical_strategy, horizontal_strategy = horizontal_strategy,
    explicit_vertical_lines = explicit_vertical_lines, explicit_horizontal_lines = explicit_horizontal_lines,
    snap_tolerance = snap_tolerance, snap_x_tolerance = snap_x_tolerance,
    snap_y_tolerance = snap_y_tolerance, join_tolerance = join_tolerance,
    join_x_tolerance = join_x_tolerance, join_y_tolerance = join_y_tolerance,
    edge_min_length = edge_min_length, min_words_vertical = min_words_vertical,
    min_words_horizontal = min_words_horizontal,
    intersection_tolerance = intersection_tolerance,
    intersection_x_tolerance = intersection_x_tolerance, intersection_y_tolerance = intersection_y_tolerance,
    text_settings = text_settings
  )
  
  NON_NEGATIVE_SETTINGS <- c(
    "snap_tolerance", "snap_x_tolerance", "snap_y_tolerance", "join_tolerance",
    "join_x_tolerance", "join_y_tolerance", "edge_min_length", "min_words_vertical",
    "min_words_horizontal", "intersection_tolerance", "intersection_x_tolerance", "intersection_y_tolerance"
  )
  
  for (setting in NON_NEGATIVE_SETTINGS) {
    if (settings[[setting]] < 0 && settings[[setting]] != UNSET) {
      stop(paste0("Table setting '", setting, "' cannot be negative"))
    }
  }
  
  for (orientation in c("horizontal", "vertical")) {
    strategy <- settings[[paste0(orientation, "_strategy")]]
    if (!strategy %in% TABLE_STRATEGIES) {
      stop(paste0(orientation, "_strategy must be one of {", paste(TABLE_STRATEGIES, collapse = ","), "}"))
    }
  }
  
  for (attr in c("snap_x_tolerance", "snap_y_tolerance")) {
    if (settings[[attr]] == UNSET) {
      settings[[attr]] <- settings$snap_tolerance
    }
  }
  for (attr in c("join_x_tolerance", "join_y_tolerance")) {
    if (settings[[attr]] == UNSET) {
      settings[[attr]] <- settings$join_tolerance
    }
  }
  for (attr in c("intersection_x_tolerance", "intersection_y_tolerance")) {
    if (settings[[attr]] == UNSET) {
      settings[[attr]] <- settings$intersection_tolerance
    }
  }
  
  return(structure(settings, class = "TableSettings"))
}

# resolve_table_settings: Pythonのクラスメソッドを関数として移植
resolve_table_settings <- function(settings_obj) {
  if (is.null(settings_obj)) {
    return(TableSettings())
  } else if (inherits(settings_obj, "TableSettings")) {
    return(settings_obj)
  } else if (is.list(settings_obj)) {
    core_settings <- list()
    text_settings <- list()
    for (k in names(settings_obj)) {
      if (startsWith(k, "text_")) {
        text_settings[[substring(k, 6)]] <- settings_obj[[k]]
      } else {
        core_settings[[k]] <- settings_obj[[k]]
      }
    }
    core_settings$text_settings <- text_settings
    return(do.call(TableSettings, core_settings))
  } else {
    stop(paste("Cannot resolve settings:", settings_obj))
  }
}

# TableFinder
TableFinder <- function(page, settings = NULL) {
  
  settings <- resolve_table_settings(settings)
  
  get_edges <- function() {
    
    for (orientation in c("vertical", "horizontal")) {
      strategy <- settings[[paste0(orientation, "_strategy")]]
      if (strategy == "explicit") {
        lines <- settings[[paste0("explicit_", orientation, "_lines")]]
        if (length(lines) < 2) {
          stop(paste0(
            "If ", orientation, "_strategy == 'explicit', ",
            "explicit_", orientation, "_lines must be specified as a list/vector of two or more floats/ints."
          ))
        }
      }
    }
    
    v_strat <- settings$vertical_strategy
    h_strat <- settings$horizontal_strategy
    
    if (v_strat == "text" || h_strat == "text") {
      words <- do.call(page$extract_words, settings$text_settings %||% list())
    }
    
    v_explicit <- lapply(settings$explicit_vertical_lines %||% list(), function(desc) {
      if (is.list(desc)) {
        res <- lapply(utils.obj_to_edges(desc), function(e) if (e$orientation == "v") e)
        return(Filter(Negate(is.null), res))
      } else {
        return(list(
          x0 = desc, x1 = desc, top = page$bbox[2], bottom = page$bbox[4],
          height = page$bbox[4] - page$bbox[2], orientation = "v"
        ))
      }
    })
    v_explicit <- unlist(v_explicit, recursive = FALSE)
    
    if (v_strat == "lines") {
      v_base <- utils.filter_edges(page$edges, "v")
    } else if (v_strat == "lines_strict") {
      v_base <- utils.filter_edges(page$edges, "v", edge_type = "line")
    } else if (v_strat == "text") {
      v_base <- words_to_edges_v(words, word_threshold = settings$min_words_vertical)
    } else if (v_strat == "explicit") {
      v_base <- list()
    }
    
    v <- c(v_base, v_explicit)
    
    h_explicit <- lapply(settings$explicit_horizontal_lines %||% list(), function(desc) {
      if (is.list(desc)) {
        res <- lapply(utils.obj_to_edges(desc), function(e) if (e$orientation == "h") e)
        return(Filter(Negate(is.null), res))
      } else {
        return(list(
          x0 = page$bbox[1], x1 = page$bbox[3], width = page$bbox[3] - page$bbox[1],
          top = desc, bottom = desc, orientation = "h"
        ))
      }
    })
    h_explicit <- unlist(h_explicit, recursive = FALSE)
    
    if (h_strat == "lines") {
      h_base <- utils.filter_edges(page$edges, "h")
    } else if (h_strat == "lines_strict") {
      h_base <- utils.filter_edges(page$edges, "h", edge_type = "line")
    } else if (h_strat == "text") {
      h_base <- words_to_edges_h(words, word_threshold = settings$min_words_horizontal)
    } else if (h_strat == "explicit") {
      h_base <- list()
    }
    
    h <- c(h_base, h_explicit)
    
    edges <- c(v, h)
    
    edges <- merge_edges(
      edges,
      snap_x_tolerance = settings$snap_x_tolerance,
      snap_y_tolerance = settings$snap_y_tolerance,
      join_x_tolerance = settings$join_x_tolerance,
      join_y_tolerance = settings$join_y_tolerance
    )
    
    return(utils.filter_edges(edges, min_length = settings$edge_min_length))
  }
  
  edges <- get_edges()
  intersections <- edges_to_intersections(
    edges,
    settings$intersection_x_tolerance,
    settings$intersection_y_tolerance
  )
  cells <- intersections_to_cells(intersections)
  tables <- lapply(cells_to_tables(cells), function(cell_group) {
    Table(page, cell_group)
  })
  
  return(list(
    page = page,
    settings = settings,
    edges = edges,
    intersections = intersections,
    cells = cells,
    tables = tables
  ))
}

