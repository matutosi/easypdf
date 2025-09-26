# R言語でのユーティリティ関数（pdfplumberのutils/text.py, utils/clustering.py, utils/geometry.py 相当）

# T_num: numeric (int or double)
# T_obj: named list (Python dict)
# T_bbox: numeric vector of length 4, c(x0, top, x1, bottom)

# --- 定数と設定 ---

DEFAULT_X_TOLERANCE <- 3
DEFAULT_Y_TOLERANCE <- 3
DEFAULT_X_DENSITY <- 7.25
DEFAULT_Y_DENSITY <- 13
DEFAULT_LINE_DIR <- "ttb"
DEFAULT_CHAR_DIR <- "ltr"

LIGATURES <- list(
  "ﬀ" = "ff",
  "ﬃ" = "ffi",
  "ﬄ" = "ffl",
  "ﬁ" = "fi",
  "ﬂ" = "fl",
  "ﬆ" = "st",
  "ﬅ" = "st"
)

# --- geometry.R の機能 ---

# bbox_getter: Pythonのoperator.itemgetter("x0", "top", "x1", "bottom") に相当
# Rではリストの要素名を直接指定してアクセスするのが一般的ですが、ここではヘルパー関数を定義
.bbox_getter_keys <- c("x0", "top", "x1", "bottom")

# オブジェクトからバウンディングボックスを取得
objects_to_bbox <- function(objects) {
  if (length(objects) == 0) {
    # ページ全体またはデフォルトの空のbboxを返す必要あり
    return(c(0, 0, 0, 0)) # ダミー値
  }
  
  # リストからx0, top, x1, bottomを抽出
  x0 <- sapply(objects, function(o) o$x0)
  top <- sapply(objects, function(o) o$top)
  x1 <- sapply(objects, function(o) o$x1)
  bottom <- sapply(objects, function(o) o$bottom)
  
  # 最小のx0, topと最大のx1, bottomを結合
  c(min(x0), min(top), max(x1), max(bottom))
}

# bbox_to_rect: T_bbox (vector) を T_obj (list) に変換
bbox_to_rect <- function(bbox) {
  list(x0 = bbox[1], top = bbox[2], x1 = bbox[3], bottom = bbox[4])
}

# --- clustering.R の機能 ---

# 数値リストをクラスタリング
cluster_list <- function(xs, tolerance = 0) {
  if (tolerance == 0 || length(xs) < 2) {
    return(lapply(sort(xs), list))
  }
  
  xs <- sort(unique(xs))
  groups <- list()
  current_group <- xs[1]
  last <- xs[1]
  
  for (i in 2:length(xs)) {
    x <- xs[i]
    if (x <= (last + tolerance)) {
      current_group <- c(current_group, x)
    } else {
      groups <- c(groups, list(current_group))
      current_group <- x
    }
    last <- x
  }
  groups <- c(groups, list(current_group))
  
  # 結果をリストのリストとして返す
  lapply(groups, function(g) as.list(g))
}

# クラスタリング結果をルックアップディクショナリ（リスト）に変換
make_cluster_dict <- function(values, tolerance) {
  clusters <- cluster_list(unique(values), tolerance)
  
  cluster_map <- list()
  for (i in 1:length(clusters)) {
    for (val in unlist(clusters[[i]])) {
      # Rのリストキーは文字列なので、valを文字列に変換する
      cluster_map[[as.character(val)]] <- i - 1 # 0-indexed
    }
  }
  return(cluster_map)
}

# オブジェクト（リスト）をクラスタリング
cluster_objects <- function(xs, key_fn, tolerance, preserve_order = FALSE) {
  
  # key_fnが文字列（キー名）か関数のどちらかを受け入れる
  if (is.character(key_fn)) {
    key_fn_impl <- function(x) x[[key_fn]]
  } else if (is.function(key_fn)) {
    key_fn_impl <- key_fn
  } else {
    stop("key_fn must be a character key name or a function.")
  }
  
  # 1. key_fnで値を抽出し、クラスタマップを作成
  values <- sapply(xs, key_fn_impl)
  cluster_dict <- make_cluster_dict(values, tolerance)
  
  # 2. オブジェクトにクラスタIDを付与
  cluster_tuples <- lapply(xs, function(x) {
    val <- key_fn_impl(x)
    # クラスタIDはmake_cluster_dictで文字列キーを持つリストとして保存されている
    cluster_id <- cluster_dict[[as.character(val)]]
    list(obj = x, cluster_id = cluster_id)
  })
  
  # 3. クラスタIDでソート（または順序を維持）
  if (!preserve_order) {
    cluster_tuples <- cluster_tuples[order(sapply(cluster_tuples, function(t) t$cluster_id))]
  }
  
  # 4. クラスタIDでグループ化
  grouped <- split(cluster_tuples, sapply(cluster_tuples, function(t) t$cluster_id))
  
  # 5. グループから元のオブジェクトを抽出
  result <- lapply(grouped, function(g) {
    lapply(g, function(t) t$obj)
  })
  
  # 名前（クラスタID）を削除してリストのリストとして返す
  unname(result)
}

# --- text.R の主要機能 ---

# ユーティリティ関数: 方向キーに基づいた値抽出関数を返す
get_line_cluster_key <- function(line_dir) {
  key_map <- list(
    "ttb" = function(x) x$top,
    "btt" = function(x) -x$bottom,
    "ltr" = function(x) x$x0,
    "rtl" = function(x) -x$x1
  )
  key_map[[line_dir]]
}

get_char_sort_key <- function(char_dir) {
  key_map <- list(
    # (主軸の開始位置, 副軸の終了/開始位置) のタプルに相当
    "ttb" = function(x) c(x$top, x$bottom), 
    "btt" = function(x) c(-(x$top + x$height), -x$top),
    "ltr" = function(x) c(x$x0, x$x0), # Rではタプルではないが、ソートキーとしてベクトルを使用
    "rtl" = function(x) c(-x$x1, -x$x0)
  )
  key_map[[char_dir]]
}

# Rでは、Pythonのitemgetterの代わりに、キー名（文字列）で直接リストにアクセスする
POSITION_KEYS <- list(
  "ttb" = "top",
  "btt" = "bottom",
  "ltr" = "x0",
  "rtl" = "x1"
)

# BBOX_ORIGIN_KEYS: Pythonのitemgetter(index) に相当
# 0=x0, 1=top, 2=x1, 3=bottom
.BBOX_ORIGIN_KEYS_INDEX <- list(
  "ttb" = 2, # top
  "btt" = 4, # bottom
  "ltr" = 1, # x0
  "rtl" = 3  # x1
)

# BBOXから原点（x0, topなど）を取得するヘルパー関数
get_bbox_origin <- function(bbox, dir) {
  index <- .BBOX_ORIGIN_KEYS_INDEX[[dir]]
  bbox[index]
}


validate_directions <- function(line_dir, char_dir, suffix = "") {
  valid_dirs <- names(POSITION_KEYS)
  if (!(line_dir %in% valid_dirs)) {
    stop(paste0("line_dir", suffix, " must be one of ", paste(valid_dirs, collapse = ", "), ", not ", line_dir))
  }
  if (!(char_dir %in% valid_dirs)) {
    stop(paste0("char_dir", suffix, " must be one of ", paste(valid_dirs, collapse = ", "), ", not ", char_dir))
  }
  # line_dirとchar_dirが直交していることを確認 (例: ttb/ltr はOK, ttb/btt はNG)
  if ((line_dir == "ttb" && char_dir == "btt") || (line_dir == "btt" && char_dir == "ttb") ||
      (line_dir == "ltr" && char_dir == "rtl") || (line_dir == "rtl" && char_dir == "ltr") ||
      (line_dir == char_dir)) {
    stop(paste0("line_dir", suffix, "=", line_dir, " is incompatible with char_dir", suffix, "=", char_dir))
  }
}


# --- WordExtractor クラス（Rではコンストラクタ関数とリストで実装）---

WordExtractor <- function(
    x_tolerance = DEFAULT_X_TOLERANCE, y_tolerance = DEFAULT_Y_TOLERANCE,
    x_tolerance_ratio = NULL, y_tolerance_ratio = NULL,
    keep_blank_chars = FALSE, use_text_flow = FALSE,
    vertical_ttb = TRUE, horizontal_ltr = TRUE,
    line_dir = DEFAULT_LINE_DIR, char_dir = DEFAULT_CHAR_DIR,
    line_dir_rotated = NULL, char_dir_rotated = NULL,
    extra_attrs = NULL, split_at_punctuation = FALSE,
    expand_ligatures = TRUE
) {
  
  if (is.null(line_dir_rotated)) line_dir_rotated <- char_dir
  if (is.null(char_dir_rotated)) char_dir_rotated <- line_dir
  
  # 方向のバリデーション
  validate_directions(line_dir, char_dir)
  validate_directions(line_dir_rotated, char_dir_rotated, "_rotated")
  
  # split_at_punctuation の処理
  split_punc <- if (isTRUE(split_at_punctuation)) {
    # Rのbaseパッケージにはstring.punctuationに相当するものがないため、手動で定義または省略
    # ここでは、一般的な記号のセットを定義
    "\\!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  } else if (is.character(split_at_punctuation)) {
    split_at_punctuation
  } else {
    ""
  }
  
  # インスタンスとしてのプロパティをリストで返す
  obj <- list(
    x_tolerance = x_tolerance, y_tolerance = y_tolerance,
    x_tolerance_ratio = x_tolerance_ratio, y_tolerance_ratio = y_tolerance_ratio,
    keep_blank_chars = keep_blank_chars, use_text_flow = use_text_flow,
    line_dir = line_dir, char_dir = char_dir,
    line_dir_rotated = line_dir_rotated, char_dir_rotated = char_dir_rotated,
    extra_attrs = if (is.null(extra_attrs)) character(0) else extra_attrs,
    split_at_punctuation = split_punc,
    expansions = if (expand_ligatures) LIGATURES else list(),
    # Rでメソッドを定義するために、WordExtractorのリストに直接関数を組み込む
    get_char_dir = function(upright) {
      if (upright) obj$char_dir else obj$char_dir_rotated
    },
    merge_chars = function(ordered_chars) {
      # Pythonのmerge_charsロジックをRに変換
      bbox <- objects_to_bbox(ordered_chars)
      x0 <- bbox[1]; top <- bbox[2]; x1 <- bbox[3]; bottom <- bbox[4]
      
      first_char <- ordered_chars[[1]]
      doctop_adj <- first_char$doctop - first_char$top
      upright <- first_char$upright
      char_dir <- obj$get_char_dir(upright)
      
      text <- paste(sapply(ordered_chars, function(c) {
        obj$expansions[[c$text]] %||% c$text
      }), collapse = "")
      
      word <- list(
        text = text, x0 = x0, x1 = x1, top = top,
        doctop = top + doctop_adj, bottom = bottom,
        upright = upright,
        height = bottom - top, width = x1 - x0,
        direction = char_dir
      )
      
      for (key in obj$extra_attrs) {
        word[[key]] <- first_char[[key]]
      }
      return(word)
    },
    char_begins_new_word = function(prev_char, curr_char, direction, x_tolerance, y_tolerance) {
      
      # Pythonの char_begins_new_word ロジックをRに変換
      if (direction %in% c("ltr", "rtl")) {
        x <- x_tolerance
        y <- y_tolerance
        ay <- prev_char$top
        cy <- curr_char$top
        if (direction == "ltr") {
          ax <- prev_char$x0
          bx <- prev_char$x1
          cx <- curr_char$x0
        } else { # rtl
          ax <- -prev_char$x1
          bx <- -prev_char$x0
          cx <- -curr_char$x1
        }
      } else { # ttb, btt
        x <- y_tolerance # x/y toleranceは非回転テキストの方向を基準に反転
        y <- x_tolerance
        ay <- prev_char$x0
        cy <- curr_char$x0
        if (direction == "ttb") {
          ax <- prev_char$top
          bx <- prev_char$bottom
          cx <- curr_char$top
        } else { # btt
          ax <- -prev_char$bottom
          bx <- -prev_char$top
          cx <- -curr_char$bottom
        }
      }
      
      # Intraline test: 新しい文字が前の文字より開始位置が前か、または許容範囲を超えて離れている
      intraline_test <- (cx < ax) || (cx > bx + x)
      # Interline test: Y方向（直交方向）の位置が許容範囲を超えてずれている
      interline_test <- abs(cy - ay) > y
      
      return(intraline_test || interline_test)
    },
    
    # Rでジェネレータ（Generator）は直接実装できないため、ここではリストを返す関数とする
    iter_chars_to_words = function(ordered_chars, direction) {
      words_list <- list()
      current_word <- list()
      
      start_next_word <- function(new_char) {
        nonlocal current_word
        if (length(current_word) > 0) {
          words_list <<- c(words_list, list(current_word))
        }
        current_word <<- if (is.null(new_char)) list() else list(new_char)
      }
      
      # Rでは代入演算子`<-`がローカルスコープ、`<<-`が親スコープ（`start_next_word`の外）に代入
      # Pythonのnonlocalの振る舞いを模倣するために`<<-`を使用
      
      for (char in ordered_chars) {
        text <- char$text
        
        # x/y tolerance の動的計算
        xt_calc <- obj$x_tolerance
        if (!is.null(obj$x_tolerance_ratio) && length(current_word) > 0) {
          xt_calc <- obj$x_tolerance_ratio * current_word[[length(current_word)]]$size
        }
        yt_calc <- obj$y_tolerance
        if (!is.null(obj$y_tolerance_ratio) && length(current_word) > 0) {
          yt_calc <- obj$y_tolerance_ratio * current_word[[length(current_word)]]$size
        }
        
        if (!obj$keep_blank_chars && grepl("^\\s+$", text)) { # text.isspace()
          start_next_word(NULL)
        } else if (grepl(paste0("[", obj$split_at_punctuation, "]"), text)) {
          start_next_word(char)
          start_next_word(NULL)
        } else if (length(current_word) > 0 && obj$char_begins_new_word(
          current_word[[length(current_word)]], char, direction,
          x_tolerance = xt_calc, y_tolerance = yt_calc
        )) {
          start_next_word(char)
        } else {
          current_word <- c(current_word, list(char))
        }
      }
      
      # 最後のワードを出力
      if (length(current_word) > 0) {
        words_list <- c(words_list, list(current_word))
      }
      return(words_list)
    },
    
    iter_chars_to_lines = function(chars) {
      chars <- as.list(chars)
      if (length(chars) == 0) return(list())
      
      first_char <- chars[[1]]
      upright <- first_char$upright
      line_dir <- if (upright) obj$line_dir else obj$line_dir_rotated
      char_dir <- obj$get_char_dir(upright)
      
      line_cluster_key <- get_line_cluster_key(line_dir)
      char_sort_key <- get_char_sort_key(char_dir)
      
      # Y/X-toleranceを決定
      tolerance <- obj$y_tolerance
      if (line_dir %in% c("ltr", "rtl")) tolerance <- obj$x_tolerance
      
      subclusters <- cluster_objects(chars, line_cluster_key, tolerance)
      
      # 各ライン（サブクラスタ）内で文字をソートし、結果をリストで返す
      lines_list <- lapply(subclusters, function(sc) {
        # ソートキー関数がベクトルを返す場合、Rのsort/order関数は辞書式順序でソートする
        chars_sorted <- sc[order(sapply(sc, char_sort_key)[1, ], sapply(sc, char_sort_key)[2, ])]
        list(chars = chars_sorted, direction = char_dir)
      })
      
      return(lines_list)
    },
    
    iter_extract_tuples = function(chars) {
      chars <- as.list(chars)
      tuples <- list()
      
      grouping_keys <- c("upright", obj$extra_attrs)
      
      # RのsplitとlapplyでPythonのitertools.groupbyを模倣
      # 1. grouping_keysに基づいて文字をグループ化
      group_factors <- apply(sapply(chars, function(c) sapply(grouping_keys, function(k) c[[k]])), 2, paste, collapse = "__")
      grouped_chars <- split(chars, group_factors)
      
      for (group_name in names(grouped_chars)) {
        char_group <- grouped_chars[[group_name]]
        
        if (obj$use_text_flow) {
          line_groups <- list(list(chars = char_group, direction = obj$char_dir))
        } else {
          line_groups <- obj$iter_chars_to_lines(char_group)
        }
        
        for (line_group in line_groups) {
          line_chars <- line_group$chars
          direction <- line_group$direction
          
          word_chars_list <- obj$iter_chars_to_words(line_chars, direction)
          
          for (word_chars in word_chars_list) {
            word <- obj$merge_chars(word_chars)
            tuples <- c(tuples, list(list(word = word, chars = word_chars)))
          }
        }
      }
      return(tuples)
    },
    
    extract_wordmap = function(chars) {
      tuples <- obj$iter_extract_tuples(chars)
      # WordMapクラスの代わりに、単なるリスト構造を返す
      list(tuples = tuples, to_textmap = function(...) {
        # TextMapのロジックが複雑なため、ここでは実装を省略
        stop("WordMap$to_textmap is not fully implemented in this R conversion.")
      })
    },
    
    extract_words = function(chars, return_chars = FALSE) {
      tuples <- obj$iter_extract_tuples(chars)
      if (return_chars) {
        # wordリストにchars要素を追加して返す
        return(lapply(tuples, function(t) {
          c(t$word, list(chars = t$chars))
        }))
      } else {
        return(lapply(tuples, function(t) t$word))
      }
    }
  )
  
  # Rでオブジェクト（リスト）に関数（メソッド）を組み込む
  class(obj) <- "WordExtractor"
  return(obj)
}

# 外部から呼び出される extract_words 関数
extract_words <- function(chars, return_chars = FALSE, ...) {
  # ... から WordExtractor の引数を抽出
  args <- list(...)
  extractor_args <- args[names(args) %in% c("x_tolerance", "y_tolerance", "x_tolerance_ratio", "y_tolerance_ratio", "keep_blank_chars", "use_text_flow", "line_dir", "char_dir", "line_dir_rotated", "char_dir_rotated", "extra_attrs", "split_at_punctuation", "expand_ligatures")]
  
  extractor <- do.call(WordExtractor, extractor_args)
  extractor$extract_words(chars, return_chars)
}

# 簡略化されたテキスト抽出 (layout=FALSE の基本ロジック)
extract_text_simple <- function(chars, x_tolerance = DEFAULT_X_TOLERANCE, y_tolerance = DEFAULT_Y_TOLERANCE) {
  
  # 行のクラスタリング
  # itemgetter("doctop")に相当: x$doctop
  clustered <- cluster_objects(chars, key_fn = "doctop", tolerance = y_tolerance)
  
  # 行内の文字を結合
  collate_line <- function(line_chars, tolerance) {
    coll <- ""
    last_x1 <- NULL
    
    # x0でソート
    line_chars_sorted <- line_chars[order(sapply(line_chars, function(c) c$x0))]
    
    for (char in line_chars_sorted) {
      if (!is.null(last_x1) && (char$x0 > (last_x1 + tolerance))) {
        coll <- paste0(coll, " ")
      }
      last_x1 <- char$x1
      coll <- paste0(coll, char$text)
    }
    return(coll)
  }
  
  # 各クラスタに対して collate_line を適用し、改行で結合
  lines <- sapply(clustered, collate_line, tolerance = x_tolerance)
  
  return(paste(lines, collapse = "\n"))
}

# Rでは、extract_text_simple が最も簡単なテキスト抽出として機能する
extract_text <- function(chars, layout = FALSE, ...) {
  if (layout) {
    # TextMapの複雑なロジックを必要とするため、ここでは非推奨または未実装とする
    # より高度なレイアウト処理が必要な場合は、Rでグリッドレイアウトライブラリを使用する必要がある
    stop("Layout-based text extraction is complex and not fully implemented in this R conversion. Use layout=FALSE or extract_text_simple.")
  }
  # layout=FALSE の場合は、WordExtractorを使用して単語を抽出し、行にクラスタリングする
  # ただし、extract_text_simpleが最も簡単な代替案である
  
  # WordExtractorを使用して単語を抽出し、それを結合
  
  extractor <- WordExtractor(...)
  words <- extractor$extract_words(chars)
  
  if (length(words) == 0) return("")
  
  line_dir_render <- list(...)$line_dir_render %||% extractor$line_dir
  
  line_cluster_key <- get_line_cluster_key(line_dir_render)
  
  # y_tolerance/x_toleranceの取得（デフォルト値の考慮）
  y_tolerance <- list(...)$y_tolerance %||% DEFAULT_Y_TOLERANCE
  x_tolerance <- list(...)$x_tolerance %||% DEFAULT_X_TOLERANCE
  
  # 単語を行にクラスタリング
  tolerance <- y_tolerance
  if (line_dir_render %in% c("ltr", "rtl")) tolerance <- x_tolerance
  
  lines <- cluster_objects(words, line_cluster_key, tolerance)
  
  # 行と単語を結合
  text_lines <- lapply(lines, function(line) {
    paste(sapply(line, function(word) word$text), collapse = " ")
  })
  
  return(paste(unlist(text_lines), collapse = "\n"))
}

# Rでは、Pythonの `or` に相当する安全なデフォルト値代入ヘルパー
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# ---
# dedupe_chars の実装（cluster_objectsを使用）
dedupe_chars <- function(chars, tolerance = 1, extra_attrs = c("fontname", "size")) {
  
  # 文字を key (upright, text, extra_attrs) でソートし、グループ化
  grouping_keys <- c("upright", "text", extra_attrs)
  group_factors <- apply(sapply(chars, function(c) sapply(grouping_keys, function(k) c[[k]])), 2, paste, collapse = "__")
  grouped_chars <- split(chars, group_factors)
  
  deduped_chars <- list()
  pos_keys <- c("doctop", "x0") # ソートキー
  
  for (group_name in names(grouped_chars)) {
    grp_chars <- grouped_chars[[group_name]]
    
    # doctopでクラスタリング
    y_clusters <- cluster_objects(grp_chars, "doctop", tolerance)
    
    for (y_cluster in y_clusters) {
      # x0でクラスタリング
      x_clusters <- cluster_objects(y_cluster, "x0", tolerance)
      
      for (x_cluster in x_clusters) {
        # 各x_clusterから最初の文字（最も小さいdoctop, x0）を選択
        # Rで複合キーソートを簡略化するため、applyでソートキーを結合し、orderでソート
        sort_values <- sapply(x_cluster, function(c) c(c$doctop, c$x0))
        # sort_values[1,] が doctop, sort_values[2,] が x0
        first_char_index <- order(sort_values[1, ], sort_values[2, ])[1]
        
        deduped_chars <- c(deduped_chars, x_cluster[first_char_index])
      }
    }
  }
  
  # 元のリスト順に戻す（Pythonのchars.indexに基づくソートを模倣）
  # 効率的ではないが、元の動作を模倣
  
  # 元のcharsからの一意なID/ハッシュを作成（ここでは簡略化のため、元のリストの順序を直接使用）
  # Rで元の順序を維持しつつ重複を排除するには、元のリスト内のインデックスを利用する
  
  # 元のcharsのオブジェクトを文字列化してハッシュを作成することで、元の順序を維持しつつ重複排除後のリストに存在する要素のみを抽出
  original_char_hashes <- sapply(chars, function(c) paste(unlist(c[c("doctop", "x0", "text", extra_attrs)]), collapse = "|"))
  deduped_char_hashes <- sapply(deduped_chars, function(c) paste(unlist(c[c("doctop", "x0", "text", extra_attrs)]), collapse = "|"))
  
  unique_sorted_chars <- list()
  seen_hashes <- c()
  
  for (i in 1:length(chars)) {
    hash <- original_char_hashes[i]
    if (hash %in% deduped_char_hashes && !(hash %in% seen_hashes)) {
      unique_sorted_chars <- c(unique_sorted_chars, chars[i])
      seen_hashes <- c(seen_hashes, hash)
    }
  }
  
  return(unique_sorted_chars)
}
