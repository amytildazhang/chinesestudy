
#' @importFrom dplyr group_by n
#' @importFrom rlang ensym !!
merge_dupes <- function(df, word_col = "word", reading_col = "pinyin",
                        defn_col = "definition") {
  word <- ensym(word_col)
  pyin <- ensym(reading_col)
  defn <- ensym(defn_col)

  df |>
    dplyr::group_by(!!word) |>
    dplyr::summarise(
      !!defn := ifelse(n() > 1,
                       paste(paste(!!pyin, !!defn, sep = "\n"), collapse = "\n===\n"),
                       !!defn),
      !!pyin := paste(!!pyin, collapse = "; ")
    ) |>
    dplyr::select(!!word, !!pyin, !!defn)
}

#' Create separate columns for simplified and traditional characters
#'
#' `anno_both()` combines
#'
#'  - `split_simplified()`: Creates a 'simplified' and 'traditional' column from
#'    the word column.
#'  - `purge_same()`: Removes column entries that are the same, e.g.
#'    `purge_same(purge = "simplified")` removes entries from the
#'    `"simplified"` column that match `"traditional"`.
#'
#'  It also removes the original column in the dataframe with the word and
#'  re-orders the columns to match my custom notetype fields.
#'
#'
#' @param df Dataframe with columns for word, pronunciation, and definition.
#' @param word_col The name of the column with the word.
#' @param brackets One of `"simplified"` or `"traditional"`. When outputting both,
#'   Pleco tends to put simplified first and then traditional in brackets. This
#'   option specifies which is in the brackets.
#' @param purge The column to remove matching entries from, one of `"simplified"`
#' or `"traditional"`.
#' @param reading_col The name of the column with pronunciation, often `"pinyin"`.
#' @param defn_col The name of the column with the definition, often `"definition"`.
#'
#' @return A dataframe with the `reading_col`, `definition_col`, and two new
#'  columns, `"simplified"` and `"traditional"`.
#' @export
anno_both <- function(df, word_col = "word",
                      brackets = "traditional",
                      purge = "simplified",
                      reading_col = "pinyin",
                      defn_col = "definition") {
  types <- c("traditional", "simplified")
  brackets <- match.arg(brackets, types)
  purge <- match.arg(purge, types)
  df <- df |>
    split_simplified(word_col, brackets) |>
    purge_same(cols = types, purge = purge)
  df[c("traditional", reading_col, defn_col, "simplified")]
}

#' @describeIn anno_both Split word column into simplified and traditional.
#' @export
split_simplified <- function(df, word_col = "word", brackets = "traditional") {
  types <- c("simplified", "traditional")
  other <- setdiff(types, brackets)
  df[[brackets]] <- gsub("^[^\\[]+\\[", "", df[[word_col]])
  df[[brackets]] <- gsub("\\]", "", df[[brackets]])
  df[[other]] <- gsub("\\[.+\\]", "", df[[word_col]])
  df
}

#' @describeIn anno_both Remove matching entries from specified purge column.
#' @export
purge_same <- function(df, cols = c("simplified", "traditional"), purge = "simplified") {
  dupes_col <- df[[cols[purge]]]
  df[[cols[purge]]] <- ifelse(dupes_col == df[[cols[-purge]]], "", dupes_col)
  df
}

separate_lists <- function(df, format = "\\d", defn_col = "definition") {
  defn <- df[[defn_col]] |>
    str_replace_all(paste0(" (", format, ")"), "\n\\1")
  df[[defn_col]] <- defn
  df
}

html_highlight <- function(text) {
  paste0("<font size='-2' ")
}


#' Title
#' As in https://resources.allsetlearning.com/chinese/grammar/Part_of_speech.
#' @return
#' @export
#'
#' @examples
chinese_parts <- function() {
  c("noun",
    "verb",
    "preposition",
    "adverb",
    "adjective",
    "auxiliary",
    "measure word")
}

subscript_pleco <- function() {
  c("literary",
    "colloquial",
    "meaningless bound form")
}

pos_abc <- function() {
  c("R\\.F\\." = "REDUPLICATED FORM",
    "S\\.V\\." = "STATIC VERB",
    "V\\.P\\." = "VERB PHRASE",
    "V\\.O\\." = "VERB-OBJECT CONSTRUCTION",
    "P\\.W\\." = "PLACE WORD",
    "B\\.F\\." = "BOUND FORM",
    "F\\.E\\." = "FIXED EXPRESSION",
    "COURT\\." = "COURTEOUS",
    "ATTR\\." = "ATTRIBUTIVE",
    "TOPO\\." = "TOPOLECT",
    "CONS\\." = "CONSTRUCTION",
    "COLL\\." = "COLLOQUIAL",
    "CONJ\\." = "CONJUNCTION",
    "TRAD\\." = "TRADITIONAL",
    "CHAR\\." = "CHARACTER",
    "PREF\\." = "PREFIX",
    "HIST\\." = "HISTORY",
    "THEA\\." = "THEATER",
    "COV\\." = "COVERB",
    "NUM\\." = "NUMBER",
    "ADV\\." = "ADVERB",
    "SUF\\." = "SUFFIX",
    "WR\\." = "WRITING",
    "PR\\." = "PRONOUN",
    "V\\." = "VERB",
    "N\\." = "NOUN",
    "M\\." = "NOMINAL MEASURE WORD")

}
