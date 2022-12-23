
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
