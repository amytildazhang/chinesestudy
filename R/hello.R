#' Read in and convert Pleco flashcards to Anki format
#'
#' Takes the ".txt" output from Pleco's exported flashcards and
#' converts it into a readable `.csv` format for Anki.
#'
#' - `read_pleco()`: Reads in the Pleco export, auto-assigns column names.
#'   Assumes file is in iCloud Drive unless told otherwise.
#'
#' - `convert_pleco()`:
#'
#' @param filename Name of the file exported from Pleco.
#' @param loc Directory of the file exported from Pleco.
#'
#' @return A dataframe with columns "word", "pinyin", and "definition."
#' @importFrom rlang %||%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' df <- read_pleco("flash.txt")
#' df
#'
read_pleco <- function(filename, loc = NULL) {
  # assumes saved in iCloud drive unless loc is provided
  loc <- loc %||%  "~/Library/Mobile\ Documents/com~apple~CloudDocs/"

  df  <- read_tsv(
    paste0(loc, filename),
    col_names = c("word", "pinyin", "definition"),
    col_types = "ccc",
    comment   = "//"
  )

  unique(df[c("word", "pinyin", "definition")])
}


#' @describeIn read_pleco Convert Pleco data to readable format
#'
#' @return A dataframe with formatted text for flashcards.
#' @export
#'
#' @examples
#' convert_pleco(df)
convert_pleco <- function(df) {
  df$definition <- gsub("( )(\\d)", " \n\\2", df$definition)
  df
}
