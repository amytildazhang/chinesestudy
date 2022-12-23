anki_headers <- function(separator = "Semicolon",
                         html = "false",
                         tags = NULL,
                         columns = "front;reading;back",
                         notetype = "Chinese",
                         deck = "tianguancifu",
                         notetype_col = NULL,
                         deck_col = NULL,
                         tags_col = NULL,
                         guid_col = NULL) {
  deck <- match.arg(deck, anki_decks(), several.ok = FALSE)
  args <- list(
    separator = separator,
    html = html,
    tags = tags,
    columns = columns,
    notetype = notetype,
    deck = deck,
    notetype_col = notetype_col,
    deck_col = deck_col,
    tags_col = tags_col,
    guid_col = guid_col
  )

  provided <- !vapply(args, is.null, logical(1L))
  args <- args[provided]

  paste0("#", names(args), ":", args)
}


#' Save processed flashcards to an Anki deck
#'
#'
#'
#' Requirements:
#'   - deck must already exist as a `.txt` file in the `decks` folder
#'
#' @param df
#' @param deck
#' @param header
#'
#' @return
#' @importFrom fs file_exists path
#' @importFrom readr write_delim write_lines
#' @importFrom data.table fread
#' @export
#'
#' @examples
save_anki <- function(df, deck, append = FALSE, ...) {
  deck     <- match.arg(deck, anki_decks(), several.ok = FALSE)
  save_loc <- anki_deck_path(deck)



  if ( !append ) {
    anki_create_deck(deck, overwrite = TRUE, verbose = FALSE, ...)
  }

  # get separator to use from the file header
  sep <- system(paste("grep '#separator:'", save_loc),  intern = TRUE)
  sep <- gsub("#separator:", "", sep)
  delim <- switch(sep,
                  "Semicolon" = ";",
                  "Comma" = ",",
                  "Tab" = "\t",
                  "Pipe" = "|",
                  "Space" = " ",
                  "Colon" = ":")

  write_delim(df, save_loc, delim = delim, na = "", append = TRUE)
}

#' Title
#'
#' @param deck
#'
#' @return
#' @export
#'
#' @examples
anki_create_deck <- function(deck, overwrite = FALSE, verbose = TRUE, ...) {
  if ( !overwrite && deck %in% anki_decks() && verbose) {
    warning("Deck already exists; will not make a new one.", call. = FALSE)
  } else {
    header <- anki_headers(deck = deck, ...)
    write_lines(header, anki_deck_path(deck))
  }
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom fs dir_ls path_ext_remove path_file
anki_decks <- function() {
  dir_ls("decks", type = "file", regexp = "[.]txt$") |>
    path_file() |>
    path_ext_remove()
}

anki_deck_path <- function(deck) {
  path("decks", paste0(deck, ".txt"))
}
