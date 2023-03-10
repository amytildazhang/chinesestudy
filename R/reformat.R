#' Title
#'
#' @param df
#' @param defn_col
#' @param pos_dict
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_replace_all
reformat_abc <- function(df,
                         pos_dict = pos_abc(),
                         defn_col = "definition") {
  defn <- df[[defn_col]]
  if (!is.null(names(pos_dict))) {
    for (pos in names(pos_dict)) {
      repl <- pos_dict[pos] # replace abbreviated form with full form
      defn <- defn |>
        # replace abbrev part-of-speech name with full name
        str_replace_all(pos, repl) |>
        # add line break after part-of-speech to separate from definition
        # some definitions have multiple p-o-s labels; those are usually
        # separted by a comma, so look for those p-o-s that are not followed
        # by a comma
        # # also filter out cases where it is already followed by a line break
        str_replace_all(paste0(repl, "[^\\n,]"), paste0(repl, "\n")) |>
        # part-of-speech name always comes first, the one exception is if
        # one word can be used in multiple ways (e.g., as noun and as adjective)
        # look for those cases and add a demarcating line between them
        str_replace_all(paste0("[^A-Z,] ", repl), paste0("\n---\n", repl)) |>
        # for nouns, the ABC dictionary also gives the measure word
        # this is indicated by `M: [measure word]` at the end of the definition
        # add a line break before `M: `
        str_replace_all(" M:", "\nM:")
    }
  }
  df$definition <- defn
  df

}
