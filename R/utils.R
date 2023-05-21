tag_hsk <- function(df, word_cols = c("simplified", "traditional"), list_dir = "wordlists", new_column="tag") {
  df$tag <- NA_character_
  for (file in fs::dir_ls(list_dir)) {
    batch <- readLines(file)
    mask <- apply(df[word_cols], c(1,2), \(x) x %in% batch)
    tag_entries <- unique(row(df[word_cols])[mask])
    df$tag[tag_entries] <- fs::path_file(file)
  }
  df
}
