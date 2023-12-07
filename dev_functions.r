add_common_name <- function(
  taxa, 
  taxa_level,
  common = "empty", 
  db = "names_common.csv"
) {
  library(tidyverse, quietly = TRUE)
  if (length(taxa_level) != length(taxa)) {
    stop("species and taxa_level msut be vectors of same length")
  }
  if (common == "empty") common <- rep(NA_character_, length(taxa))
  if (length(taxa) == length(common)) {
    df <- read_csv(db, show_col_types = FALSE)
    tx <- df$taxa
    tx_lvl <- df$taxa_level
    cmn <- df$common
    tx2 <- c(tx, taxa)
    tx_lvl2 <- c(tx_lvl, taxa_level)
    cmn2 <- c(cmn, common)
    data.frame(taxa = tx2, taxa_level = tx_lvl2, common = cmn2) |> 
      write_csv(db)
  } else {
    stop("taxa must be same length as common (unless common='empty')")
  }
}
fill_in_next_common_name <- function(db = "names_common.csv") {
  library(tidyverse)
  df <- read.csv(db)
  missing <- df |> 
    dplyr::filter(is.na(common))
  tx <- missing[1, "taxa"]
  tx_lvl <- missing[1, "taxa_level"]
  get_input <- TRUE
  while (get_input) {
    cat(paste0("\n", str_to_title(tx_lvl), " name:          ", tx))
    cat("\nEnter common name:     ")
    cmn <- readLines("stdin", n = 1)
    message(paste("\n          Common name:", cmn))
    cat("\nIs this correct (Y/y)? ")
    yn <- readLines("stdin", n = 1)
    if (tolower(yn) == "y") {
      cat("\n☺\n")
      cmn
      get_input <- FALSE
    } else {
      cat("\n☹")
    }
  }
  df |> 
    # rowwise() |> 
    mutate(common = case_when(
        taxa == tx & taxa_level == tx_lvl ~ cmn, 
        TRUE ~ common
      )
    ) |> 
    write_csv(db)
}
fill_missing_common_name <- function() {

}
