## Differences across dataset header

library(unmplymnt)

create_comparison_table <- function() {
  g <- names(suppressMessages(glints(limit = 1)))
  j <- names(suppressMessages(jobstreet(limit = 1)))
  k <- names(suppressMessages(kalibrr(limit = 1)))
  n_max <- max(length(g), length(j), length(k))
  return(data.frame(
    glints = c(g, rep(NA_character_, n_max - length(g))),
    jobstreet = c(j, rep(NA_character_, n_max - length(j))),
    kalibrr = c(k, rep(NA_character_, n_max - length(k)))
  ))
}
fields_available <- create_comparison_table()
usethis::use_data(fields_available, overwrite = TRUE)
