#' Set default keyword message
#'
#' Supply key argument
#' @param key Characters indicate the keyword for job search
#'
set_default_key <- function(key) {
  message(sprintf('Argument "key" is missing, using default: "%s"', key))
  key
}
