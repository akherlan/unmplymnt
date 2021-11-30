#' Job ID (category) for Glints vacancy
#'
#' @return Job ID
#' @export
#'
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#'
glints_jobid <- function() {

  category <- fromJSON("https://glints.com/api/jobCategories")
  category <- category$data %>%
    select("id", "name", "descriptionMarkdown") %>%
    as_tibble() %>%
    arrange(id) %>%
    clean_names()
  return(category)

}
