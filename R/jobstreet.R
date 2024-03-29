#' Jobstreet Vacancy
#'
#' @description Get job vacancy from Jobstreet's website (Seek ID).
#' @param key Characters indicate the keyword for job search
#' @param limit Number of limit for job results
#'
#' @examples
#' \dontrun{
#' jobstreet("data analyst") # return search result for data analyst
#' jobstreet("data engineer", 10) # return 10 data engineer jobs
#' }
#'
#' @import dplyr
#' @importFrom jsonlite toJSON
#' @importFrom purrr map map_df
#' @export
#'
jobstreet <- function(key, limit = 30L) {
  if (missing(key)) key <- set_default_key("data analyst")
  page <- seq(1L, ceiling(limit / 30L), 1L)
  country <- "id"
  url <- sprintf(
    paste0(
      "https://xapi.supercharge-srp.co/job-search/graphql?",
      "country=%s&isSmartSearch=true"
    ),
    country
  )
  set_variables <- function(page) {
    toJSON(
      list(keyword = key,
           jobFunctions = list(),
           locations = list(),
           salaryType = 1,
           jobTypes = list(),
           careerLevels = list(),
           page = page,
           country = country,
           categories = list(),
           workTypes = list(),
           industries = list(),
           locale = "id"
      ),
      auto_unbox = TRUE
    )
  }
  var <- sapply(page, function(p) set_variables(p))
  querypath <- list.files(
    system.file("extdata/graphql", package = "unmplymnt"),
    pattern = "jobstreet",
    full.names = TRUE
  )
  query <- paste(readLines(querypath), collapse = "")
  message(
    sprintf(
      "Pulling job data from Jobstreet (Seek %s)...",
      toupper(country)
    )
  )
  jobs <- map(var, function(x) gql(query = query, var = x, url = url))
  jobs <- map(jobs, function(x) x$jobs$jobs)
  vacancy <- map_df(jobs, function(x) restruct_job(x))
  vacancy <- distinct(vacancy)[1:limit, ]
  return(vacancy)
}
