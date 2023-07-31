#' Restructuring List to Dataframe
#'
#' @param jobs A list of job result from \code{gql()} function
#'
#' @return A data.frame of job in tibble format
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
#' @importFrom lubridate ymd
#'
restruct_job <- function(jobs) {
  extract_jobid <- function(item) {
    return(item[["job_id"]])
  }
  extract_jobtitle <- function(item) {
    return(str_squish(item[["job_title"]]))
  }
  extract_item <- function(item, elem, name) {
    return(item[[elem]][[name]])
  }
  extract_items <- function(item, elem, name) {
    elements <- tryCatch(
      list(
        sapply(item[[elem]], function(x) {
          x[[name]]
        })
      ),
      error = list()
    )
    return(elements)
  }
  extract_joburl <- function(item) {
    baseurl <- "https://www.jobstreet.co.id/id/job/"
    return(paste0(baseurl, item[["job_id"]]))
  }
  extract_date_published <- function(item) {
    date_published <- tryCatch(
      ymd(
        str_replace(
          string = item[["posted_at"]],
          pattern = "^(\\d{4}-\\d{2}-\\d{2}).+$",
          replacement = "\\1"
        )
      ),
      error = NA_integer_
    )
    return(date_published)
  }
  construct <- function(item) {
    return(
      tibble(
        job_id = extract_jobid(item),
        job_title = extract_jobtitle(item),
        company = extract_item(item, "company", "name"),
        salary_max = extract_item(item, "salary", "max"),
        salary_min = extract_item(item, "salary", "min"),
        salary_currency = extract_item(item, "salary", "currency"),
        salary_period = extract_item(item, "salary", "period"),
        category = extract_items(item, "category", "name"),
        employment_type = extract_items(item, "employment", "type"),
        city = extract_items(item, "city", "name"),
        posted_at = extract_date_published(item),
        job_url = extract_joburl(item),
        source = "Jobstreet",
        country = "Indonesia",
        is_remote = NA,
      )
    )
  }
  arrange_col <- function(df) {
    select(
      df,
      "job_title",
      "company",
      "city",
      "country",
      "is_remote",
      "category",
      "salary_currency",
      "salary_min",
      "salary_max",
      "salary_period",
      "employment_type",
      "posted_at",
      "source",
      "job_url",
      "job_id"
    )
  }

  vacancies <- map(jobs, function(x) construct(x))
  vacancies <- do.call(bind_rows, vacancies)
  vacancies <- arrange_col(vacancies)
  return(vacancies)

}
