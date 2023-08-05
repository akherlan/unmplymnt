#' Glints Vacancy
#'
#' @description Get job vacancy from Glints' website.
#' @param key Characters indicate the keyword for job search
#' @param limit Number of limit for job results
#'
#' @examples
#' \dontrun{
#' glints("data analyst", 15) # return data analyst job
#' }
#'
#' @import dplyr
#' @importFrom purrr map map_df
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_title str_squish str_replace str_remove
#'
#' @export
#'


glints <- function(key, limit = 30L) {
  if (missing(key)) key <- set_default_key("data analyst")
  generate_variable <- function(key, country, limit, offset) {
    vardata <- toJSON(
      list(
        data = list(
          SearchTerm = key,
          CountryCode = country,
          limit = limit,
          offset = offset,
          includeExternalJobs = FALSE,
          sources = "NATIVE"
        )
      ),
      auto_unbox = TRUE
    )
    return(vardata)
  }
  extract_jobid <- function(item) {
    return(item$job_id)
  }
  extract_jobtitle <- function(item) {
    return(str_squish(item$job_title))
  }
  extract_remote <- function(item) {
    return(item$is_remote)
  }
  extract_company <- function(item) {
    return(item$company$name)
  }
  extract_category <- function(item) {
    return(item$category$name)
  }
  extract_country <- function(item) {
    return(item$country$name)
  }
  extract_city <- function(item) {
    if (length(item$city) == 1) {
      city <- item$city$name
    } else {
      city <- paste(
        as.vector(sapply(item$city, function(x) x$name)),
        collapse = ", "
      )
    }
    return(city)
  }
  extract_date_published <- function(item) {
    date_published <- tryCatch(
      ymd(
        str_replace(
          string = item$posted_at,
          pattern = "^(\\d{4}-\\d{2}-\\d{2}).+$",
          replacement = "\\1"
        )
      ),
      error = NA_integer_
    )
    return(date_published)
  }
  extract_salary <- function(item) {
    salary <- unlist(item$salary)
    if (!is.null(salary)) {
      salary <- as_tibble(t(salary))
    } else {
      salary <- as_tibble(
        t(sapply(
          c(
            "salary_period",
            "salary_currency",
            "salary_max",
            "salary_min",
            "salary_type"
          ),
          function(x) NA_character_
        ))
      )
    }
    return(salary)
  }
  extract_source <- function(item) {
    return(
      paste(
        "Glints",
        str_replace(item$source, "_", " ") %>%
          str_to_title()
      )
    )
  }
  extract_joburl <- function(item) {
    paste0("https://glints.com/id/opportunities/jobs/", item$job_id)
  }
  construct <- function(item) {
    bind_cols(
      tibble(
        job_title = extract_jobtitle(item),
        job_id = extract_jobid(item),
        is_remote = extract_remote(item),
        company = extract_company(item),
        category = extract_category(item),
        posted_at = extract_date_published(item),
        country = extract_country(item),
        city = extract_city(item),
        source = extract_source(item),
        job_url = extract_joburl(item)
      ),
      extract_salary(item)
    )
  }
  arrange_col <- function(df) {
    return(
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
        "salary_type",
        # "employment_type",
        "posted_at",
        "source",
        "job_url",
        "job_id"
      )
    )
  }
  # GraphQL requests
  url <- "https://glints.com/api/graphql"
  opnam <- "searchJobs"
  country <- "ID"
  querypath <- list.files(
    system.file("extdata/graphql", package = "unmplymnt"),
    pattern = "glints",
    full.names = TRUE
  )
  query <- paste(readLines(querypath), collapse = "")
  message(sprintf("Pulling job data from Glints %s...", country))
  if (limit > 100) {
    offset <- (seq(1, ceiling(limit / 100)) - 1) * 100
    limit_x <- 100
    var <- sapply(
      offset,
      function(x) {
        generate_variable(
          key = key,
          country = country,
          limit = limit_x,
          offset = x
        )
      }
    )
  } else {
    var <- generate_variable(key, country, limit, 0)
  }
  vacancies <- map_df(
    var, function(x) {
      jobs <- gql(query = query, var = x, opnam = opnam, url = url)
      jobs <- jobs$search$jobs
      vacancy <- map(jobs, function(x) construct(x))
      vacancy <- do.call(bind_rows, vacancy)
      vacancy <- arrange_col(vacancy)
      return(vacancy)
    }
  )
  vacancies <- vacancies[1:limit, ]
  return(vacancies)
}
