#' Glints Vacancy
#'
#' @description Get job vacancy from Glints' website.
#' @param key A character indicate the keyword for job search.
#' @param limit Limit amount of job results to return. Maximum 100.
#'
#' @examples
#' \dontrun{
#' glints("data analyst", 15) # return data analyst job
#' }
#'
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom janitor clean_names
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_title str_squish str_replace
#'
#' @export
#'


glints <- function(key, limit = 30L) {

  if (limit > 100L) stop('Argument "limit" should not be greater than 100')
  if (missing(key)) {
    key <- "data analyst"
    message(sprintf('Argument "key" is missing, using default: "%s"', key))
  }

  url <- "https://glints.com/api/graphql"
  opnam <- "searchJobs"
  country <- "ID"

  var <- sprintf('{
    "data": {
      "SearchTerm": "%s",
      "CountryCode": "%s",
      "limit": %i,
      "offset": 0,
      "includeExternalJobs": false,
      "sources": "NATIVE"
    }
  }', key, country, limit)

  query <- "query searchJobs($data: JobSearchConditionInput!) {
    search: searchJobs(data: $data) {
      jobs: jobsInPage {
        job_id: id
        job_title: title
        is_remote: isRemote
        posted_at: createdAt
        company {
          id
          name
        }
        city {
          name
        }
        country {
          name
        }
        category {
          id
          name
        }
        salary: salaries {
          salary_period: salaryMode
          salary_currency: CurrencyCode
          salary_max: maxAmount
          salary_min: minAmount
          salary_type: salaryType
        }
        employment_expmin: minYearsOfExperience
        employment_expmax: maxYearsOfExperience
        source
      }
    }
  }"

  message(sprintf("Pulling job data from Glints %s...", country))
  jobs <- gql(query = query, var = var, opnam = opnam, url = url)
  jobs <- jobs$search$jobs

  message("Building a data.frame...")
  # reforming salaries to include only BASIC type and eliminate BONUS type
  salaries <- map(jobs, ~{
    salary <- unlist(.x$salary)
    if (!is.null(salary)) {
      salary <- as_tibble(t(salary))
    } else {
      salary <- tibble(
        salary_type = NA_character_,
        salary_period = NA_character_,
        salary_max = NA_character_, # NA_integer_
        salary_min = NA_character_, # NA_integer_
        salary_currency = NA_character_
      )
    }
    salary
  })
  salaries <- do.call(bind_rows, salaries)
  # reforming complete vacancy data frame
  vacancies <- map_df(jobs, ~{
    .x[["salary"]] <- NULL
    vacancy <- as_tibble(t(unlist(.x)), .name_repair = 'unique')
    vacancy <- clean_names(vacancy)
  })
  vacancies <- bind_cols(vacancies, salaries)
  vacancies <- vacancies %>%
    mutate(
      job_url = paste0("https://glints.com/id/opportunities/jobs/", .$job_id),
      source = paste("Glints", str_to_title(str_replace(source, "_", " "))),
      job_title = str_squish(job_title),
      is_remote = as.logical(is_remote),
      posted_at = ymd(str_replace(posted_at, "^(\\d{4}-\\d{2}-\\d{2}).+$", "\\1"))
    ) %>%
    select(
      "job_title",
      company = company_name,
      city = city_name,
      country = country_name,
      "is_remote",
      category = category_name,
      "salary_currency",
      "salary_min",
      "salary_max",
      "salary_period",
      "salary_type",
      matches("employ"),
      "posted_at",
      "source",
      "job_url",
      "job_id"
    )
  #vacancies$posted_at <- ymd(
  #  str_replace(vacancies$posted_at, "^(\\d{4}-\\d{2}-\\d{2}).+$", "\\1")
  #)
  #attributes(vacancies$posted_at)$tzone <- "Asia/Jakarta"
  message("Done")
  return(vacancies)

}
