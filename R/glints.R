#' Glints Vacancy
#'
#' @description Get job vacancy from Glints' website
#' @param key (character) Keyword for the jobs
#' @param limit (numeric) Limit amount of job results
#'
#' @return Job vacancy data.frame in tibble format
#'
#' @examples
#' \dontrun{
#' glints("data analyst", 15) # return data analyst job
#' }
#'
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom janitor clean_names convert_to_datetime
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
			"CountryCode": "%s",
			"includeExternalJobs": false,
			"limit": %s,
			"offset": 90,
			"prioritiseHotJobs": true,
			"SearchTerm": "%s",
			"sources": [ "NATIVE" ]
		}
  }', country, limit, key)

  query <- 'query searchJobs($data: JobSearchConditionInput!) {
    searchJobs(data: $data) {
      jobsInPage {
        id
        title
        isRemote
        status
        createdAt
        isActivelyHiring
        isHot
        company {
          name
        }
        citySubDivision {
          name
        }
        city {
          name
        }
        country {
          name
          code
        }
        category {
          id
          name
        }
        salaries {
          salaryType
          salaryMode
          maxAmount
          minAmount
          CurrencyCode
        }
        minYearsOfExperience
        maxYearsOfExperience
        source
      }
      totalJobs
    }
  }'

  message(sprintf("Pulling job data from Glints %s...", country))
  jobs <- gql(query = query, var = var, opnam = opnam, url = url)
  jobs <- jobs$searchJobs$jobsInPage

  message("Building a data.frame...")
  # reforming salaries to include only BASIC type and eliminate BONUS type
  salaries <- map_df(jobs, ~{
    salaries <- map_df(.x$salaries, ~{.x})
    if (nrow(salaries) > 1) {
      salaries <- dplyr::filter(salaries, salaryType == "BASIC")
    } else if (nrow(salaries) < 1) {
      salaries <- tibble(salaryType = NA_character_,
                         salaryMode = NA_character_,
                         maxAmount = NA_integer_,
                         minAmount = NA_integer_,
                         CurrencyCode = NA_character_)
    } else { salaries }
  })
  # reforming complete vacancy data frame
  vacancy <- map_df(jobs, ~{
    .x[["salaries"]] <- NULL
    as_tibble(t(unlist(.x)))
  })
  vacancy <- bind_cols(vacancy, salaries)
  vacancy <- clean_names(vacancy)
  vacancy <- vacancy %>%
    mutate(job_url = paste0("https://glints.com/id/opportunities/jobs/", id),
           source = paste("Glints", str_to_title(str_replace(source, "_", " "))),
           title = str_squish(title)) %>%
    select(
      "id", "title", "job_url", "created_at", "source", matches("category"),
      matches("city"), "country_name", matches("company"),
      matches("experience"), matches("salary"), matches("amount"),
      matches("currency"), "status", "is_remote"
    ) %>%
    rename("job_title" = "title", "posted_at" = "created_at")
  vacancy$posted_at <- convert_to_datetime(vacancy$posted_at)
  attributes(vacancy$posted_at)$tzone <- "Asia/Jakarta" # Sys.timezone()
  message("Done")
  return(vacancy)

}
