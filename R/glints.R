#' Glints Vacancy
#'
#' @description Get job vacancy from Glints' website
#' @param key (character) Keyword for the jobs
#' @param limit (numeric) Limit amount of job results
#'
#' @return (data.frame) Job vacancy data.frame in tibble format
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

  var <- sprintf('{
    "data": {
			"CountryCode": "ID",
			"includeExternalJobs": true,
			"limit": %s,
			"offset": 90,
			"prioritiseHotJobs": true,
			"SearchTerm": "%s",
			"sources": [ "NATIVE" ]
		}
  }', limit, key)

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
        salaryEstimate {
          minAmount
          maxAmount
          CurrencyCode
        }
        company {
          id
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
          id
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

  jobs <- gql(query = query, var = var, opnam = opnam, url = url)
  jobs <- jobs$searchJobs$jobsInPage
  vacancy <- map_df(jobs, ~as_tibble(t(unlist(.x)), .name_repair = "minimal"))
  vacancy <- clean_names(vacancy)
  # vacancy <- restruct_job(jobs)
  vacancy <- vacancy %>%
    mutate(job_url = paste0("https://glints.com/id/opportunities/jobs/", id),
           source = paste("Glints", str_to_title(str_replace(source, "_", " "))),
           title = str_squish(title)) %>%
    select(
      "id", "title", "job_url", "created_at", "source", matches("category"),
      matches("city"), "country_name", matches("company"),
      matches("experience"), matches("salaries"), matches("salary_estimate"),
      "status", "is_remote"
    ) %>%
    rename("job_title" = "title", "posted_at" = "created_at")
  vacancy$posted_at <- convert_to_datetime(vacancy$posted_at)
  attributes(vacancy$posted_at)$tzone <- "Asia/Jakarta" # Sys.timezone()

  return(vacancy)

}
