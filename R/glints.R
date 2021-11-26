#' Get Vacancy from Glints
#'
#' @param jobid Job's id
#' @param lim Limit number of job result
#'
#' @return Job vacancy data.frame in tibble format
#' @export
#'
#' @examples
#' \dontrun{
#' glints(2, 15)
#' }
glints <- function(jobid = 2, lim = 30) {
  url <- "https://glints.com/api/graphql"
  opnam <- "searchJobs"
  var <- sprintf('{
    "data": {
      "JobCategoryId": %s,
      "CountryCode": "ID",
      "limit": %s,
      "offset": 30,
      "prioritiseHotJobs": true,
      "includeExternalJobs": true,
      "variant": "A"
    }
  }', jobid, lim)

  query <- 'query searchJobs($data: JobSearchConditionInput!) {
      searchJobs(data: $data) {
        jobsInPage {
          id
          title
          isRemote
          status
          createdAt
          isHot
          salaryEstimate {
            minAmount
            maxAmount
            CurrencyCode
          }
          company {
            name
            id
          }
          citySubDivision {
            name
          }
          city {
            name
          }
          country {
            name
          }
          category {
            name
          }
          salaries {
            salaryType
            salaryMode
            maxAmount
            minAmount
            CurrencyCode
          }
          applicantCount
          minYearsOfExperience
          maxYearsOfExperience
        }
        totalJobs
      }
    }'

  jobs <- gql(
    query = query,
    .variables = var,
    .operationName = opnam,
    .url = url
  )

  jobs <- jobs$searchJobs$jobsInPage
  vacancy <- restruct_job(jobs)
  return(vacancy)

}
