#' Glints Vacancy
#'
#' @description Get job vacancy from Glints' website
#' @param jobid Defined job's category ID. See glints_jobid() function.
#' @param limit Limit amount of job result
#'
#' @return Job vacancy data.frame in tibble format
#' @export
#'
#' @examples
#' \dontrun{
#' glints(2, 15) # return data science job
#' }
glints <- function(jobid, limit = 30) {

  if (missing(jobid)) {
    message('Argument "jobid" is missing, using default: 2')
    jobid <- 2
  }

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
  }', jobid, limit)

  query <- 'query searchJobs($data: JobSearchConditionInput!) {
      searchJobs(data: $data) {
        jobsInPage {
          id
          title
          isRemote
          status
          createdAt
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

  jobs <- gql(query = query, .variables = var,
              .operationName = opnam, .url = url)
  jobs <- jobs$searchJobs$jobsInPage
  vacancy <- restruct_job(jobs)
  return(vacancy)

}
