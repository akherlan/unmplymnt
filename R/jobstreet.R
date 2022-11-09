#' Jobstreet Vacancy
#'
#' @description Get job vacancy from Jobstreet's website (Seek ID).
#' @param key (character) Keyword for the jobs
#' @param limit (numeric) Limit amount of job results
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

  if (missing(key)) {
    key <- "data analyst"
    message(sprintf('Argument "key" is missing, using default: "%s"', key))
  }

  page <- seq(1L, ceiling(limit/30L), 1L)
  country <- "id"
  url <- sprintf(
    "https://xapi.supercharge-srp.co/job-search/graphql?country=%s&isSmartSearch=true",
    country
  )

  var <- sapply(page, function(p) {
    toJSON(list(keyword = key,
                jobFunctions = list(),
                locations = list(),
                salaryType = 1,
                jobTypes = list(),
                careerLevels = list(),
                page = p,
                country = country,
                categories = list(),
                workTypes = list(),
                industries = list(),
                locale = "id"),
           auto_unbox = TRUE)
  })

  query <- 'query getJobs($country: String, $locale: String,
  $keyword: String, $createdAt: String, $jobFunctions: [Int],
  $categories: [String], $locations: [Int], $careerLevels: [Int],
  $minSalary: Int, $maxSalary: Int, $salaryType: Int,
  $candidateSalary: Int, $candidateSalaryCurrency: String,
  $datePosted: Int, $jobTypes: [Int], $workTypes: [String],
  $industries: [Int], $page: Int, $pageSize: Int, $companyId: String,
  $advertiserId: String, $userAgent: String, $accNums: Int,
  $subAccount: Int, $minEdu: Int, $maxEdu: Int, $edus: [Int],
  $minExp: Int, $maxExp: Int, $seo: String, $searchFields: String,
  $candidateId: ID, $isDesktop: Boolean, $isCompanySearch: Boolean,
  $sort: String, $sVi: String, $duplicates: String, $flight: String,
  $solVisitorId: String) {
    jobs(
      country: $country
      locale: $locale
      keyword: $keyword
      createdAt: $createdAt
      jobFunctions: $jobFunctions
      categories: $categories
      locations: $locations
      careerLevels: $careerLevels
      minSalary: $minSalary
      maxSalary: $maxSalary
      salaryType: $salaryType
      candidateSalary: $candidateSalary
      candidateSalaryCurrency: $candidateSalaryCurrency
      datePosted: $datePosted
      jobTypes: $jobTypes
      workTypes: $workTypes
      industries: $industries
      page: $page
      pageSize: $pageSize
      companyId: $companyId
      advertiserId: $advertiserId
      userAgent: $userAgent
      accNums: $accNums
      subAccount: $subAccount
      minEdu: $minEdu
      edus: $edus
      maxEdu: $maxEdu
      minExp: $minExp
      maxExp: $maxExp
      seo: $seo
      searchFields: $searchFields
      candidateId: $candidateId
      isDesktop: $isDesktop
      isCompanySearch: $isCompanySearch
      sort: $sort
      sVi: $sVi
      duplicates: $duplicates
      flight: $flight
      solVisitorId: $solVisitorId
    ) {
      relatedSearchKeywords {
        keywords
      }
      jobs {
        job_id: id
        company: companyMeta {
          id
          name
        }
        job_title: jobTitle
        employment: employmentTypes {
          type: name
        }
        city: locations {
          name
        }
        category: categories {
          id: code
          name
        }
        salary: salaryRange {
          max
          min
          currency
          period
        }
        posted_at: postedAt
      }
    }
  }'

  message(sprintf("Pulling job data from Jobstreet (Seek %s)...",
                  toupper(country)))

  jobs <- map(var, ~gql(query = query, var = .x, url = url))
  jobs <- map(jobs, ~{.x$jobs$jobs})

  message("Building a data.frame...")

  vacancy <- map_df(jobs, ~restruct_job(.x))
  vacancy <- distinct(vacancy)[1:limit,]

  message("Done")
  return(vacancy)
}
