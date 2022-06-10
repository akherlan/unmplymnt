#' Jobstreet Vacancy
#'
#' @description Get job vacancy from Jobstreet's website
#' @param key (character) Keyword for the jobs
#' @param limit (numeric) Limit amount of job results
#'
#' @return Job vacancy data.frame in tibble format
#'
#' @examples
#' \dontrun{
#' jobstreet("data analyst") # return search result for data analyst
#' jobstreet("data engineer", 10) # return 10 data engineer jobs
#' }
#'
#' @import dplyr
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
  var <- sprintf('{
    "keyword": "%s",
    "jobFunctions": [
      501,
      503,
      508,
      512
    ],
    "locations": [],
    "salaryType": 1,
    "jobTypes": [],
    "createdAt": null,
    "careerLevels": [],
    "page": %s,
    "country": "%s",
    "sVi": "",
    "categories": [
      "501",
      "503",
      "508",
      "512"
    ],
    "workTypes": [],
    "industries": [],
    "locale": "id"
  }', key, page, country)

  query <- 'query getJobs($country: String, $locale: String, $keyword: String, $createdAt: String, $jobFunctions: [Int], $categories: [String], $locations: [Int], $careerLevels: [Int], $minSalary: Int, $maxSalary: Int, $salaryType: Int, $candidateSalary: Int, $candidateSalaryCurrency: String, $datePosted: Int, $jobTypes: [Int], $workTypes: [String], $industries: [Int], $page: Int, $pageSize: Int, $companyId: String, $advertiserId: String, $userAgent: String, $accNums: Int, $subAccount: Int, $minEdu: Int, $maxEdu: Int, $edus: [Int], $minExp: Int, $maxExp: Int, $seo: String, $searchFields: String, $candidateId: ID, $isDesktop: Boolean, $isCompanySearch: Boolean, $sort: String, $sVi: String, $duplicates: String, $flight: String, $solVisitorId: String) {
   jobs(country: $country, locale: $locale, keyword: $keyword, createdAt: $createdAt, jobFunctions: $jobFunctions, categories: $categories, locations: $locations, careerLevels: $careerLevels, minSalary: $minSalary, maxSalary: $maxSalary, salaryType: $salaryType, candidateSalary: $candidateSalary, candidateSalaryCurrency: $candidateSalaryCurrency, datePosted: $datePosted, jobTypes: $jobTypes, workTypes: $workTypes, industries: $industries, page: $page, pageSize: $pageSize, companyId: $companyId, advertiserId: $advertiserId, userAgent: $userAgent, accNums: $accNums, subAccount: $subAccount, minEdu: $minEdu, edus: $edus, maxEdu: $maxEdu, minExp: $minExp, maxExp: $maxExp, seo: $seo, searchFields: $searchFields, candidateId: $candidateId, isDesktop: $isDesktop, isCompanySearch: $isCompanySearch, sort: $sort, sVi: $sVi, duplicates: $duplicates, flight: $flight, solVisitorId: $solVisitorId) {
    ...LegacyCompat_SearchResult
   }
  }
  fragment LegacyCompat_SearchResult on SearchResult {
    solMetadata
    jobs {
      id
      sourceCountryCode
      isStandout
      companyMeta {
        id
        isPrivate
        name
      }
      jobTitle
      employmentTypes {
        name
      }
      sellingPoints
      locations {
        code
        name
        children {
          code
          name
        }
      }
      categories {
        code
        name
        children {
          code
          name
        }
      }
      postedAt
      salaryRange {
        currency
        max
        min
        period
        term
      }
      isClassified
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
