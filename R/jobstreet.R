#' Jobstreet Vacancy
#'
#' @description Get job vacancy from Jobstreet's website
#' @param key Keyword for the jobs
#' @param page The amount of pages to be looking at
#'
#' @return Job opportunity data.frame in tibble format
#'
#' @examples
#' \dontrun{
#' jobstreet("data analyst") # return search result for data analyst
#' }
#'
#' @export
#'
jobstreet <- function(key, page = 1) {

  if (missing(key)) {
    key <- "data analyst"
    message(sprintf('Argument "key" is missing, using default: "%s"', key))
  }

  url <- "https://xapi.supercharge-srp.co/job-search/graphql?country=id&isSmartSearch=true"
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
    "country": "id",
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
  }', key, page)

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

  jobs <- gql(query = query, var = var, url = url)
  jobs <- jobs$jobs$jobs
  vacancy <- restruct_job(jobs)
  return(vacancy)
}
