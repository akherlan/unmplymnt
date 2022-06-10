#' Indeed Indonesia Vacancy
#'
#' @description Get job vacancy from Indeed's website (Indonesia)
#' @param key (character) Keyword for the jobs
#' @param limit (numeric) Limit amount of job results
#'
#' @return Job vacancy data.frame in tibble format
#'
#' @examples
#' \dontrun{
#' indeed("ux designer") # return UX designer job
#' }
#'
#' @import dplyr
#' @import rvest
#' @import stringr
#' @export
#'
indeed <- function(key, limit = 30L) {

  if (missing(key)) {
    key <- "data analyst"
    message(sprintf('Argument "key" is missing, using default: "%s"', key))
  }
  # country and subdomain
  country <- "id"

  # query handle
  query <- str_replace_all(key, "\\s+", "+")

  # page handle
  page <- ceiling(limit/15)
  page <- (c(1:page)-1)*10

  message(sprintf("Pulling job data from Indeed %s...", toupper(country)))
  for (p in page) {

    # get html page
    url <- sprintf("https://%s.indeed.com/jobs?q=%s&sort=date&start=%s",
                   country, query, p)
    url <- url(url, "rb")
    htmlraw <- tryCatch(read_html(url))
    close(url)

    # get vacancy items
    item <- htmlraw |>
      html_elements(".mosaic-provider-jobcards") |>
      html_children()

    # job url for full page direction and the id
    # jobsearch-ResultsList
    job_url <- item[grepl('a id="job', item)] |>
      html_elements(".jobTitle") |>
      html_element("a") |>
      html_attr("href")
    for (item in seq_along(job_url)) {
      # if (str_detect(job_url[item], "\\/rc\\/clk\\?jk")) {
      if (grepl("/rc/clk\\?jk", job_url[item])) {
        job_url[item] <- str_extract(job_url[item], "\\?jk=[a-z0-9]{16}")
      } else {
        job_url[item] <- str_replace(job_url[item], "^(.+)\\?fccid.+$", "\\1")
      }
    }
    job_id <- str_extract(job_url, "[a-z0-9]{16}")
    for (item in seq_along(job_url)) {
      if (grepl("company/.+/jobs", job_url[item])) {
        job_url[item] <- paste0("https://", country, ".indeed.com", job_url[item])
      } else {
        job_url[item] <- paste0("https://", country, ".indeed.com/lihat-lowongan-kerja", job_url[item])
      }
    }

    # job title
    job_position <- htmlraw |>
      html_elements(".jobTitle") |>
      html_text() |>
      str_remove("^Baru")

    # company
    company_name <- htmlraw |>
      html_elements(".companyName") |>
      html_text()

    company_rating <- htmlraw |>
      html_elements(".heading6.company_location") |>
      html_element(".ratingsDisplay") |>
      html_text() |>
      str_replace(",", ".") |>
      as.numeric()

    # location
    location <- htmlraw |>
      html_elements(".companyLocation") |>
      html_text()
    company_location <- str_replace(location, "^(.+)\\s?•\\s?.+", "\\1")
    working_location <- str_extract(location, "^.+\\s?•\\s?(.+)") |> str_remove("^.+\\s?•\\s?")

    # salary
    salary <- htmlraw |>
      html_elements(".resultContent") |>
      html_element(".salary-snippet") |>
      html_text()

    salary_from <- salary |>
      str_replace("^Rp\\.?\\s?(\\d+(\\.?\\d{1,3}){1,3})\\s?-.+", "\\1") |>
      str_remove_all("\\.") |>
      as.numeric()

    salary_until <- salary |>
      str_replace("^Rp.+-\\s?Rp\\.?\\s?(\\d+(\\.?\\d{1,3}){1,3})\\s?.+", "\\1") |>
      str_remove_all("\\.") |>
      as.numeric()

    # generate dataframe
    joblist <- data.frame(
      id = job_id,
      job_title = job_position,
      job_url = job_url,
      company_name = company_name,
      company_location = company_location,
      company_rating = company_rating,
      working_location = working_location,
      salary_from = salary_from,
      salary_until = salary_until
    ) |> as_tibble()

    if (p == 0) { vacancy <- joblist } else {
      vacancy <- bind_rows(vacancy, joblist)
    }
  }

  message("Building a data.frame...")
  vacancy <- distinct(vacancy)[1:limit,]

  message("Done")
  return(vacancy)

}
