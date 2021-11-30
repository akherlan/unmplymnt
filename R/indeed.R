#' Indeed Indonesia Vacancy
#'
#' @description Get job vacancy from Indeed's website (Indonesia)
#' @param key Keywords in character
#' @param page Numeric indicate number of pages
#'
#' @return Job opportunity data.frame in tibble format
#' @export
#' @import stringr
#' @import rvest
#' @importFrom dplyr bind_rows as_tibble
#'
#' @examples
#' \dontrun{
#' indeed("data analyst") # return data analyst job
#' }
indeed <- function(key, page = 2) {

  if (missing(key)) {
    message('Argument "key" is missing, using default: "data analyst"')
    key <- "data analyst"
  }

  # query handle
  query <- str_replace_all(key, "\\s+", "+")

  # page handle
  page <- (c(1:page)-1)*10

  for (p in page) {

    # get html page
    url <- sprintf("https://id.indeed.com/jobs?q=%s&sort=date&start=%s", query, p)
    url <- url(url, "rb")
    htmlraw <- read_html(url)
    close(url)

    # get vacancy items
    item <- htmlraw |>
      html_elements(".mosaic-provider-jobcards") |>
      html_children()

    # job url for full page direction and the id
    job_url <- suppressWarnings(item[str_detect(item, "a\\sid")] |> html_attr("href"))
    for (item in seq_along(job_url)) {
      if (str_detect(job_url[item], "\\/rc\\/clk\\?jk")) {
        job_url[item] <- str_extract(job_url[item], "\\?jk=[a-z0-9]{16}")
      } else {
        job_url[item] <- str_replace(job_url[item], "^(.+)\\?fccid.+$", "\\1")
      }
    }
    job_id <- str_extract(job_url, "[a-z0-9]{16}")
    for (item in seq_along(job_url)) {
      if (str_detect(job_url[item], "company\\/.+\\/jobs")) {
        job_url[item] <- paste0("https://id.indeed.com", job_url[item])
      } else {
        job_url[item] <- paste0("https://id.indeed.com/lihat-lowongan-kerja", job_url[item])
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

    # generate data.frame
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

    if (p == 0) vacancy <- joblist else vacancy <- bind_rows(vacancy, joblist)
  }

  return(vacancy)

}
