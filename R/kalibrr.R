#' Kalibrr Vacancy
#'
#' @description Get job vacancy from Kalibbr ID website.
#' @param key A character indicate the keyword for job search.
#' @param limit Limit amount of job results to return.
#' @param remote Remote work allowance.
#'
#' @examples
#' \dontrun{
#' kalibrr("data engineer", 20, TRUE) # return remote data engineer job
#' }
#'
#' @import rvest
#' @import lubridate
#' @importFrom stringr str_detect str_extract str_squish
#' @importFrom dplyr as_tibble distinct
#'
#' @export
#'

kalibrr <- function(key, limit = 30L, remote = FALSE) {

  if (missing(key)) {
    key <- "data analyst"
    message(sprintf('Argument "key" is missing, using default: "%s"', key))
  }
  if (missing(remote)) { remote <- FALSE }

  country <- "Indonesia"

  generate_url <- function(key, limit = limit, remote = remote) {

    # example url:
    # https://www.kalibrr.com/job-board/te/data-engineer/co/Indonesia/1?sort=Freshness
    # https://www.kalibrr.com/job-board/te/data-engineer/co/Indonesia/1
    # https://www.kalibrr.com/job-board/te/data-engineer/co/Indonesia/work_from_home/y/1
    # https://www.kalibrr.com/job-board/te/data-engineer/co/Indonesia/work_from_home/y/1?sort=Freshness

    key <- gsub("\\s", "-", key)
    n_page <- ceiling(limit/15)
    if (remote) {
      co = sprintf("%s/work_from_home/y/%s?sort=Relevance", country, 1:n_page)
    } else {
      co = sprintf("%s/%s?sort=Relevance", country, 1:n_page)
    }
    url <- sprintf("https://www.kalibrr.com/job-board/te/%s/co/%s", key, co)
    return(url)
  }

  get_job_url <- function(card) {
    paste0(
      "https://www.kalibrr.com",
      card %>%
        html_element("h2 > a") %>%
        html_attr("href")
    )
  }

  get_id <- function(job_url) {
    strsplit(job_url, "/", fixed = TRUE)[[1]][7]
  }

  get_title <- function(card) {
    card %>%
      html_element("h2 > a") %>%
      html_text(trim = TRUE)
  }

  get_company <- function(card) {
    card %>%
      html_element("div > span > a") %>%
      html_text(trim = TRUE)
  }

  get_location <- function(card) {
    card %>%
      html_element(".k-text-subdued.k-block") %>%
      html_text(trim = TRUE)
  }

  get_remote <- function(card) {
    is_remote <- card %>%
      html_element(".k-flex.k-flex-col > .k-mt-1 > a") %>%
      html_text(trim = TRUE) == "Remote Work"
    if (is.na(is_remote)) is_remote <- FALSE
    return(is_remote)
  }

  get_date <- function(card) {
    card %>%
      html_element('.k-block.k-mb-1') %>%
      html_text()
  }

  parse_city <- function(location) {
    str_squish(strsplit(location, ",")[[1]][1])
  }

  parse_country <- function(location) {
    country <- strsplit(location, ",")[[1]]
    country <- str_squish(country[length(country)])
  }

  parse_date <- function(text) {
    # job posted date
    text <- strsplit(text, "\u2022")[[1]] # u+2022 is bullet point
    post_date <- text[1]
    num <- as.numeric(str_extract(post_date, "\\d+"))
    if (is.na(num)) num <- 1 # e.g. a month ago, a day ago
    if (str_detect(post_date, "year")) {
      posted_date <- as_date(Sys.time() - dyears(num))
    } else if (str_detect(post_date, "month")) {
      posted_date <- as_date(Sys.time() - dmonths(num))
    } else if (str_detect(post_date, "week")) {
      posted_date <- as_date(Sys.time() - dweeks(num))
    } else if (str_detect(post_date, "day")) {
      posted_date <- as_date(Sys.time() - ddays(num))
    } else if (str_detect(post_date, "hour")) {
      posted_date <- as_date(Sys.time() - dhours(num))
    } else if (str_detect(post_date, "minute")) {
      posted_date <- as_date(Sys.time() - dminutes(num))
    }
    # application deadline
    due_date <- text[2]
    due_date <- str_extract(due_date, "\\d{1,2}.+$")
    if ( # guessing year of the deadline
      dmy(paste(due_date, year(Sys.Date()))) - posted_date < 0
    ) {
      due_date <- dmy(paste(due_date, year(Sys.Date()) + 1))
    } else {
      due_date <- dmy(paste(due_date, year(Sys.Date())))
    }

    job_date <- c(posted_date, due_date)
    names(job_date) <- c("published", "due_date")

    return(job_date)
  }

  scrape_kalibrr <- function(url) {
    response <- read_html(url)
    cards <- html_elements(response, ".k-border-tertiary-ghost-color")
    i <- 0; data <- list()
    for (card in cards) {
      i <- i + 1
      job_date <- get_date(card) %>% parse_date()
      data[[i]] <- data.frame(
        job_title = get_title(card),
        company = get_company(card),
        city = parse_city(get_location(card)),
        country = parse_country(get_location(card)),
        is_remote = get_remote(card),
        due_date = job_date[["due_date"]],
        posted_at = job_date[["published"]],
        source = "Kalibrr",
        job_url = get_job_url(card),
        job_id = get_id(get_job_url(card))
      )
    }
    data <- do.call(rbind, data) %>% distinct()
    return(data)
  }

  urls <- generate_url(key = key, limit = limit, remote = remote)
  message(sprintf("Pulling job data from Kalibrr %s...", country))
  data <- lapply(urls, scrape_kalibrr)
  message("Building a data.frame...")
  data <- as_tibble(do.call(rbind, data))[1:limit,]
  message("Done")
  return(data)
}
