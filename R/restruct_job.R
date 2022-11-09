#' Restructuring List to Dataframe
#'
#' @param jobs A list of job result from \code{gql()} function
#'
#' @return A data.frame of job in tibble format
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
#' @importFrom lubridate ymd
#'
restruct_job <- function(jobs){

  # restructure
  for (i in seq_along(jobs)) {
    v <- unlist(jobs[[i]])
    v <- cbind(name = names(v), value = v)
    v <- as_tibble(v) %>% mutate(num = i)
    if(i == 1){
      vacancy <- v
    } else {
      vacancy <- bind_rows(vacancy, v)
    }
  }

  # reform
  vacancy <- vacancy %>%
    group_by(num, name) %>%
    summarise_all(~toString(value)) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = "num",
      names_from = "name",
      values_from = "value"
    ) %>%
    select(-num) %>%
    clean_names()

  # arrange
  if (nchar(vacancy$job_id[1]) == 7) { # jobstreet

    vacancy <- vacancy %>%
      mutate(job_url = paste0("https://www.jobstreet.co.id/id/job/", job_id),
             source = "Jobstreet",
             country = "Indonesia",
             is_remote = NA,
             posted_at = ymd(str_replace(posted_at, "^(\\d{4}-\\d{2}-\\d{2}).+$", "\\1"))) %>%
      select(
        "job_title",
        "company" = "company_name",
        "city" = "city_name",
        "country",
        "is_remote",
        "category" = "category_name",
        "salary_currency",
        "salary_min",
        "salary_max",
        "salary_period",
        matches("employ"),
        "posted_at",
        "source",
        "job_url",
        "job_id"
      )

  } else {

    message("Something wrong")

  }

  return(vacancy)

}
