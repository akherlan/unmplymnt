#' Restructuring List to Dataframe
#'
#' @param jobls A list of job result from \code{gql()} function
#'
#' @return A data.frame of job in tibble format
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names convert_to_datetime
#'
restruct_job <- function(jobls){

  # restructure
  for (i in seq_along(jobls)) {
    v <- unlist(jobls[[i]])
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
  if (nchar(vacancy$id[1]) == 7) { # jobstreet

    vacancy <- vacancy %>%
      mutate(job_url = paste0("https://www.jobstreet.co.id/id/job/", id),
             source = "Jobstreet") %>%
      select(
        "id", matches("job"), "posted_at", "source", matches("categories"),
        matches("company"), matches("employ"), matches("is_"),
        matches("location"), matches("country"),
        matches("salary"), matches("selling")
      ) %>%
      filter(source_country_code == "id") %>%
      mutate(source = "Jobstreet")
    vacancy$posted_at <- convert_to_datetime(vacancy$posted_at)
    attributes(vacancy$posted_at)$tzone <- Sys.timezone()

  }

  else if (nchar(vacancy$id[1]) == 36) { # glints

    vacancy <- vacancy %>%
      mutate(job_url = paste0("https://glints.com/id/opportunities/jobs/", id),
             source = "Glints") %>%
      select(
        "id", "title", "job_url", "created_at", "source", matches("category"),
        matches("city"), "country_name", "applicant_count", matches("company"),
        matches("experience"), matches("salaries"), matches("salary_estimate"),
        "status", "is_remote"
      )
    vacancy <- rename(vacancy, "job_title" = "title", "posted_at" = "created_at")
    vacancy$posted_at <- convert_to_datetime(vacancy$posted_at)
    attributes(vacancy$posted_at)$tzone <- Sys.timezone()

  } else {

    message("Something wrong")

  }

  return(vacancy)

}
