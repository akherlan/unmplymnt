#' Restructuring List to Dataframe
#'
#' @param jobls List of job result from GQL
#'
#' @return A tibble of job
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
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
  if (nchar(vacancy$id[1]) == 7) {

    # jobstreet
    vacancy <- vacancy %>%
      mutate(
        job_url = paste0("https://www.jobstreet.co.id/id/job/", id)
      ) %>%
      select(
        id, matches("job"), posted_at, matches("categories"),
        matches("company"), matches("employ"), matches("is_"),
        matches("location"), matches("country"),
        matches("salary"), matches("selling")
      ) %>%
      filter(source_country_code == "id") %>%
      mutate(source = "Jobstreet")

  }

  else {

    # glints
    vacancy <- vacancy %>%
      select(
        id, title, matches("category"), matches("city"),
        country_name, applicant_count, matches("company"),
        matches("experience"), matches("salaries"), matches("salary_estimate"),
        status, is_remote, is_hot, created_at
      ) %>%
      mutate(source = "Glints")

    vacancy <- rename(vacancy, "job_title" = "title")

  }

  return(vacancy)

}
