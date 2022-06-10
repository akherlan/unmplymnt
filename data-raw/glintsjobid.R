## Pull `glintsjobid` dataset

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)

glintsjobid <- fromJSON("https://glints.com/api/jobCategories")
glintsjobid <- arrange(as_tibble(glintsjobid$data[c("id", "name")]), id)
usethis::use_data(glintsjobid, overwrite = TRUE)
