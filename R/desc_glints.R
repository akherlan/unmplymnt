#' Description formatting for Glints
#'
#' @description Scrape Glints' job description (experimental).
#'
#' @param url Character of job's URLs.
#' @param head Include title. Default ot FALSE.
#' @param title Character title, same length with url.
#' @param mode The way to return the description text c("print", "telegram").
#'
#' @examples
#' \dontrun{
#' desc_glints("https://glints.com/id/opportunities/jobs/0850c687-5e79-4402-ad84-d5c04cb422d6")
#' }
#'
#' @import dplyr
#' @import rvest
#' @import stringr
#' @export
#'
desc_glints <- function(url, head, title, mode = "print") {

  # argument validation
  if (missing(head)) head <- FALSE
  if (missing(title)) {
    if (!head) title <- NULL
    if (head) stop('Argument "title" is missing')
  }
  if (head & length(title) != length(url)) stop('Argument "title" is not in same length with "url"')
  if (!(mode %in% c("print", "telegram"))) stop('Available mode = c("print", "telegram")')

  # process
  for (n in seq_along(url)) {

    # get html page
    f <- read_html(url[n])
    desc <- html_element(f, ".DraftEditor-editorContainer")

    # assume prefered description tag
    desc <- desc %>%
      html_children() %>%
      html_children() %>%
      html_children() %>%
      html_children()

    # description tabulation
    tag <- desc %>% html_name()
    content <- desc %>% html_text() %>% str_squish()

    # pre-formatting
    desc_preform <- tibble(tag, content)

    # post-formatting
    desc_posform <- desc_preform %>%
      # define type of content
      mutate(type = case_when(
        ( # subtitle
          (tag == "div") &
            (str_count(content, "\\w+\\s") <= 4) &
            !str_detect(content, "^-\\s") &
            !str_detect(content, "\u2022\\s") # %95 - url encoding
        ) ~ "title",
        # paragraph
        (tag == "div") & (str_count(content, "\\w+\\s") > 4) ~ "alinea",
        # item
        (tag == "li") ~ "item",
        (tag == "div") & str_detect(content, "^-\\s") ~ "item",
        #(tag == "div") & str_detect(content, "•\\s") ~ "item"
        (tag == "div") & str_detect(content, "\u2022\\s") ~ "item" # bullet
      )) %>%
      # formatting
      mutate(content = case_when(
        (type == "title") ~ str_replace(content, "^(.+)$", "*\\1*"),
        (type == "alinea") ~ content,
        (type == "item") ~ str_replace(content, "^(.+)$", "\u2022 \\1"), # %95
      ))

    desc_posform <- desc_posform %>%
      mutate(sum = if_else(type == "title", 1L, 0L),
             temp = 0L,
             section = cumsum(temp + sum)) %>%
      select(-temp, -sum)

    txt <- split(desc_posform, desc_posform$section)
    txt <- lapply(txt, function(x) { append(x[["content"]], " ") })
    txt <- do.call(c, txt)
    txt <- append(txt, url[n])
    if (head) txt <- append(c(paste0("*", toupper(title[n]), "*"), " "), txt)

    file_name <- paste0(tempdir(), "glints_", as.integer(Sys.time()), ".txt")
    cat(txt, sep = "\n", file = file_name)

    if (mode == "print") {
      cat(readLines(file_name), sep = "\n")
      cat("\n")
    }

    if (mode == "telegram") {
      cat('mode = "telegram" in development')
    }

  }

  # return all result here

}
