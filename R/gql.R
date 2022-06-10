#' GraphQL Query Request
#'
#' @description Request data using GraphQL query
#' @param query GraphQL query
#' @param ... Other arguments used by \code{httr::POST()}
#' @param token Token
#' @param var Variables
#' @param opnam Operation name
#' @param url Complete URL to RestAPI service base to endpoint
#'
#' @return A list of query result
#'
#' @import httr
#' @import jsonlite
#'
#' @source \href{https://gist.github.com/rentrop/83cb1d8fc8593726a808032e55314019/}{Rentrop's Gist}
#'
gql <- function(query,
                ...,
                token = NULL,
                var = NULL,
                opnam = NULL,
                url = url){
  pbody <- list(query = query, variables = var, operationName = opnam)
  if (is.null(token)) {
    res <- POST(url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", token)
    res <- POST(
      url,
      body = pbody,
      encode = "json",
      add_headers(Authorization = auth_header),
      ...
    )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  return(res$data)
}
