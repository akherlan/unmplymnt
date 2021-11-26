#' GraphQL Request
#'
#' @param query GraphQL query
#' @param ... Others
#' @param .token Token
#' @param .variables Variables
#' @param .operationName Operation name
#' @param .url URL to RestAPI service base to endpoint
#'
#' @return List of query result
#' @export
#' @import httr
#' @import jsonlite
gql <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url){
  pbody <- list(query = query, variables = .variables, operationName = .operationName)
  if(is.null(.token)){
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <- POST(.url, body = pbody, encode = "json", add_headers(Authorization=auth_header), ...)
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if(!is.null(res$errors)){
    warning(toJSON(res$errors))
  }
  res$data
}
