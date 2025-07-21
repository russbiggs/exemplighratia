status_url <- function() {
  "https://kctbh9vrtdwd.statuspage.io/api/v2/components.json"
}

#' GitHub APIs status
#'
#' @description Get the status of requests to GitHub APIs
#'
#' @return A character vector, one of "operational", "degraded_performance",
#' "partial_outage", or "major_outage."
#'
#' @details See details in https://www.githubstatus.com/api#components.
#' @export
#'
#' @examples
#' \dontrun{
#' gh_api_status()
#' }
gh_api_status <- function() {
  req <- httr2::request(status_url())

  response <- httr2::req_perform(req)

  content <- httr2::resp_body_json(response)

  components <- content$components

  api_status <- components[purrr::map_chr(components, "name") == "API Requests"][[1]]

  api_status$status
}