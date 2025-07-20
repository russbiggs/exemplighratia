gh_v3_url <- function() {
  "https://api.github.com/"
}

#' GitHub organizations
#'
#' @description Get logins of GitHub organizations.
#'
#' @param since The integer ID of the last organization that you've seen.
#'
#' @return A character vector of at most 30 elements.
#' @export
#'
#' @details Refer to https://developer.github.com/v3/orgs/#list-organizations
#'
#' @examples
#' \dontrun{
#' gh_organizations(since = 42)
#' }
gh_organizations <- function(since = 1) {

  token <- Sys.getenv("GITHUB_PAT")

  if (!nchar(token)) {
    stop("No token provided! Set up the GITHUB_PAT environment variable please.", call. = FALSE)
  }

  req <- httr2::request(gh_v3_url()) |>
    httr2::req_url_path("organizations") |>
    httr2::req_url_query(since = since) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_retry(is_transient = \(resp) resp_status(resp) %in% c(500, 502, 503))


  response <- httr2::req_perform(req)

  httr2::resp_check_status(response)

  content <- httr2::resp_body_json(response)

  purrr::map_chr(content, "login")
}
