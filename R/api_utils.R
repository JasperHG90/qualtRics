#### Utilities to work with API

# Helper that fetches a bearer token for user if they use oauth method
qualtrics_set_bearer_token <- function() {

  # body for requests
  body <- list(
    grant_type = "client_credentials"
  )

  # Get bearer token
  res <- httr::POST(paste0("https://",
                           Sys.getenv("QUALTRICS_DATA_CENTER"),
                           ".qualtrics.com/oauth2/token"),
                    encode = "form",
                    body = body,
                    httr::authenticate(Sys.getenv("QUALTRICS_CLIENT_ID"),
                                       Sys.getenv("QUALTRICS_CLIENT_SECRET"),
                                       type = "basic"))

  ### CHECK FOR WARNINGS AND OR ERRORS
  httr::warn_for_status(res)

  # Bearer token
  bt <- httr::content(res)$access_token

  # Set as env variable
  Sys.setenv("QUALTRICS_OAUTH_TOKEN" = bt)

}

# Create headers for an API request
qualtrics_create_header <- function() {

  if(Sys.getenv("QUALTRICS_AUTH_TYPE") == "token") {

    c(
      'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_TOKEN"),
      'Content-Type' = "application/json",
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )

  } else {

    c(
      'authorization' = paste("bearer",
                              Sys.getenv("QUALTRICS_OAUTH_TOKEN")),
      'Content-Type' = "application/json",
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )

  }

}

# Helper function that communicates a request to qualtrics API
qualtrics_handle_request <- function(verb = c("GET", "POST"),
                                     endpoint,
                                     body) {
  # Match arg
  verb <- match.arg(verb)

  # Construct header
  header <- qualtrics_create_header()

  # Send request to qualtrics API
  res <- httr::VERB(verb,
                    url = paste0("https://",
                                 Sys.getenv("QUALTRICS_DATA_CENTER"),
                                 ".qualtrics.com/API/v3/",
                                 endpoint),
                    httr::add_headers(
                      header
                    ),
                    body = body)

  # Look for situation where the bearer token is no longer valid
  # this happens after 60 minutes
  # simply register new token and re-send request

  if(httr::http_error(res)) {

    if(httr::status_code(res) == 401) {

      if(httr::has_content(res)) {

        tmp <- httr::content(res)

        if(!is.null(tmp$meta$error$errorCode)) {

          if(tmp$meta$error$errorCode == "AUTH_6.0") {

            # Get new bearer token
            qualtrics_set_bearer_token()

            # Construct header
            header <- qualtrics_create_header()

            # Send request to qualtrics API
            res <- httr::VERB(verb,
                              url = paste0("https://",
                                           Sys.getenv("QUALTRICS_DATA_CENTER"),
                                           ".qualtrics.com/API/v3/",
                                           endpoint),
                              httr::add_headers(
                                header
                              ),
                              body = body)


          }

        }

      }

    }

  }

  # Check if response type is OK
  cnt <- qualtRicsResponseCodes(res)
  # Check if OK
  if(cnt$OK) {
    # If notice occurs, raise warning
    w <- checkForWarnings(cnt)
    # return content
    return(cnt$content)
  }
}
