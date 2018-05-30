#---------------------------------------------------------------------
#
# Utilities to communicate with the qualtrics API
#
# - set bearer token
# - create a header to POST to API
# - send a request to the API
#
#---------------------------------------------------------------------

# Helper that fetches a bearer token for user if they use oauth method
qualtrics_set_bearer_token <- function() {

  # body for requests
  body <- list(
    grant_type = "client_credentials"
  )

  # If use keychain
  if(Sys.getenv("QUALTRICS_USE_KEYCHAIN") == "") {

    stop("You must first register your credentials using 'qualtrics_authenticate()'")

  } else if(Sys.getenv("QUALTRICS_USE_KEYCHAIN")) {

    # Retrieve OS
    os <- Sys.getenv("QUALTRICS_SYS_OS")

    # Set data center
    data_center <- switch (

      os,
      osx = keyringr::decrypt_kc_pw("qualtrics_data_center"),
      linux = keyringr::decrypt_gk_pw("key qualtrics_data_center")

    )

    # Set as env. variable
    Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

    res <- switch(
      os,
      # mac
      osx = httr::POST(paste0("https://",
                              Sys.getenv("QUALTRICS_DATA_CENTER"),
                              ".qualtrics.com/oauth2/token"),
                       encode = "form",
                       body = body,
                       httr::authenticate(keyringr::decrypt_kc_pw("qualtrics_api_client"),
                                          keyringr::decrypt_kc_pw("qualtrics_api_secret"),
                                          type = "basic")),
      # linux
      linux = httr::POST(paste0("https://",
                                Sys.getenv("QUALTRICS_DATA_CENTER"),
                                ".qualtrics.com/oauth2/token"),
                         encode = "form",
                         body = body,
                         httr::authenticate(keyringr::decrypt_gk_pw("key qualtrics_api_client"),
                                            keyringr::decrypt_gk_pw("key qualtrics_api_secret"),
                                            type = "basic"))
    )

  } else { # User does not use keychain

    # Get bearer token
    res <- httr::POST(paste0("https://",
                             Sys.getenv("QUALTRICS_DATA_CENTER"),
                             ".qualtrics.com/oauth2/token"),
                      encode = "form",
                      body = body,
                      httr::authenticate(Sys.getenv("QUALTRICS_CLIENT_ID"),
                                         Sys.getenv("QUALTRICS_CLIENT_SECRET"),
                                         type = "basic"))

  }

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

    # If keychain
    if(Sys.getenv("QUALTRICS_USE_KEYCHAIN")) {

      os <- Sys.getenv("QUALTRICS_SYS_OS")

      # Set data center as env variable
      data_center <- switch(

        os,
        osx = keyringr::decrypt_kc_pw("qualtrics_data_center"),
        linux = keyringr::decrypt_gk_pw("key qualtrics_data_center")

      )

      Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

      switch(

        os,
        osx = c(
          'X-API-TOKEN' = keyringr::decrypt_kc_pw("qualtrics_api_token"),
          'Content-Type' = "application/json",
          'Accept' = '*/*',
          'accept-encoding' = 'gzip, deflate'
        ),
        linux = c(
          'X-API-TOKEN' = keyringr::decrypt_gk_pw("key qualtrics_api_token"),
          'Content-Type' = "application/json",
          'Accept' = '*/*',
          'accept-encoding' = 'gzip, deflate'
        )
      )

    } else { # not use keychain

      c(
        'X-API-TOKEN' = Sys.getenv("QUALTRICS_API_TOKEN"),
        'Content-Type' = "application/json",
        'Accept' = '*/*',
        'accept-encoding' = 'gzip, deflate'
      )

    }

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
                                     body = NULL) {
  # Match arg
  verb <- match.arg(verb)

  # If use oauth ...
  if(Sys.getenv("QUALTRICS_AUTH_TYPE") == "oauth") {

    # If empty, set
    if(Sys.getenv("QUALTRICS_OAUTH_TOKEN") == "") {

      # Set bearer
      qualtrics_set_bearer_token()

    }

  }

  # Must set data center
  if(Sys.getenv("QUALTRICS_DATA_CENTER") == "") {

    stop("Data center not found. Did you register your credentials using 'qualtrics_authenticate()'?")

  }

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
            message("Retrieving new bearer token ... ")
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

# Checks responses against qualtrics response codes and returns error message.
#
# @param res response from httr::GET
# @param raw if TRUE, add 'raw' flag to httr::content() function.
#
# @author Jasper Ginn

qualtRicsResponseCodes <- function(res, raw=FALSE) {
  # Check status code and raise error/warning
  if(res$status_code == 200) {
    if(raw) {
      result <- httr::content(res, "raw")
    } else {
      result <- httr::content(res)
    }
    return(list(
      "content" = result,
      "OK" = TRUE
    )
    )
  } else if(res$status_code == 401) {
    stop("Qualtrics API raised an authentication (401) error - you may not have the\nrequired authorization. Please check your API key and root url.") # nolint
  } else if(res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on\nhttps://github.com/ropensci/qualtRics/issues") # nolint
  } else if(res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error).\nPlease check if you are using the correct survey ID.") # nolint
  } else if(res$status_code == 500) {
    stop(paste0("Qualtrics API reports an internal server (500) error. Please contact\nQualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n", # nolint
                "\n",
                "instanceId:", " ",
                httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ",
                httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    ))
  } else if(res$status_code == 503) {
    stop(paste0("Qualtrics API reports a temporary internal server (500) error. Please\ncontact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and\nerrorCode below or retry your query.", "\n", # nolint
                "\n",
                "instanceId:", " ", httr::content(res)$meta$error$instanceId,
                "\n",
                "errorCode: ", httr::content(res)$meta$error$errorCode))
    return(list(
      "content" = httr::content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a\nmultipart/form-data request is malformed.") # nolint
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  }
}

# Check if httr GET result contains a warning
#
# @param resp object returned by 'qualtRicsResponseCodes()'
#
# @author Jasper Ginn

checkForWarnings <- function(resp) {
  # Raise warning if resp contains notice
  if(!is.null(resp$content$meta)) {
    if(!is.null(resp$content$meta$notice)) {
      warning(resp$content$meta$notice)
    }
  }
  NULL
}
