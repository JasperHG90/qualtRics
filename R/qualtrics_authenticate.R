# New authenticate function

#' Authenticate your api token, data center and oauth credentials
#'
#' @param type either one of 'token' (API credentials) or 'oauth' (using secret and id)
#' @param use_keychain should qualtRics retrieve stored credentials from the keychain?
#' @param ... any of the options (api_token, data_center, client_id, client_secret) can be passed as named objects. If you pass these parameters, they will override e.g. environment variables.
#'
#' @return exits silently
#'
#' @import keyringr
#'
#' @export

qualtrics_authenticate <- function(type = c("token", "oauth"),
                                   use_keychain = FALSE,
                                   ...) {

  # Set use keychain to false
  Sys.setenv("QUALTRICS_USE_KEYCHAIN" = FALSE)

  # Plan ----

   # Depending on the type (token or oauth)
    # 1. Check environment variables for tokens (set via e.g. Rprofile)
    #    OR
    #     IF use_keychain
    #       check keychain for tokens (using keyringr package)
    #    OR
    #    Check the optional args. If client_secret, client_auth, data_center or api_token passed
    #    then use these.
    # 2. Set environment variables

  # Code ----

  # Get type
  type <- match.arg(type)

  # Get optional args
  opts <- list(...)
  if("client_secret" %in% names(opts)) {

    user_passed_secret <- opts$client_secret

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
in the local environment. If you want to override the default options, set 'use_keychain'
to FALSE."
      )

    }

  } else {

    user_passed_secret <- NULL

  }

  if("client_id" %in% names(opts)) {

    user_passed_id <- opts$client_id

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
in the local environment. If you want to override the default options, set 'use_keychain'
to FALSE."
      )

    }

  } else {

    user_passed_id <- NULL

  }

  if("api_token" %in% names(opts)) {

    user_passed_token <- opts$api_token

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
in the local environment. If you want to override the default options, set 'use_keychain'
to FALSE."
      )

    }

  } else {

    user_passed_token <- NULL

  }

  if("data_center" %in% names(opts)) {

    user_passed_datacenter <- opts$data_center

    if(use_keychain) {

      warning(
        "'use_keychain' is set to TRUE and options passed to this function will not be saved
in the local environment. If you want to override the default options, set 'use_keychain'
to FALSE."
      )

    }

  } else {

    user_passed_datacenter <- NULL

  }

  # Split on auth type
  if(type == "oauth") {

    client_id <- Sys.getenv("QUALTRICS_CLIENT_ID")
    client_secret <- Sys.getenv("QUALTRICS_CLIENT_SECRET")
    data_center <- Sys.getenv("QUALTRICS_DATA_CENTER")

    # Check keychain
    if(use_keychain) {

      cred <- qualtrics_helper_keychain_credentials("oauth")

      check <- vapply(cred, function(x) {

        if(!is.null(x)) {
            if(length(x) > 0) {
              if(x > 0) {
                TRUE
              } else {
                FALSE
              }
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        },
        TRUE)

      if(!all(check)) {

        stop(
          "An error occurred while retrieving your credentials from the keychain. Ensure that
you set your credentials correctly or use another method to authenticate."
        )

      }

      Sys.setenv("QUALTRICS_USE_KEYCHAIN" = TRUE)

    }

    # If user passed credentials, override
    if(!is.null(user_passed_secret)) {

      client_secret <- user_passed_secret

    }
    if(!is.null(user_passed_id)) {

      client_id <- user_passed_id

    }
    if(!is.null(user_passed_datacenter)) {

      data_center <- user_passed_datacenter

    }

    # Set envs
    Sys.setenv("QUALTRICS_CLIENT_ID" = client_id)
    Sys.setenv("QUALTRICS_CLIENT_SECRET" = client_secret)
    Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

    # Set auth type
    Sys.setenv("QUALTRICS_AUTH_TYPE" = "oauth")

    # Check
    qualtrics_helper_envs_set("oauth")

  } else if(type == "token") {

    api_token <- Sys.getenv("QUALTRICS_API_TOKEN")
    data_center <- Sys.getenv("QUALTRICS_DATA_CENTER")

    # Check keychain
    if(use_keychain) {

      cred <- qualtrics_helper_keychain_credentials("token")

      check <- vapply(cred, function(x) {

        if(!is.null(x)) {
          if(length(x) > 0) {
            if(x > 0) {
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        } else {
          FALSE
        }
      },
      TRUE)

      if(!all(check)) {

        stop(
          "An error occurred while retrieving your credentials from the keychain.\nEnsure that you set your credentials correctly or use another method to authenticate."
        )

      }

      Sys.setenv("QUALTRICS_USE_KEYCHAIN" = TRUE)

    }

    # If user passed credentials, override
    if(!is.null(user_passed_token)) {

      api_token <- user_passed_token

    }
    if(!is.null(user_passed_datacenter)) {

      data_center <- user_passed_datacenter

    }

    # Set envs
    Sys.setenv("QUALTRICS_API_TOKEN" = api_token)
    Sys.setenv("QUALTRICS_DATA_CENTER" = data_center)

    # Set auth type
    Sys.setenv("QUALTRICS_AUTH_TYPE" = "token")

    # Check
    qualtrics_helper_envs_set("token")

  }

  # Exit

}

# Helper function. Retrieves API credentials from keychain
qualtrics_helper_keychain_credentials <- function(type = c("oauth", "token")) {

  type <- match.arg(type)

  # Get OS variable
  os <- Sys.getenv("QUALTRICS_SYS_OS")

  # If empty, call function to retrieve OS
  if(os == "") {

    os <- get_os()

    Sys.setenv("QUALTRICS_SYS_OS" = os)

  }

  if(type == "oauth") {

    client_id <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_client")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_client"))
    )

    client_secret <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_secret")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_secret"))
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_data_center")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_data_center"))
    )

    # Return
    list(
      "client_id" = client_id,
      "client_secret" = client_secret,
      "data_center" = data_center
    )

  } else if(type == "token") {

    # Api & root url
    api_token <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_api_token")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_api_token"))
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = nchar(keyringr::decrypt_kc_pw("qualtrics_data_center")),
      "linux" = nchar(keyringr::decrypt_gk_pw("key qualtrics_data_center"))
    )

    # Return
    list(
      "token" = api_token,
      "data_center" = data_center
    )

  }

}

# Helper function. Raises warning if one or more environment variables are not set
qualtrics_helper_envs_set <- function(type = c("oauth", "token")) {

  type <- match.arg(type)

  # If token
  if(type == "token") {

    if(Sys.getenv("QUALTRICS_API_TOKEN") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics api token is not registered")

    }

  } else if (type == "oauth") {

    if(Sys.getenv("QUALTRICS_CLIENT_ID") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics client id is not registered")

    }

    if(Sys.getenv("QUALTRICS_CLIENT_SECRET") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

      warning("Qualtrics client secret is not registered")

    }

  }

  if(Sys.getenv("QUALTRICS_DATA_CENTER") == "" & Sys.getenv("QUALTRICS_USE_KEYCHAIN") == FALSE) {

    warning("Qualtrics data center is not registered")

  }

}

# Helper function to determine type of OS
# Todo: add windows
get_os <- function(){

  sysinf <- Sys.info()

  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }

  tolower(os)

}
