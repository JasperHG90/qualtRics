# New authenticate function

qualtrics_authenticate <- function(type = c("token", "oauth"),
                                   use_keychain = FALSE,
                                   ...) {

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

  } else {

    user_passed_secret <- NULL

  }
  if("client_id" %in% names(opts)) {

    user_passed_id <- opts$client_id

  } else {

    user_passed_id <- NULL

  }
  if("api_token" %in% names(opts)) {

    user_passed_token <- opts$api_token

  } else {

    user_passed_token <- NULL

  }
  if("data_center" %in% names(opts)) {

    user_passed_datacenter <- opts$data_center

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
      client_id <- cred$client_id
      client_secret <- cred$client_secret
      data_center <- cred$data_center

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
      api_token <- cred$token
      data_center <- cred$data_center

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

  # Get OS
  os <- get_os()

  if(type == "oauth") {

    client_id <- switch(
      os,
      "osx" = keyringr::decrypt_kc_pw("qualtrics_api_client"),
      "linux" = keyringr::decrypt_gk_pw("qualtrics_api_client")
    )
    client_secret <- switch(
      os,
      "osx" = keyringr::decrypt_kc_pw("qualtrics_api_secret"),
      "linux" = keyringr::decrypt_gk_pw("qualtrics_api_secret")
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = keyringr::decrypt_kc_pw("qualtrics_data_center"),
      "linux" = keyringr::decrypt_gk_pw("qualtrics_data_center")
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
      "osx" = keyringr::decrypt_kc_pw("qualtrics_api_token"),
      "linux" = keyringr::decrypt_gk_pw("qualtrics_api_token")
    )

    # Data center
    data_center <- switch(
      os,
      "osx" = keyringr::decrypt_kc_pw("qualtrics_data_center"),
      "linux" = keyringr::decrypt_gk_pw("qualtrics_data_center")
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

    if(Sys.getenv("QUALTRICS_API_TOKEN") == "") {

      warning("Qualtrics api token is not registered")

    }

  } else if (type == "oauth") {

    if(Sys.getenv("QUALTRICS_CLIENT_ID") == "") {

      warning("Qualtrics client id is not registered")

    }

    if(Sys.getenv("QUALTRICS_CLIENT_SECRET") == "") {

      warning("Qualtrics client secret is not registered")

    }

  }

  if(Sys.getenv("QUALTRICS_DATA_CENTER") == "") {

    warning("Qualtrics data center is not registered")

  }

}
