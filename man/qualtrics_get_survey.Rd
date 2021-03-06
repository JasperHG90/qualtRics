% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R, R/qualtrics_get_survey.R
\name{getSurvey}
\alias{getSurvey}
\alias{qualtrics_get_survey}
\title{Export a Survey and Download it Into R}
\usage{
getSurvey(surveyID, lastResponseId = NULL, startDate = NULL,
  endDate = NULL, seenUnansweredRecode = NULL, limit = NULL,
  includedQuestionIds = NULL, saveDir = NULL, forceRequest = FALSE, ...)

qualtrics_get_survey(surveyID, lastResponseId = NULL, startDate = NULL,
  endDate = NULL, seenUnansweredRecode = NULL, limit = NULL,
  includedQuestionIds = NULL, saveDir = NULL, forceRequest = FALSE, ...)
}
\arguments{
\item{surveyID}{String. Unique ID for the survey you want to download. Returned as 'id' by the \link[qualtRics]{getSurveys} function.}

\item{lastResponseId}{String. Export all responses received after the specified response. Defaults to NULL.}

\item{startDate}{String. Filter to only exports responses recorded after the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.}

\item{endDate}{String. Filter to only exports responses recorded before the specified date. Accepts dates as character strings in format "YYYY-MM-DD". Defaults to NULL.}

\item{seenUnansweredRecode}{String. Recode seen but unanswered questions with a string value. Defaults to NULL.}

\item{limit}{Integer. Maximum number of responses exported. Defaults to NULL (all responses).}

\item{includedQuestionIds}{Vector of strings (e.g. c('QID1', 'QID2', 'QID3'). Export only specified questions. Defaults to NULL.}

\item{saveDir}{String. Directory where survey results will be stored. Defaults to a temporary directory which is cleaned when your R session is terminated. This argument is useful if you'd like to store survey results. The downloaded survey will be stored as an RDS file (see \link[base]{readRDS}).}

\item{forceRequest}{Logical. getSurvey() saves each survey in a temporary directory so that it can quickly be retrieved later. If forceRequest is TRUE, getSurvey() always downloads the survey from the API instead of loading it from the temporary directory. Defaults to FALSE.}

\item{...}{optional arguments. You can pass all arguments listed in \code{\link{registerOptions}} (except a different base url / api key). You can also pass a argument 'fileEncoding' (see 'fileEncoding' argument in \code{\link{readSurvey}}) to import your survey using a specific encoding.}
}
\description{
Export a qualtrics survey you own and import the survey directly into R.
}
\examples{
\dontrun{
# Register your Qualtrics credentials if you haven't already
registerOptions(api_token = "<YOUR-API-TOKEN>",
                base_url = "<YOUR-ROOT-URL>")
# Retrieve a list of surveys
surveys <- getSurveys()
# Retrieve a single survey
mysurvey <- getSurvey(surveyID = surveys$id[6],
                      saveDir = tempdir(),
                      verbose = TRUE)
# You can use the same parameters as those found in the qualtrics API documentation
# Found here: https://api.qualtrics.com/docs/csv
mysurvey <- getSurvey(surveyID = surveys$id[6],
                      saveDir = tempdir(),
                      startDate = "2017-01-01",
                      endDate = "2017-01-31",
                      limit = 100,
                      useLabels = TRUE,
                      seenUnansweredRecode = "UNANS",
                      verbose = TRUE)
# You can also choose to only download an export with specific questions using
# the \\code{\\link[getSurveyQuestions]{getSurveyQuestions}} function.

# Retrieve questions for a survey
questions <- getSurveyQuestions(surveyID = surveys$id[6])
# Retrieve a single survey, filtering for questions I want.
mysurvey <- getSurvey(surveyID = surveys$id[6],
                      saveDir = tempdir(),
                      includedQuestionIds = c("QID1", "QID2", "QID3"),
                      verbose = TRUE)
}
}
\seealso{
See \url{https://api.qualtrics.com/docs/csv} for documentation on the Qualtrics API.
}
\author{
Jasper Ginn
}
