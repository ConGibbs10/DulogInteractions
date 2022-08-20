#' Tags and pairs for barn swallows.
#'
#' A dataset containing the tags, sex, and pairs for each of the 34 barn swallows
#' from 2021 and beyond.
#'
#' @format A data frame with 34 rows and 7 variables:
#' \describe{
#'   \item{year}{year of study}
#'   \item{nest_id}{nest ID}
#'   \item{nest}{nest}
#'   \item{bird_id}{bird ID}
#'   \item{tag}{tag number}
#'   \item{sex}{sex}
#'   \item{sex}{tag number for pair}
#' }
#' @source \url{https://docs.google.com/document/d/15JlJuqI2GuHtTGzNFy0lefYcYhPoZQKN/edit?usp=sharing&ouid=112915548451337201344&rtpof=true&sd=true}
"birds"

#' Time frame of barnswallow study.
#'
#' A dataset containing the start and end dates for the 2021 barn swallow study
#' and beyond. The start and end times dictate when data are collected from the
#' swallows each day under study.
#'
#' @format A data frame with 1 row and 3 variables:
#' \describe{
#'   \item{year}{year of study}
#'   \item{start}{start date and time of data collection, format: HH:MM:SS YYYY-MM-DD UTC}
#'   \item{end}{end date and time of data collection, format: HH:MM:SS YYYY-MM-DD UTC}
#' }
#' @source \url{https://docs.google.com/document/d/15JlJuqI2GuHtTGzNFy0lefYcYhPoZQKN/edit?usp=sharing&ouid=112915548451337201344&rtpof=true&sd=true}
"timeframe"
