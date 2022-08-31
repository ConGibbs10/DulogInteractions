#' Tags and pairs for barn swallows.
#'
#' A dataset containing the tags, sex, and pairs for each of the 34 barn swallows
#' from 2021 and beyond.
#'
#' @format A data frame with 34 rows and 7 variables:
#' \describe{
#'   \item{year}{year the data were collected}
#'   \item{band_id}{unique 9-digit band number; the bird wears the band on one of its legs where leg is part of the unique-to-site color combination}
#'   \item{first_nest}{number of the first nest (first brood)}
#'   \item{second_nest}{number of the second nest (second brood)}
#'   \item{tag}{Dulog tag ID}
#'   \item{sex}{sex of the bird; confirmed via  observations with mate and sometimes checked molecularly}
#'   \item{recap}{whether bird has been banded by researchers in prior years}
#'   \item{first_banded}{year researchers first banded the bird}
#'   \item{age_cat}{standard ornithological age categories}
#'   \item{age}{age of the birds}
#'   \item{main_barn}{whether bird bred in the main barn}
#'   \item{pair_tag}{Dulog tag ID for mate; confirmed via observations of the pair feeding nestlings at a particular nest}
#'   \item{pair_id}{unique 9-digit band number for mate}
#'   \item{MB_side}{which leg hosts the metal band with the band_id}
#'   \item{top_color}{color of the plastic band that is closest to the bird's body}
#'   \item{bottom_color}{color of the plastic band that is closest to the bird's feet}
#'   \item{date_captured}{date of first capture in the given year}
#'   \item{avg_rwl}{average right wing length in mm}
#'   \item{lts_intact}{whether left tail streamer is intact}
#'   \item{avg_lts}{average length of left tail streamer in mm}
#'   \item{rts_intact}{whether right tail streamer is intact}
#'   \item{avg_rts}{average length of right tail streamer in mm}
#'   \item{feathers_sampled}{whether the ventral feathers have been been collected to quantify plumage color}
#'   \item{mass}{mass of the bird}
#'   \item{blood_sampled}{whether the blood has been sampled to test paternity and malaria}
#'   \item{feather_mites}{count of feather mites on the bird's left wing}
#'   \item{malaria}{whether malaria parasite DNA was found in the bird's blood}
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
