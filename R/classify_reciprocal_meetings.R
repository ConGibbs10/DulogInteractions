#' Append whether meeting is deemed reciprocal.
#'
#' @details At a given meeting time, a receiver registers a signal from a sender.
#' This meeting is deemed reciprocal if the sender also registers a signal from
#' the sender no more than `time_eps` seconds before or after the meeting time.
#'
#' @param pdulogs Data frame of pre-processed dulog data.
#' @param time_eps A numeric for the maximum number of seconds the timestamp's can
#' deviate while classifying a meeting as reciprocal (see details).
#'
#' @keywords internal
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' if(FALSE) {
#' dulogs <- read_dulogs(
#' paths = c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022'),
#' patterns = c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE'),
#' year = 2022,
#' time_eps = 2,
#' barn_rule = 'or')
#' )
#' pdulogs <- preprocess_dulogs(dulogs)
#' classify_reciprocal_meetings(pdulogs, time_eps = 2)
#' }
classify_reciprocal_meetings <- function(dulogs, time_eps = 2) {
  # check time gap
  if (length(time_eps) > 1 ||
      !is.numeric(time_eps) || time_eps < 0) {
    stop('Argument time_eps must be a non-negative numeric scalar.')
  }

  intervals <- dulogs %>%
    dplyr::mutate(
      .,
      start = meeting_date - lubridate::seconds(time_eps),
      end = meeting_date + lubridate::seconds(time_eps)
    ) %>%
    dplyr::select(., year, i_node, j_node, start, end) %>%
    dplyr::distinct()

  dulogs <-
    dplyr::mutate(dulogs, meeting_date_end = meeting_date, .after = meeting_date)

  # use data.table to find reciprocated meetings
  ## check if both swapped and nonswapped in overlaps
  data.table::setDT(dulogs)
  data.table::setDT(intervals)
  data.table::setkey(intervals, year, i_node, j_node, start, end)
  reciprocated <-
    data.table::foverlaps(
      dulogs,
      by.x = c(
        'year',
        'i_node',
        'j_node',
        'meeting_date',
        'meeting_date_end'
      ),
      intervals
    )
  reciprocated <-
    reciprocated[, list('reciprocated' = all(c(any(swapped), any(!swapped)))), by = c('year', 'i_node', 'j_node', 'start', 'end')]

  # summarize and return
  reciprocated <- tibble::as_tibble(reciprocated)
  dulogs <- dulogs %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      .,
      start = meeting_date - lubridate::seconds(time_eps),
      end = meeting_date + lubridate::seconds(time_eps)
    ) %>%
    dplyr::left_join(.,
                     reciprocated,
                     by = c('year', 'i_node', 'j_node', 'start', 'end')) %>%
    dplyr::select(.,-meeting_date_end,-start,-end)

  return(dulogs)
}
