#' Append whether the sender and receiver nodes were registered by the base station.
#'
#' @details If the sender/receiver node is registered by a base station no more
#' than `time_eps` seconds before or after the time of the meeting, then the
#' sender/receiver is deemed near the base station at the time of the meeting.
#' The meeting is deemed near the base station according to the `barn_rule`.
#'
#' @param mnData Mobile node data frame.
#' @param irData In range data frame.
#' @param time_eps A numeric for the maximum number of seconds the in range timestamp
#' can deviate from the mobile node timestamp while claiming a bird is in the barn.
#' @param barn_rule Rule for determining whether the meeting took place in barn.
#' Option `and` signifies both birds need be in the barn; whereas option `or`
#' signifies at least one bird needs to be in the barn.
#'
#' @keywords internal
#'
#' @return A data frame.
#' @export
#'
#' @examples
#'
#' if(FALSE) {
#' mnData <- read_mobilenode(path = 'dulogs/2022', pattern = 'MN_DATA', year = 2022)
#' irData <- read_inrange(path = 'dulogs/2022', pattern = 'MN_RANGE', year = 2022)
#' classify_barn_meetings(mnData = mnData, irData = irData, time_eps = 2, barn_rule = 'or')
#' }
classify_barn_meetings <- function(mnData, irData, time_eps = 2, barn_rule = 'or') {
  # check time epsilon
  if (length(time_eps) != 1 || !is.numeric(time_eps) || time_eps < 0) {
    stop('time_eps should be a nonnegative numeric scalar.')
  }
  if (!barn_rule %in% c('or', 'and')) {
    stop("barn_rule must be either 'or' or 'and'.")
  }

  # mnData in long form
  lmnData <- mnData %>%
    dplyr::mutate(., start = meeting_date - lubridate::seconds(time_eps),
                  end = meeting_date + lubridate::seconds(time_eps)) %>%
    tidyr::pivot_longer(., cols = c('rx_node', 'tx_node'), names_to = 'type', values_to = 'node') %>%
    dplyr::select(., year, type, node, meeting_date, start, end) %>%
    dplyr::distinct(., .keep_all = TRUE)

  lmnDataNode <- lmnData %>%
    dplyr::select(., -type) %>%
    dplyr::distinct(., .keep_all = TRUE)

  # irData in long form
  lirData <- irData %>%
    dplyr::mutate(.,
                  range_date_end = range_date,
                  barn = TRUE) %>%
    dplyr::select(., year, node, barn, range_date, range_date_end)

  # use data.table to find if bird (either rx or tx was in the barn within
  # time_eps of meeting)
  ## check if range file exists within epsilon of the meeting_date
  data.table::setDT(lmnDataNode)
  data.table::setDT(lirData)
  data.table::setkey(lirData, year, node, range_date, range_date_end)
  inBarn <- data.table::foverlaps(lmnDataNode, by.x = c('year', 'node', 'start', 'end'), lirData)
  inBarn <- inBarn[, barn := ifelse(is.na(barn), FALSE, barn)]
  inBarn <- inBarn[, list('barn' = any(barn), 'range_date' = range_date[1]), by = c('year', 'node', 'meeting_date')]

  # join by sender and receiver
  inBarn <- inBarn %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(., dplyr::select(lmnData, year, type, node, meeting_date),
                     by = c('year', 'node', 'meeting_date'))

  mnData <- mnData %>%
    dplyr::left_join(
      .,
      inBarn %>%
        dplyr::filter(., type == 'rx_node') %>%
        dplyr::select(., year, meeting_date, rx_node = node, rx_barn = barn, rx_range_date = range_date),
      by = c('year', 'meeting_date', 'rx_node')
    ) %>%
    dplyr::left_join(
      .,
      inBarn %>%
        dplyr::filter(., type == 'tx_node') %>%
        dplyr::select(., year, meeting_date, tx_node = node, tx_barn = barn, tx_range_date = range_date),
      by = c('year', 'meeting_date', 'tx_node')
    )

  if (barn_rule == 'and') {
    mnData <- dplyr::mutate(mnData, barn = rx_barn & tx_barn)
  } else if (barn_rule == 'or') {
    mnData <- dplyr::mutate(mnData, barn = rx_barn | tx_barn)
  } else {
    stop("Argument barn_rule must be either 'and' or 'or'.")
  }

  # sort columns
  mnData <-
    dplyr::select(
      mnData,
      year,
      origin,
      meeting_date,
      rx_node,
      rx_range_date,
      rx_barn,
      tx_node,
      tx_range_date,
      tx_barn,
      barn,
      rssi,
      received_date,
      dplyr::everything()
    )

  return(mnData)
}
