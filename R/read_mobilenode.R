#' Read and combine mobile-node data files. See details.
#'
#' @details Combines mobile-node data files from a given year. The user provides
#' the directory containing the mobile-node `csv` files and a regex pattern to
#' retrieve the mobile node data files. These files are combined when possible
#' into a data frame with tidy column names and proper data types.
#'
#' @param path A character vector of full path name
#' @param pattern A regular expression. Only file names which match the regular
#' expression will be returned.
#' @param year A character string, YYYY, indicating the year under study.
#'
#' @keywords internal
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' if(FALSE) {
#' read_mobilenode(path = 'dulogs/2022', pattern = 'MN_DATA', year = 2022)
#' }
read_mobilenode <-
  function(path,
           pattern,
           year) {
    # get all files in the path
    mnData <- list.files(path, pattern = pattern, full.names = TRUE)
    # check if any were found
    if (length(mnData) == 0) {
      stop('No tag data sources found in the specified path. Check specified path and pattern.')
    }
    # check if year is proper form
    year <- as.character(year)
    if (!stringr::str_detect(year, '^\\d{4}$')) {
      stop('Year (YYYY) must be a constant string.')
    }

    # read and combine data
    mnData <- mnData %>%
      purrr::map_dfr(., function(f)
        purrr::quietly(readr::read_csv)(f)$result %>%
          tibble::add_column(., origin = basename(f), .before = 1)) %>%
      # format column names
      janitor::clean_names()

    # check that the timezone is consistent for all values
    tzone <- unique(mnData$tz)
    if (length(tzone) != 1) {
      stop('Data are from multiple timezones. Unify dates before running read_mobilenode.')
    }

    # combine date information into R date object
    mnData %>%
      # create data objects
      dplyr::mutate(
        .,
        meeting_date = lubridate::make_datetime(
          year = meeting_year,
          month = meeting_month,
          day = meeting_day,
          hour = meeting_hour,
          min = meeting_minute,
          sec = meeting_second,
          tz = tzone
        ),
        received_date = lubridate::make_datetime(
          year = received_year,
          month = received_month,
          day = received_day,
          hour = received_hour,
          min = received_minute,
          sec = received_second,
          tz = tzone
        )
      ) %>%
      dplyr::select(origin,
                    meeting_date,
                    rx_node,
                    tx_node,
                    rssi,
                    received_date) %>%
      # add year of study
      tibble::add_column(., year = !!year, .before = 1) %>%
      # make times utc time
      dplyr::mutate(
        .,
        meeting_date = lubridate::with_tz(meeting_date, tzone = 'UTC'),
        received_date = lubridate::with_tz(received_date, tzone = 'UTC'),
        rx_node = as.character(rx_node),
        tx_node = as.character(tx_node)
      )
  }
