#' Read and combine in range data files. See details.
#'
#' @details Combines in range data files from a given year. The user provides
#' the directory containing the in range `csv` files and a regex pattern to
#' retrieve the in range files. These files are combined when possible
#' into a data frame with tidy column names and proper data types.
#'
#' @param path A character vector of full path name.
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
#' irData <- read_inrange(path = 'dulogs/2022', pattern = 'MN_RANGE', year = 2022)
#' }
read_inrange <-
  function(path,
           pattern,
           year) {
    # get all files in the path
    irData <- list.files(path, pattern = pattern, full.names = TRUE)
    # check if any were found
    if (length(irData) == 0) {
      stop(
        'No in range data sources found in the specified path. Check specified path and pattern.'
      )
    }
    # check if year is proper form
    year <- as.character(year)
    if (!stringr::str_detect(year, '^\\d{4}$')) {
      stop('Year (YYYY) must be a constant string.')
    }

    # read and combine data
    irData <- irData %>%
      purrr::map_dfr(., function(f)
        purrr::quietly(readr::read_csv)(f)$result %>%
          tibble::add_column(., origin = basename(f), .before = 1)) %>%
      # format column names
      janitor::clean_names()

    # check that the timezone is consistent for all values
    tzone <- unique(irData$tz)
    if (length(tzone) != 1) {
      stop('Data are from multiple timezones. Unify dates before running read_mobilenode.')
    }

    irData %>%
      dplyr::mutate(
        node_id = as.character(node_id),
        range_date = lubridate::make_datetime(
          year = year,
          month = month,
          day = day,
          hour = hour,
          min = minute,
          sec = second,
          tz = tzone
        )
      ) %>%
      dplyr::select(origin,
                    range_date,
                    node = node_id,
                    rssi,
                    memory_usage) %>%
      # add year of study
      tibble::add_column(., year = !!year, .before = 1) %>%
      # make times utc time
      dplyr::mutate(., range_date = lubridate::with_tz(range_date, tzone = 'UTC'))
  }
