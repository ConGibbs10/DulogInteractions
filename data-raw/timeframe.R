library(dplyr)

# read timeframe from csv
timeframe_raw <- readr::read_csv('data-raw/timeframe-raw.csv',
                                 col_types = 'ccdddddddd')
# create dates one year at a time before binding
## can't use timezones in a vectorized way
timeframe <- timeframe_raw %>%
  purrr::pmap(., function(year,
                          tz,
                          start_year,
                          start_month,
                          start_day,
                          start_hour,
                          end_year,
                          end_month,
                          end_day,
                          end_hour) {
    tibble::tibble(
      year = year,
      start = lubridate::make_datetime(
        year = start_year,
        month = start_month,
        day = start_day,
        hour = start_hour,
        tz = tz
      ),
      end = lubridate::make_datetime(
        year = end_year,
        month = end_month,
        day = end_day,
        hour = end_hour,
        tz = tz
      )
    )
  }) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(., start = lubridate::with_tz(start, tzone = 'UTC'),
                end = lubridate::with_tz(end, tzone = 'UTC'))

# write data
usethis::use_data(timeframe, overwrite = TRUE)
