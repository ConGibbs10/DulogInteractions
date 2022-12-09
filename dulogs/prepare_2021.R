#-------------------------------------------------------------------------------
# prepare_2021.R
# Takes the 2021 data and
#  (1) add year of investigation,
#  (2) add timezone times are recorded in
#-------------------------------------------------------------------------------

library(dplyr)

#-----------------------------
# Base station data (in range)
#-----------------------------

# point user to raw dulogs for 2022
path <- 'dulogs-raw/2021/In range/'
files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

# create files names from directory
new_files <- files %>%
  stringr::str_remove(., path) %>%
  stringr::str_c('dulogs/2021/', .)

# copy files over to dulogs with new name
file.copy(from = files, to = new_files)

# open and add year to files
for(f in new_files) {
  mnInRange <- suppressMessages(readr::read_csv(f))
  mnInRange <- mnInRange %>%
    tibble::add_column(., year = 2021, .before = 'month') %>%
    tibble::add_column(., tz = 'UTC', .after = 'second')
  suppressMessages(readr::write_csv(mnInRange, file = f))
}

#-----------------------
# Tag data (mobile node)
#-----------------------

# point user to raw dulogs for 2022
path <- 'dulogs-raw/2021/Mobile node data'
files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

# create files names from directory
new_files <- files %>%
  stringr::str_remove(., path) %>%
  stringr::str_c('dulogs/2021', .)

# copy files over to dulogs with new name
file.copy(from = files, to = new_files)

# open and add year to files
for(f in new_files) {
  mn <- suppressMessages(readr::read_csv(f))
  mn <- mn %>%
    tibble::add_column(., 'Meeting Year' = 2021, .before = 'Meeting Month') %>%
    tibble::add_column(., 'Received Year' = 2021, .before = 'Received Month') %>%
    tibble::add_column(., 'MN Year' = 2021, .before = 'MN Month') %>%
    tibble::add_column(., tz = 'UTC', .after = 'Meeting Second')
  suppressMessages(readr::write_csv(mn, file = f))
}
