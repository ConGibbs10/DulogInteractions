#-------------------------------------------------------------------------------
# prepare_2022.R
# Takes the 2022 data and
#  (1) constructs unique and tidy file names from complex directories
#  (2) add timezone times are recorded in
#-------------------------------------------------------------------------------

library(dplyr)

#------------------
# Base station data
#------------------

# point user to raw dulogs for 2022
path <- 'dulogs-raw/2022/Base station during deployment back ups-20220708T160816Z-001'
files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

# create files names from directory
new_files <- files %>%
  stringr::str_remove(., path) %>%
  stringr::str_remove(., 'MN_RANGE.CSV') %>%
  stringr::str_replace_all(., '/', ' ') %>%
  stringr::str_squish() %>%
  janitor::make_clean_names() %>%
  stringr::str_c('dulogs/2022/', 'MN_RANGE_', ., '.csv')

# copy files over to dulogs with new name
file.copy(from = files, to = new_files)

# open and add timezones
for(f in new_files) {
  mnInRange <- suppressMessages(readr::read_csv(f))
  mnInRange <- tibble::add_column(mnInRange, tz = 'America/Virgin', .after = 'second')
  suppressMessages(readr::write_csv(mnInRange, file = f))
}

#------------------
# Tag data
#------------------

# point user to raw dulogs for 2022
path <- 'dulogs-raw/2022/Data from tags - downloaded post deployment-20220708T160812Z-001'
files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

# create files names from directory
new_files <- files %>%
  stringr::str_remove(., path) %>%
  stringr::str_remove(., '.CSV') %>%
  stringr::str_replace_all(., '/', ' ') %>%
  stringr::str_replace(., 'tag', 'Tag') %>%
  stringr::str_squish() %>%
  stringr::str_c('dulogs/2022/', ., '.csv')

# copy files over to dulogs with new name
file.copy(from = files, to = new_files)

# open and add year to files
for(f in new_files) {
  mn <- suppressMessages(readr::read_csv(f))
  mn <- tibble::add_column(mn, tz = 'America/Virgin', .after = 'Meeting Second')
  suppressMessages(readr::write_csv(mn, file = f))
}

#------------------
# Tag data, round 2
#------------------

# point user to raw dulogs for 2022
path <- 'dulogs-raw/2022/Data from tags - Niklas retrieved when tags would not restart-20220801T202245Z-001'
files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

# create files names from directory
new_files <- files %>%
  stringr::str_remove(., path) %>%
  stringr::str_remove(., '.CSV') %>%
  stringr::str_replace_all(., '/', ' ') %>%
  stringr::str_replace(., 'tag', 'Tag ') %>%
  stringr::str_squish() %>%
  stringr::str_replace(., 'DATA_', 'DATA ') %>%
  stringr::str_c('dulogs/2022/', ., '.csv')

# copy files over to dulogs with new name
file.copy(from = files, to = new_files)

# open and add year to files
for(f in new_files) {
  mn <- suppressMessages(readr::read_csv(f))
  mn <- tibble::add_column(mn, tz = 'America/Virgin', .after = 'Meeting Second')
  suppressMessages(readr::write_csv(mn, file = f))
}
