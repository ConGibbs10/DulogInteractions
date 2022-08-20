# read birds from csv
birds <- readr::read_csv('data-raw/birds-raw.csv',
                         col_types = 'ccicccc')
# write data
usethis::use_data(birds, overwrite = TRUE)
