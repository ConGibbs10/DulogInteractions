# read birds from csv
birds <- readr::read_csv('data-raw/birds-raw.csv',
                         col_types = 'cccccccccncccccc?ncncncncnc')

# edits:
## create logical columns out of logical features
## turn date captured into a date object
birds <- dplyr::mutate(
  birds,
  dplyr::across(
    c(
      'main_barn',
      'recap',
      'feathers_sampled',
      'blood_sampled',
      'malaria',
      'lts_intact',
      'rts_intact'
    ),
    ~ dplyr::case_when(.x == 'Y' ~ TRUE, .x == 'N' ~ FALSE, TRUE ~ NA)
  ),
  date_captured = lubridate::mdy(date_captured)
)

# write data
usethis::use_data(birds, overwrite = TRUE)
