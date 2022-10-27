# read birds from csv
birds <- readr::read_csv('data-raw/birds-raw.csv',
                          col_types = 'cccccccccncccccc?ncncncncncnnnnnnnnnnnn')

# vector of column names in desired order
col_order <- c(
  "year",
  "band_id",
  "first_nest",
  "second_nest",
  "tag",
  "sex",
  "recap",
  "first_banded",
  "age_cat",
  "age",
  "main_barn",
  "pair_tag",
  "pair_id",
  "MB_side",
  "top_color",
  "bottom_color",
  "date_captured",
  "avg_rwl",
  "lts_intact",
  "avg_lts",
  "rts_intact",
  "avg_rts",
  "feathers_sampled",
  "mass",
  "blood_sampled",
  "feather_mites",
  "malaria",
  "B_avg.bright",
  "B_hue",
  "B_chroma",
  "R_avg.bright",
  "R_hue",
  "R_chroma",
  "T_avg.bright",
  "T_hue",
  "T_chroma",
  "V_avg.bright",
  "V_hue",
  "V_chroma"
)

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

# order columns as desired
birds <- dplyr::select(birds, dplyr::all_of(col_order), dplyr::everything())

# write data
usethis::use_data(birds, overwrite = TRUE)
