library(DulogInteractions)

################################################################################
# timeeps2_barnruleor
################################################################################

## read in the time epsilon of 2 and barn rule of or
read_step <- readr::read_rds('logs/read_dulogs/timeeps2_barnruleor.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/timeeps2_barnruleor/', recursive = TRUE)

#-------------------------------------------------------------------------------
# only barn is FALSE
#-------------------------------------------------------------------------------

tictoc::tic()
## run preprocessing with time epsilon of 0 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 0,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps0_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 1 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 1,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps1_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 2 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 2,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps2_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 3 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 3,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps3_onlybarnF'
)
tictoc::toc()

#-------------------------------------------------------------------------------
# only barn is TRUE
#-------------------------------------------------------------------------------

tictoc::tic()
## run preprocessing with time epsilon of 0 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 0,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps0_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 1 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 1,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps1_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 2 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 2,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps2_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 3 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 3,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleor/timeeps3_onlybarnT'
)
tictoc::toc()

################################################################################
# timeeps2_barnruleand
################################################################################

## read in the time epsilon of 2 and barn rule of or
read_step <- readr::read_rds('logs/read_dulogs/timeeps2_barnruleand.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/timeeps2_barnruleand/', recursive = TRUE)

#-------------------------------------------------------------------------------
# only barn is FALSE
#-------------------------------------------------------------------------------

tictoc::tic()
## run preprocessing with time epsilon of 0 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 0,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps0_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 1 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 1,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps1_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 2 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 2,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps2_onlybarnF'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 3 and only_barn = FALSE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = FALSE,
  time_eps = 3,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps3_onlybarnF'
)
tictoc::toc()

#-------------------------------------------------------------------------------
# only barn is TRUE
#-------------------------------------------------------------------------------

tictoc::tic()
## run preprocessing with time epsilon of 0 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 0,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps0_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 1 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 1,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps1_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 2 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 2,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps2_onlybarnT'
)
tictoc::toc()

tictoc::tic()
## run preprocessing with time epsilon of 3 and only_barn = TRUE
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  only_barn = TRUE,
  time_eps = 3,
  log = 'logs/preprocess_dulogs/timeeps2_barnruleand/timeeps3_onlybarnT'
)
tictoc::toc()
