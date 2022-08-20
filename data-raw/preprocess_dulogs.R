library(DulogInteractions)

# with time epsilon of 0:
tictoc::tic()
## read in the time epsilon of 0
read_step <- readr::read_rds('logs/read_dulogs/time_eps0.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/time_eps0/')
## run preprocessing
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  only_barn = FALSE,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  log = 'logs/preprocess_dulogs/time_eps0/default'
)
tictoc::toc()

# with time epsilon of 1:
tictoc::tic()
## read in the time epsilon of 1
read_step <- readr::read_rds('logs/read_dulogs/time_eps1.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/time_eps1/')
## run preprocessing
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  only_barn = FALSE,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  log = 'logs/preprocess_dulogs/time_eps1/default'
)
tictoc::toc()

# with time epsilon of 2:
tictoc::tic()
## read in the time epsilon of 2
read_step <- readr::read_rds('logs/read_dulogs/time_eps2.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/time_eps2/')
## run preprocessing
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  only_barn = FALSE,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  log = 'logs/preprocess_dulogs/time_eps2/default'
)
tictoc::toc()

# with time epsilon of 3:
tictoc::tic()
## read in the time epsilon of 3
read_step <- readr::read_rds('logs/read_dulogs/time_eps3.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/time_eps3/')
## run preprocessing
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  only_barn = FALSE,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  log = 'logs/preprocess_dulogs/time_eps3/default'
)
tictoc::toc()

# with time epsilon of 3.99:
tictoc::tic()
## read in the time epsilon of 3.99
read_step <- readr::read_rds('logs/read_dulogs/time_eps3p99.rds')
dulogs <- read_step$return
## create directory within preprocess_dulogs
dir.create('logs/preprocess_dulogs/time_eps3p99/')
## run preprocessing
preprocess_dulogs(
  dulogs,
  min_rssi = -Inf,
  max_rssi = 15,
  only_barn = FALSE,
  start_time = '0000-01-01 UTC',
  end_time = '9999-01-01 UTC',
  log = 'logs/preprocess_dulogs/time_eps3p99/default'
)
tictoc::toc()
