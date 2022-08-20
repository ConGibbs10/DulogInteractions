library(DulogInteractions)

# paths and patterns
paths <- c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022')
patterns <- c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE')

# with time epsilon of 0: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 0,
  barn_rule = 'or',
  log = 'logs/read_dulogs/time_eps0'
)
tictoc::toc()

# with time epsilon of 1: 62 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 1,
  barn_rule = 'or',
  log = 'logs/read_dulogs/time_eps1'
)
tictoc::toc()

# with time epsilon of 2: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 2,
  barn_rule = 'or',
  log = 'logs/read_dulogs/time_eps2'
)
tictoc::toc()

# with time epsilon of 3: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 3,
  barn_rule = 'or',
  log = 'logs/read_dulogs/time_eps3'
)
tictoc::toc()
