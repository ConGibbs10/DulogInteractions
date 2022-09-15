library(DulogInteractions)

# paths and patterns
paths <- c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022')
patterns <- c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE')
dir.create('logs/read_dulogs')

#-------------------------------------------------------------------------------
# OR
#-------------------------------------------------------------------------------
# with time epsilon of 0: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 0,
  barn_rule = 'or',
  log = 'logs/read_dulogs/timeeps0_barnruleor'
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
  log = 'logs/read_dulogs/timeeps1_barnruleor'
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
  log = 'logs/read_dulogs/timeeps2_barnruleor'
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
  log = 'logs/read_dulogs/timeeps3_barnruleor'
)
tictoc::toc()

#-------------------------------------------------------------------------------
# AND
#-------------------------------------------------------------------------------
# with time epsilon of 0: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 0,
  barn_rule = 'and',
  log = 'logs/read_dulogs/timeeps0_barnruleand'
)
tictoc::toc()

# with time epsilon of 1: 62 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 1,
  barn_rule = 'and',
  log = 'logs/read_dulogs/timeeps1_barnruleand'
)
tictoc::toc()

# with time epsilon of 2: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 2,
  barn_rule = 'and',
  log = 'logs/read_dulogs/timeeps2_barnruleand'
)
tictoc::toc()

# with time epsilon of 3: 63 seconds
tictoc::tic()
read_dulogs(
  paths = paths,
  patterns = patterns,
  year = 2022,
  time_eps = 3,
  barn_rule = 'and',
  log = 'logs/read_dulogs/timeeps3_barnruleand'
)
tictoc::toc()
