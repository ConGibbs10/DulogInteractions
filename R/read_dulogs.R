#' Read and combine dulog files. See details.
#'
#' @details Combines dulog files from a given year, including those data from the
#' mobile-node (named `mobilenode`) and mobile range (named `inrange`) csv files.
#' The user provides a named character vector specifying the directories, and a
#' named character vector of regular expression patterns for which only matching
#' files will be returned. These files are combined when possible into a data frame
#' with tidy column names and proper data types. The in range data are used as a
#' proxy from whether meetings between actors occured in a barn (where the base
#' stations are located).
#'
#' @param paths A named character vector with two named elements: `mobilenode` and
#' `inrange`, each specifying the directory containing the mobile node and node
#' range data, respectively.
#' @param patterns A named character vector with two named elements: `mobilenode` and
#' `inrange`, each specifying a regular expression. Only file names within the
#' directory which match the regular expression will be returned.
#' @param year A character string, YYYY, indicating the year under study.
#' @param time_eps A numeric for the maximum number of seconds the in range timestamp
#' can deviate from the mobile node timestamp while claiming a bird is in the barn.
#' @param barn_rule Rule for determining whether the meeting took place in barn.
#' Option `and` signifies both birds need be in the barn; whereas option `or`
#' signifies at least one bird needs to be in the barn.
#' @param log File path and name for preprocessing log.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' if(FALSE) {
#' read_dulogs(
#' paths = c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022'),
#' patterns = c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE'),
#' year = 2022,
#' time_eps = 2,
#' barn_rule = 'or'
#' )
#' }
read_dulogs <-
  function(paths,
           patterns,
           year,
           time_eps = 2,
           barn_rule = 'or',
           log = NULL) {
    # check paths
    if (!all(names(paths) %in% c('mobilenode', 'inrange'))) {
      stop('paths must be a named list of file paths. See function documentation.')
    }

    # check patterns
    if (!all(names(patterns) %in% c('mobilenode', 'inrange'))) {
      stop('patterns must be a named list of regular expressions. See function documentation.')
    }

    # check time epsilon
    if (length(time_eps) != 1 || !is.numeric(time_eps) || time_eps < 0) {
      stop('time_eps should be a nonnegative numeric scalar.')
    }

    if (!barn_rule %in% c('or', 'and')) {
      stop("barn_rule must be either 'or' or 'and'.")
    }

    # check year
    # check if year is proper form
    year <- as.character(year)
    if (!stringr::str_detect(year, '^\\d{4}$')) {
      stop('Year (YYYY) must be a constant string.')
    }

    # read mobile data
    mnData <- read_mobilenode(path = paths['mobilenode'], pattern = patterns['mobilenode'], year = year)
    # read in range data
    irData <- read_inrange(path = paths['inrange'], pattern = patterns['inrange'], year = year)

    # check log is path
    if (!is.null(log)) {
      if (!dir.exists(dirname(log))) {
        stop('log directory must be a valid directory.')
      }

      lname <- file.path(dirname(log), paste0(basename(log), '.txt'))
      tname <- file.path(dirname(log), paste0(basename(log), '.rds'))

      # get inputs
      args_nms <- rlang::fn_fmls_names()
      input <- lapply(as.list(args_nms), function(arg) get(arg))
      names(input) <- args_nms

      # save initial dulogs
      inirange <- irData
      ltrace <- list(input = input)
      removals <- list()
      profiles <- list()
      acc <- c(input = nrow(inirange))
    }

    # bind additional data
    firData <- irData %>%
      # append time frame of study
      dplyr::left_join(., timeframe, by = 'year') %>%
      # append the bird id
      dplyr::left_join(.,
                       dplyr::select(birds, year, node = tag, node_id = bird_id),
                       by = c('year', 'node'))
    if (!is.null(log)) {
      acc <- c(acc, bind_pkg_data = nrow(firData))
    }
    irData <- firData

    # meetings must satisfy timeframe: between start date and end date
    firData <- dplyr::filter(irData, range_date >= start, range_date <= end)
    if (!is.null(log)) {
      removals$within_dates <- suppressMessages(dplyr::anti_join(irData, firData))
      acc <- c(acc, within_dates = nrow(firData))
    }
    irData <- firData

    # meetings must satisfy timeframe: between start date and end times each day
    firData <- dplyr::filter(irData, (lubridate::hour(range_date) >= lubridate::hour(start) |
                                        lubridate::hour(range_date) <= lubridate::hour(end)))
    if (!is.null(log)) {
      removals$within_times <- suppressMessages(dplyr::anti_join(irData, firData))
      acc <- c(acc, within_times = nrow(firData))
    }
    irData <- firData

    # meetings must exclude phantom birds
    firData <- dplyr::filter(irData, !is.na(node_id))
    if (!is.null(log)) {
      removals$phantom_actors <- suppressMessages(dplyr::anti_join(irData, firData))
      acc <- c(acc, phantom_actors = nrow(firData))
    }
    irData <- firData

    # if logs are requested, profile phantom nodes
    if (!is.null(log)) {
      dphantom <-
        dplyr::filter(removals$phantom_actors, is.na(node_id))
      if (nrow(dphantom) > 0) {
        # receivers
        profiles$phantom <- tibble::tibble(
          year = dphantom$year[1],
          type = 'node',
          phantom = dphantom$node[is.na(dphantom$node_id)],
          origin = dphantom$origin[is.na(dphantom$node_id)],
          rssi = dphantom$rssi[is.na(dphantom$node_id)]
        ) %>%
          dplyr::group_by(., year, type, phantom) %>%
          dplyr::summarise(
            .,
            n = dplyr::n(),
            origins = list(sort(unique(origin))),
            rssis = list(sort(unique(rssi)))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(., year, type, phantom, n)
      } else {
        profiles$phantom <- tibble::tibble(
          year = dphantom$year[1],
          type = character(),
          phantom = numeric(),
          n = integer(),
          origins = list(),
          rssis = list()
        )
      }
      rm(dphantom)
    }

    # if logs are requested, group all duplicates and near duplicates, specifying
    # each
    if (!is.null(log)) {
      profiles$duplicates <- irData %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(., year, node, range_date) %>%
        dplyr::summarise(., n = dplyr::n(), origins = list(origin), rssis = list(rssi)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(., n > 1) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(., same_rssis = purrr::map_lgl(rssis, function(x) length(unique(x)) == 1)) %>%
        dplyr::arrange(., node, range_date)
    }

    # keep only unique meetings, meaning (1) the meeting time is the same to the
    # second, (2) same node, and (3) the registered signal strength is the same
    firData <- dplyr::distinct(irData, range_date, node, rssi, .keep_all = TRUE)
    if (!is.null(log)) {
      removals$duplicates <- suppressMessages(dplyr::anti_join(irData, firData))
      acc <- c(acc, duplicates = nrow(firData))
    }
    irData <- firData

    # keep only nearly unique meetings, meaning (1) the range time is the same to the
    # second, (2) node is the same, BUT (3) the registered signal strengths are
    # different. In this case, the weakest signal strength is retained.
    firData <- irData %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(., range_date, node) %>%
      dplyr::slice_min(., rssi) %>%
      # keep only one row in case of ties
      dplyr::slice(., 1) %>%
      dplyr::ungroup() %>%
      tibble::as_tibble()
    if (!is.null(log)) {
      removals$near_duplicates <- suppressMessages(dplyr::anti_join(irData, firData))
      acc <- c(acc, near_duplicates = nrow(firData))
    }
    irData <- firData

    #---------------------------------------------------------------------------
    # check if meeting occurred in barn
    #---------------------------------------------------------------------------
    mnData <-
      classify_barn_meetings(mnData, irData, time_eps = time_eps, barn_rule = barn_rule)

    # if logs are requested, profile barn
    if (!is.null(log)) {
      profiles$barn$rxin_txin <- dplyr::filter(mnData, rx_barn, tx_barn)
      profiles$barn$rxout_txout <- dplyr::filter(mnData, !rx_barn, !tx_barn)
      profiles$barn$rxin_txout <- dplyr::filter(mnData, rx_barn, !tx_barn)
      profiles$barn$rxout_txin <- dplyr::filter(mnData, !rx_barn, tx_barn)
    }

    # write logs if necessary
    if (!is.null(log)) {
      message('Writing the preprocessing log can be time consuming. Patience is a virtue.')

      #---------------------------------------------------------------------------
      # trace items
      #---------------------------------------------------------------------------
      ltrace$output <- irData
      ltrace$return <- mnData
      # sort rows of removal and add to trace
      removals <-
        purrr::map_if(removals,
                      function(x) nrow(x) > 0,
                      function(x) dplyr::arrange(x, node, range_date),
                      .else = function(x) x)
      ltrace$removals <- removals
      # add profiles to trace
      ltrace$profiles <- profiles
      # add sankey flow
      acc <- c(acc, output = nrow(irData))
      nrm <- acc - dplyr::lag(acc)
      nrm[1] <- 0
      ltrace$sankey_flow <-
        tibble::tibble(
          description = names(acc),
          n_meetings = unname(acc),
          n_removed = abs(nrm)
        )
      # write trace
      readr::write_rds(ltrace, file = tname, compress = 'gz')

      #---------------------------------------------------------------------------
      # text items
      #---------------------------------------------------------------------------
      nrm <- nrow(inirange)-nrow(ltrace$output)
      ph <- sort(unique(ltrace$profiles$phantom$phantom))
      rxout <- table(profiles$barn$rxout_txin$rx_node)
      rxout <- rxout[order(rxout, decreasing = TRUE)]
      txout <- table(profiles$barn$rxin_txout$tx_node)
      txout <- txout[order(txout, decreasing = TRUE)]
      inbarn <- table(c(profiles$barn$rxin_txin$rx_node, profiles$barn$rxin_txin$tx_node, profiles$barn$rxin_txout$rx_node, profiles$barn$rxout_txin$tx_node))
      inbarn <- inbarn[order(inbarn, decreasing = TRUE)]
      outbarn <- table(c(profiles$barn$rxout_txout$rx_node, profiles$barn$rxout_txout$tx_node, profiles$barn$rxin_txout$tx_node, profiles$barn$rxout_txin$rx_node))
      outbarn <- outbarn[order(outbarn, decreasing = TRUE)]
      cat('Description of Preprocessing In Range Data Outlined by', tname, ':\n\n',
          '----Number of Range Meetings (see sankey_flow and removals for details)----\n',
          '  Input:', nrow(inirange), '\n',
          '  Removed:', nrm, paste0('(',formatC(100*(nrm/nrow(inirange)), format = 'f', digits = 2), '%)'), '\n',
          '  Output:', nrow(ltrace$output), '\n\n',
          '----Phantom Nodes (see profiles$phantom for details)----\n',
          '  Total:', length(ph), '\n',
          '  Phantom nodes:', paste(ph, collapse = ', '), '\n',
          '----Duplicate Meetings (see profiles$duplicates for details)----\n',
          '  See profiles$duplicates for details on origin files leading to duplicates and duplicate signal strengths\n',
          '  Total duplicates (i.e. same node, range time, and signal strength):',
          sum(ltrace$profiles$duplicates$same_rssis), '\n',
          '  Total near duplicates (i.e. same node, range time, but different signal strength):',
          sum(!ltrace$profiles$duplicates$same_rssis), '\n',
          'Near duplicates are defined AFTER removing all duplicates\n\n',
          '----Meeting in Barn (see profiles$duplicates for details)----\n',
          '  See profiles$barn for details on which meetings have both, neither, or only one bird in the barn.\n',
          '  Rx and tx in barn:', nrow(profiles$barn$rxin_txin), paste0('(',formatC(100*(nrow(profiles$barn$rxin_txin)/nrow(mnData)), format = 'f', digits = 2), '%)'), '\n',
          '  Rx and tx out of barn:', nrow(profiles$barn$rxout_txout), paste0('(',formatC(100*(nrow(profiles$barn$rxout_txout)/nrow(mnData)), format = 'f', digits = 2), '%)'), '\n',
          '  Rx in and tx out of barn:', nrow(profiles$barn$rxin_txout), paste0('(',formatC(100*(nrow(profiles$barn$rxin_txout)/nrow(mnData)), format = 'f', digits = 2), '%)'), '\n',
          '  Rx out of and tx in barn:', nrow(profiles$barn$rxout_txin), paste0('(',formatC(100*(nrow(profiles$barn$rxout_txin)/nrow(mnData)), format = 'f', digits = 2), '%)'), '\n',
          '  Frequency of rx out of and tx in barn:', paste0(purrr::map2_chr(names(rxout), rxout, ~paste0(.x, ': ', .y)), collapse = ', '), '\n',
          '  Frequency of tx out of and rx in barn:', paste0(purrr::map2_chr(names(txout), txout, ~paste0(.x, ': ', .y)), collapse = ', '), '\n',
          '  Frequency in barn:', paste0(purrr::map2_chr(names(inbarn), inbarn, ~paste0(.x, ': ', .y)), collapse = ', '), '\n',
          '  Frequency out of barn:', paste0(purrr::map2_chr(names(outbarn), outbarn, ~paste0(.x, ': ', .y)), collapse = ', '),
          file = lname
      )
    }

    return(mnData)
  }
