#' Preprocess dulogs. See details.
#'
#' @details Meetings must satisfy the time frame of study including start and end dates
#' and daily monitoring times. These yearly constraints are provided in `data-raw/timeframe.R`.
#' Only birds under study are considered; these birds are documented in `data-raw/birds.R`.
#' Any meeting involving a phantom tag is subsequently removed. Proximity of birds
#' is proxied by signal strength. Some meetings are perfect duplicates (i.e.
#' having the same sender, receiver, meeting time, and signal strength) and are
#' subsequently removed. Other meetings are near duplicates (i.e. having the same
#' sender, receiver, and meeting time). In this case, the log registering the weakest
#' signal strength is preserved. Meetings with a signal strength weaker than the
#' provided upperbound are removed. If requested, only meetings in the barn are
#' considered, and finally, all meetings must take place before the start and end
#' times specified.
#'
#' @param dulogs Data frame of dulog data.
#' @param min_rssi Minimum RSSI to consider.
#' @param max_rssi Maximum RSSI to consider.
#' @param start_time Earliest date and time to consider.
#' @param end_time Latest date and time to consider.
#' @param only_barn Logical indicating whether to keep only meetings in the barn.
#' @param time_eps A numeric for the maximum number of seconds the timestamp's can
#' deviate while classifying a meeting as reciprocal (see `classify_reciprocal_meetings`).
#' @param log File path and name for preprocessing log.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' if(FALSE) {
#' dulogs <- read_dulogs(
#' paths = c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022'),
#' patterns = c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE'),
#' year = 2022,
#' time_eps = 2,
#' barn_rule = 'or'
#' )
#' preprocess_dulogs(
#' dulogs,
#' min_rssi = -Inf,
#' max_rssi = 15,
#' end_time = '0000-01-01 UTC',
#' start_time = '9999-01-01 UTC',
#' only_barn = FALSE
#' )
#' }
preprocess_dulogs <-
  function(dulogs,
           min_rssi = -Inf,
           max_rssi = 15,
           start_time = lubridate::make_datetime(year = 0000),
           end_time = lubridate::make_datetime(year = 9999),
           only_barn = FALSE,
           time_eps = 2,
           log = NULL) {
    # check log is path
    if (!is.null(log)) {
      if (!dir.exists(dirname(log))) {
        stop('log directory must be a valid directory.')
      }

      lname <- file.path(dirname(log), paste0(basename(log), '.txt'))
      tname <- file.path(dirname(log), paste0(basename(log), '.rds'))

      # get inputs
      args_nms <- rlang::fn_fmls_names()
      input <- lapply(as.list(args_nms), function(arg)
        get(arg))
      names(input) <- args_nms

      # save initial dulogs
      inidulogs <- dulogs
      ltrace <- list(input = input)
      removals <- list()
      profiles <- list()
      acc <- c(input = nrow(inidulogs))
    }

    # check start and end time arguments
    bot <- lubridate::make_datetime(year = 0000)
    eot <- lubridate::make_datetime(year = 9999)
    if (!identical(start_time, bot)) {
      start_time <- lubridate::as_datetime(start_time)
      if (is.na(start_time)) {
        stop('Argument start_time is not coercible to an object of class POSIXt of length 1.\n Use lubridate::make_datetime() to set start_time.')
      }
      start_time <- lubridate::with_tz(start_time, tzone = 'UTC')
    }

    if(!identical(end_time, eot)) {
      end_time <- lubridate::as_datetime(end_time)
      if(is.na(end_time)) {
        stop('Argument end_time is not coercible to an object of class POSIXt of length 1.\n Use lubridate::make_datetime() to set end_time.')
      }
      end_time <- lubridate::with_tz(end_time, tzone = 'UTC')
    }

    # bind additional data
    fdulogs <- dulogs %>%
      # append time frame of study
      dplyr::left_join(., timeframe, by = 'year') %>%
      # append the bird id
      dplyr::left_join(.,
                       dplyr::select(birds, year, rx_node = tag, rx_id = bird_id),
                       by = c('year', 'rx_node')) %>%
      dplyr::left_join(.,
                       dplyr::select(birds, year, tx_node = tag, tx_id = bird_id),
                       by = c('year', 'tx_node'))
    if (!is.null(log)) {
      acc <- c(acc, bind_pkg_data = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must satisfy timeframe: between start date and end date
    fdulogs <-
      dplyr::filter(dulogs, meeting_date >= start, meeting_date <= end)
    if (!is.null(log)) {
      removals$within_dates <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, within_dates = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must satisfy timeframe: between start date and end times each day
    fdulogs <-
      dplyr::filter(
        dulogs,
        (
          lubridate::hour(meeting_date) >= lubridate::hour(start) |
            lubridate::hour(meeting_date) <= lubridate::hour(end)
        )
      )
    if (!is.null(log)) {
      removals$within_times <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, within_times = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must exclude phantom birds
    fdulogs <- dplyr::filter(dulogs,!is.na(rx_id),!is.na(tx_id))
    if (!is.null(log)) {
      removals$phantom_actors <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, phantom_actors = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # if logs are requested, profile phantom nodes
    if (!is.null(log)) {
      dphantom <-
        dplyr::filter(removals$phantom_actors, is.na(rx_id) |
                        is.na(tx_id))
      if (nrow(dphantom) > 0) {
        # receivers
        profiles$phantom <- tibble::tibble(
          year = dphantom$year[1],
          type = 'rx',
          phantom = dphantom$rx_node[is.na(dphantom$rx_id)],
          origin = dphantom$origin[is.na(dphantom$rx_id)],
          rssi = dphantom$rssi[is.na(dphantom$rx_id)],
        ) %>%
          # senders
          tibble::add_row(
            .,
            year = dphantom$year[1],
            type = 'tx',
            phantom = dphantom$tx_node[is.na(dphantom$tx_id)],
            origin = dphantom$origin[is.na(dphantom$tx_id)],
            rssi = dphantom$rssi[is.na(dphantom$tx_id)]
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
      profiles$duplicates <- dulogs %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(., year, rx_node, tx_node, meeting_date) %>%
        dplyr::summarise(
          .,
          n = dplyr::n(),
          origins = list(origin),
          rssis = list(rssi)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(., n > 1) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(., same_rssis = purrr::map_lgl(rssis, function(x)
          length(unique(x)) == 1)) %>%
        dplyr::arrange(., rx_node, tx_node, meeting_date)
    }

    # keep only unique meetings, meaning (1) the meeting time is the same to the
    # second, (2) the sender and receiver are the same, and (3) the registered signal
    # strength is the same
    fdulogs <-
      dplyr::distinct(dulogs, meeting_date, rx_node, tx_node, rssi, .keep_all = TRUE)
    if (!is.null(log)) {
      removals$duplicates <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, duplicates = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # keep only nearly unique meetings, meaning (1) the meeting time is the same to the
    # second, (2) the sender and receiver are the same, BUT (3) the registered signal
    # strengths are different. In this case, the weakest signal strength is retained.
    fdulogs <- dulogs %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(., meeting_date, rx_node, tx_node) %>%
      dplyr::slice_min(., rssi) %>%
      # keep only one row in case of ties
      dplyr::slice(., 1) %>%
      dplyr::ungroup() %>%
      tibble::as_tibble()
    if (!is.null(log)) {
      removals$near_duplicates <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, near_duplicates = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must have signal strength better than the maximum specified
    fdulogs <-
      dplyr::filter(dulogs, rssi <= max_rssi, rssi >= min_rssi)
    if (!is.null(log)) {
      removals$rssi_range <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, rssi_range = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must be in the barn if requested
    if (only_barn) {
      fdulogs <- dplyr::filter(dulogs, barn)
    } else {
      fdulogs <- dulogs
    }
    if (!is.null(log)) {
      removals$only_barn <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, only_barn = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # meetings must be within the time constraints
    fdulogs <-
      dplyr::filter(dulogs, meeting_date <= end_time, meeting_date >= start_time)
    if (!is.null(log)) {
      removals$startend_time <-
        suppressMessages(dplyr::anti_join(dulogs, fdulogs))
      acc <- c(acc, startend_time_range = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # sort final result and prepare data for graph construction
    fdulogs <- dulogs %>%
      dplyr::mutate(., dplyr::across(c('rx_node', 'tx_node'), as.numeric)) %>%
      dplyr::arrange(., rx_node, tx_node, meeting_date) %>%
      # swap rx and tx nodes by tag to get general adjacencies
      swap_if(
        .,
        to_compare = c('rx_node', 'tx_node'),
        to_swap = c('rx_node', 'tx_node'),
        mark_swap = TRUE
      ) %>%
      # after swapping, receiver and sender lose meaning, call them a general i and j
      dplyr::rename(., i_node = rx_node, j_node = tx_node) %>%
      # reassign receiver and treatment as it was originally based on whether a swap occured;
      ## originally rx was i, so if it was swapped rx is j, otherwise i
      ## originally tx was j, so if it was swapped tx is i, otherwise j
      ## rx_id and tx_id never swapped. If tags were swapped, then i is now tx, otherwise i is rx.
      ## rx_id and tx_id never swapped. If tags were swapped, then j is now rx, otherwise j is tx.
      dplyr::mutate(
        .,
        rx_node = ifelse(swapped, j_node, i_node),
        tx_node = ifelse(swapped, i_node, j_node),
        i_id = ifelse(swapped, tx_id, rx_id),
        j_id = ifelse(swapped, rx_id, tx_id),
        i_barn = ifelse(swapped, tx_barn, rx_barn),
        j_barn = ifelse(swapped, rx_barn, tx_barn),
        i_range_date = lubridate::as_datetime(ifelse(swapped, tx_range_date, rx_range_date)),
        j_range_date = lubridate::as_datetime(ifelse(swapped, rx_range_date, tx_range_date)),
        dplyr::across(c(
          'rx_node', 'tx_node', 'i_node', 'j_node'
        ), as.character)
      ) %>%
      dplyr::select(., dplyr::all_of(
        c(
          'year',
          'origin',
          'meeting_date',
          'rx_node',
          'tx_node',
          'rx_id',
          'tx_id',
          'rx_barn',
          'tx_barn',
          'rx_range_date',
          'tx_range_date',
          'rssi',
          'barn',
          'i_node',
          'j_node',
          'i_id',
          'j_id',
          'i_barn',
          'j_barn',
          'i_range_date',
          'j_range_date'
        )
      ),
      dplyr::everything(),
      -start,
      -end)
    if (!is.null(log)) {
      acc <- c(acc, organize_rows_cols = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # check reciprocated meetings
    fdulogs <- classify_reciprocal_meetings(dulogs, time_eps = time_eps)
    if (!is.null(log)) {
      acc <- c(acc, classify_reciprocal_meetings = nrow(fdulogs))
    }
    dulogs <- fdulogs

    # write logs if necessary
    if (!is.null(log)) {
      message('Writing the preprocessing log can be time consuming. Patience is a virtue.')

      #---------------------------------------------------------------------------
      # trace items
      #---------------------------------------------------------------------------
      ltrace$output <- dulogs
      # sort rows of removal and add to trace
      removals <-
        purrr::map_if(removals,
                      function(x)
                        nrow(x) > 0,
                      function(x)
                        dplyr::arrange(x, rx_node, tx_node, meeting_date),
                      .else = function(x)
                        x)
      ltrace$removals <- removals
      # add profiles to trace
      ltrace$profiles <- profiles
      # add sankey flow
      acc <- c(acc, output = nrow(dulogs))
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
      nrm <- nrow(inidulogs) - nrow(ltrace$output)
      ph <- sort(unique(ltrace$profiles$phantom$phantom))
      phrx <-
        sort(unique(ltrace$profiles$phantom$phantom[ltrace$profiles$phantom$type == 'rx']))
      phtx <-
        sort(unique(ltrace$profiles$phantom$phantom[ltrace$profiles$phantom$type == 'tx']))
      cat(
        'Description of Preprocessing Mobile Node Data Outlined by',
        tname,
        ':\n\n',
        '----Number of Meetings (see sankey_flow and removals for details)----\n',
        '  Input:',
        nrow(inidulogs),
        '\n',
        '  Removed:',
        nrm,
        paste0('(', formatC(
          100 * (nrm / nrow(inidulogs)), format = 'f', digits = 2
        ), '%)'),
        '\n',
        '  Output:',
        nrow(ltrace$output),
        '\n\n',
        '----Phantom Nodes (see profiles$phantom for details)----\n',
        '  Total:',
        length(ph),
        '\n',
        '  Phantom nodes:',
        paste(ph, collapse = ', '),
        '\n',
        '  Phantom receivers (rx):',
        paste(phrx, collapse = ', '),
        '\n',
        '  Phantom senders (tx):',
        paste(phtx, collapse = ', '),
        '\n\n',
        '----Duplicate Meetings (see profiles$duplicates for details)----\n',
        '  See profiles$duplicates for details on origin files leading to duplicates and duplicate signal strengths\n',
        '  Total duplicates (i.e. same sender, receiver, meeting time, and signal strength):',
        sum(ltrace$profiles$duplicates$same_rssis),
        '\n',
        '  Total near duplicates (i.e. same sender, receiver, meeting time, but different signal strength):',
        sum(!ltrace$profiles$duplicates$same_rssis),
        '\n',
        'Near duplicates are defined AFTER removing all duplicates\n\n',
        '----Graph Considerations----\n',
        '  Loops (i.e. sender and receiver are the same actor):',
        sum(dulogs$rx_node == dulogs$tx_node),
        '\n',
        '  Reciprocated meetings, excluding loops (i.e. the sender received the receiver signal within time_eps of the receiver receiving the sender signal, making a bidirectional edge):',
        sum(dulogs$reciprocated),
        '\n',
        '  Unreciprocated meetings, including loops (i.e. sender logged receiver but receiver did not log sender at the same time, making unidirectional edge):',
        sum(!dulogs$reciprocated),
        file = lname
      )
    }

    return(dulogs)
  }
