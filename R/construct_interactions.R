#' Construct data frame of interactions.
#'
#' @param pdulogs Data frame of pre-processed dulog data.
#' @param time_gap A numeric for the maximum number of seconds between meetings
#' before a new interaction is defined.
#' @param directed Logical indicating whether to treat as directed.
#' @param loops Logical indicating whether to include self loops.
#'
#' @return A data frame.
#' @export
construct_interactions <-
  function(pdulogs,
           time_gap = 8,
           directed = FALSE,
           loops = FALSE,
           log = NULL) {
    # check time gap
    if (length(time_gap) > 1 ||
        !is.numeric(time_gap) || time_gap < 0) {
      stop('Time gap must be a non-negative numeric scalar.')
    }

    # check log is path
    if (!is.null(log)) {
      if (!dir.exists(dirname(log))) {
        stop('log directory must be a valid directory.')
      }

      lname <-
        file.path(dirname(log), paste0(basename(log), '.txt'))
      tname <-
        file.path(dirname(log), paste0(basename(log), '.rds'))

      # get inputs
      args_nms <- rlang::fn_fmls_names()
      input <- lapply(as.list(args_nms), function(arg)
        get(arg))
      names(input) <- args_nms

      # save initial dulogs
      ltrace <- list(input = input)
    }

    # remove any instances when a bird registered herself
    if (!loops) {
      pdulogs <- dplyr::filter(pdulogs, rx_node != tx_node)
    }

    # check if the graph is directed
    if (directed) {
      gdf <- dplyr::select(pdulogs,
                           year,
                           meeting_date,
                           from_node = tx_node,
                           to_node = rx_node,
                           rssi,
                           barn)
    } else {
      # how do we want to deal with difference in RSSI when a logs b and b logs a?

      # keep the smallest?
      # gdf <- pdulogs %>%
      #   dplyr::select(.,
      #                 year,
      #                 meeting_date,
      #                 from_node = i_node,
      #                 to_node = j_node,
      #                 rssi,
      #                 barn) %>%
      #   dtplyr::lazy_dt() %>%
      #   dplyr::group_by(., year, meeting_date, from_node, to_node) %>%
      #   dplyr::mutate(., barn = any(barn)) %>%
      #   dplyr::slice_min(., rssi) %>%
      #   # keep only one row in case of ties
      #   dplyr::slice(., 1) %>%
      #   dplyr::ungroup() %>%
      #   tibble::as_tibble()

      # keep all of them?
      gdf <- dplyr::select(pdulogs,
                           year,
                           meeting_date,
                           from_node = i_node,
                           to_node = j_node,
                           rssi,
                           barn)
    }

    # get interaction IDs
    gdf <- gdf %>%
      dplyr::arrange(., as.numeric(from_node), as.numeric(to_node), meeting_date) %>%
      dplyr::group_split(., from_node, to_node) %>%
      # define interaction for each from to pairing
      purrr::map_dfr(., function(pair) {
        if (nrow(pair) > 1) {
          tdiff <-
            as.double(pair$meeting_date - dplyr::lead(pair$meeting_date, n = 1),
                      units = 'secs')
          intg <- cumsum(tdiff <= -time_gap)
          intg <- c(NA, intg[-length(intg)])
          # change the first entry accordingly
          tdiff12 <-
            as.double(pair$meeting_date[1] - pair$meeting_date[2], units = 'secs')
          if (tdiff12 <= -time_gap) {
            intg[1] <- 0
            intg <- intg + 1
          } else {
            intg[1] <- 1
          }
          # add interaction indicator
          pair$interaction_group <- intg
          pair
        } else {
          tibble::add_column(pair, interaction_group = 1)
        }
      })

    # apply summarization
    interactions <- gdf %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(., year, from_node, to_node, interaction_group) %>%
      dplyr::summarize(
        .,
        start_time = min(meeting_date),
        end_time = max(meeting_date),
        num_meetings = dplyr::n(),
        proportion_barn = mean(barn),
        dplyr::across(
          .cols = rssi,
          .fns = list(
            min = function(x)
              min(x, na.rm = TRUE),
            med = function(x)
              median(x, na.rm = TRUE),
            mean = function(x)
              mean(x, na.rm = TRUE),
            max = function(x)
              max(x, na.rm = TRUE)
          )
        )
      ) %>%
      dplyr::mutate(
        .,
        meetings_per_sec = ifelse(
          num_meetings > 1,
          num_meetings / as.double(end_time - start_time, units = 'secs'),
          0
        ),
        secs_elapsed = as.double(end_time - start_time, units = 'secs')
      ) %>%
      dplyr::select(
        .,
        year,
        from_node,
        to_node,
        interaction_group,
        start_time,
        end_time,
        secs_elapsed,
        num_meetings,
        meetings_per_sec,
        proportion_barn,
        dplyr::everything()
      ) %>%
      dplyr::as_tibble()

    if (!is.null(log)) {
      message('Writing the preprocessing log can be time consuming. Patience is a virtue.')
      ltrace$output <- interactions
      readr::write_rds(ltrace, file = tname, compress = 'gz')
    }

    return(interactions)
  }
