#' Restructure interactions as an edgelist
#'
#' @details Interactions are filtered according to the seconds elapsed in the
#' interaction and proportion of meetings taking place in the barn. Once a subset
#' of the interactions of interest has been obtained, the resulting data frame is
#' summarized into an edgelist by counting the number of resulting interactions
#' between each pair of actors.
#'
#' @param interactions Data frame of interactions between actors.
#' @param min_secs_elapsed Minimum number of seconds elapsed for an interaction to count.
#' @param max_secs_elapsed Maximum number of seconds elapsed for an interaction to count.
#' @param min_proportion_barn Minimum proportion of meetings taking place in the barn for an interaction to count.
#' @param max_proportion_barn Maximum proportion of meetings taking place in the barn for an interaction to count.
#'
#' @return A data frame.
#' @export
as_edgelist <-
  function(interactions,
           min_secs_elapsed = 4,
           max_secs_elapsed = Inf,
           min_proportion_barn = 0,
           max_proportion_barn = 1) {
    interactions %>%
      dplyr::filter(
        .,
        secs_elapsed >= min_secs_elapsed,
        secs_elapsed <= max_secs_elapsed,
        proportion_barn >= min_proportion_barn,
        proportion_barn <= max_proportion_barn
      ) %>%
      dplyr::group_by(., year, from_node, to_node) %>%
      dplyr::summarize(num_interactions = dplyr::n(),
                       secs_interactions = sum(secs_elapsed)) %>%
      dplyr::ungroup()
  }
