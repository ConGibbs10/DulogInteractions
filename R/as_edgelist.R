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
      dplyr::count(name = 'num_interactions') %>%
      dplyr::ungroup()
  }
