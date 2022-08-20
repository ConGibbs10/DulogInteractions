#' Swap values in two columns based on the order of values in another two columns of a data frame.
#'
#' @param df Dataframe containing columns to compare and swap.
#' @param to_compare A vector of size two of column names. If the value of the first column is larger than the value of the second column, then the values in the columns `to_swap` are swapped.
#' @param to_swap A vector of size two of column names. If the value of the first column of `to_compare` is larger than the value of the second column of `to_compare`, then the values in the columns `to_swap` are swapped.
#' @param mark_swap Logical indicating whether or not to mark rows whose columns have been swapped.
#'
#' @keywords internal
#'
#' @return Dataframe with the proper ordering of columns.
#' @export
#'
#' @examples
#' if(FALSE) {
#' df <- data.frame(a = c(1, 8, 3), b = c(1, 2, 27))
#' swap_if(df, to_compare = c('a', 'b'), to_swap = c('a', 'b'))
#' }
swap_if <- function(df, to_compare, to_swap, mark_swap = FALSE) {
  # checks
  if (length(to_compare) != 2 || length(to_swap) != 2 ||
      !all(to_compare %in% names(df)) || !all(to_compare %in% names(df))) {
    stop('Arguments to_compare and to_swap should be character vectors of size two which represent column names.')
  }
  if (length(mark_swap) != 1 || !is.logical(mark_swap)) {
    stop('Argument mark_swap should be a logical of size 1.')
  }
  x1 <- df[, to_compare[1]]
  x2 <- df[, to_compare[2]]
  should_swap <- ifelse(x1 <= x2, 0, 1)
  df[should_swap == 1, to_swap] <-
    df[should_swap == 1, rev(to_swap)]
  if(mark_swap) {
    df$swapped <- as.logical(as.vector(should_swap))
  }

  return(df)
}
