#' Make a sorted frequency table for a factor
#'
#' @param x factor
#'
#' @return A tibble
#' @export
#' @examples
#' fcount(iris$Species)
fcount <- function(x) {
  new_x = forcats::fct_count(x, sort = TRUE)
  new_x = dplyr::rename(.data =new_x, variable = f, count = n)

  return(new_x)
}
fcount(c("a", "a", "b"))
