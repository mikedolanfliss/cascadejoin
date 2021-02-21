#' Bind two factors Wurds
#'
#' Create a new single factor from two other factors without dumbly converting
#' them to integers
#'
#' @param a factor
#' @param b factor
#'
#' @return factor
#' @export
#'
#' @examples
#' fbind(iris$Species[c(1,51,101)], PlantGrowth$group[c(1,11,21)])
#'
fbind = function(a, b) {
  factor(c(as.character(a), as.character(b)))
}

# https://happygitwithr.com/rstudio-git-github.html
# https://r-pkgs.org/whole-game.html
# https://r-pkgs.org/package-within.html
