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

#' Match id variables in a tidyselect pipeline
#'
#' @param match_regex An optional, alternate id variable regular expression to match on
#'
#' @returns A matches() function in tidyselect format
#' @export
#'
#' @examples
#' this_tbl |> select(match_id_vars()) |> ...
match_id_vars = function(match_regex = "^id_"){
  matches(match_regex)
}

#' Match linkage variables in a tidyselect pipeline
#'
#' @param match_regex An optional, alternate linkage variable regular expression to match on
#'
#' @returns A matches() function in tidyselect format
#' @export
#'
#' @examples
#' this_tbl |> select(match_linkage_vars()) |> ...
match_linkage_vars = function(match_regex = "^l_"){
  matches(match_regex)
}

#' Select linkage variables by name in a table
#'
#' @param this_tbl A tibble
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' this_tbl |> select(match_linkage_vars()) |> ...
select_linkage_vars = function(this_tbl){
  this_tbl |> select(match_linkage_vars())
}


#' Get shared linkage variables as a list
#'
#' @param tbl1 Table 1
#' @param tbl2 Table 2
#'
#' @returns an intersection list of their common named linkage variables
#' @export
#'
#' @examples
#' get_shared_linkage_vars(this_tbl, that_tbl)
get_shared_linkage_vars = function(tbl1, tbl2){
  intersect(tbl1 |> select_linkage_vars() |> names(),
            tbl2 |> select_linkage_vars() |> names())
}


#' Create a linkage variable comparison table
#'
#' @param tbl1 First table
#' @param tbl2 Second table
#' @param tbl_names A vector of table names for convenience
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' linkage_var_compare_tbl(this_tbl, that_tbl)
linkage_var_compare_tbl = function(tbl1, tbl2, tbl_names = c("tbl1", "tbl2")){
  # TODO optional additional variable vector
  tbl_to_return = tibble(var = union(tbl1 |> select_linkage_vars() |> names(),
                                     tbl2 |> select_linkage_vars() |> names()),
                         tbl1 = var %in% (tbl1 |> select_linkage_vars() |> names()),
                         tbl2 = var %in% (tbl2 |> select_linkage_vars() |> names())) |>
    arrange(var)
  names(tbl_to_return) = c("var", tbl_names)
  return(tbl_to_return)
}

#' Tag / rename linkage variables en masse
#'
#' @param this_tbl Table to tag linkage variables in
#' @param tag The tag to add as suffix
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' this_tbl = this_tbl |> tag_link_vars("EMS")
tag_link_vars = function(this_tbl, tag){
  if(tag == "") return(this_tbl)
  tbl_to_return = case_when(this_tbl |> str_detect("^l_") ~ paste0(x, "_", tag), T ~ x)
  return(tbl_to_return)
}


