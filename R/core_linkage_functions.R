#' Inner join two well formatted tables, renaming as we go
#'
#' @param tbl1 First table to join
#' @param tbl2 Second table to join
#' @param by_vars Variables to join them on (TODO: make this optional, default to linkage vars)
#' @param tags Optional tags to add as suffix on common variables
#'
#' @returns A tibble
#' @export
#'
#' @examples link_tbl  = inner_join_rename(tbl1, tbl2, get_shared_linkage_vars(tbl1), tags = c("ED", "MVC"))
inner_join_rename = function(tbl1, tbl2, by_vars, tags = c("a", "b")){
  if( (tbl1 |> select(match_id_vars()) |> ncol()) != 1) {print(match_id_vars); stop("tbl1 must have exactly 1 id column")}
  if( (tbl2 |> select(match_id_vars()) |> ncol()) != 1) {print(match_id_vars); stop("tbl2 must have exactly 1 id column")}

  # Join, extract ids
  to_return_tbl = inner_join(tbl1, tbl2, by = by_vars) |> select(match_id_vars())

  # Join original data to ids, renamed, reordered
  to_return_tbl = to_return_tbl |>
    left_join(tbl1 |> rename_with(~tag_link_vars(.x, tags[[1]]))) |>
    left_join(tbl2 |> rename_with(~tag_link_vars(.x, tags[[2]]))) |>
    select(match_id_vars(), sort(tidyselect::peek_vars()))

  return(to_return_tbl)
}
