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

#' Title
#'
#' @param tbl1 First table to join
#' @param tbl2 Second table to join
#' @param link_instruction_tbl A tibble of cascading join instructions
#' @param name_tags Optional name tags to suffix common variables
#' @param shrink_tbl1 Whether to shink first table as it is iteratively joined. Default TRUE.
#' @param shrink_tbl2 Whether to shink second table as it is iteratively joined. Default TRUE.
#'
#' @returns A tibble of the cascade-joined tables
#' @export
#'
#' @examples cascade_link(ed_tbl, mvc_tbl, ed_mvc_linkage_plan_tbl, name_tags = c("ed","mvc"))
cascade_link = function(tbl1, tbl2, link_instruction_tbl, name_tags = c("",""), shrink_tbl1 = T, shrink_tbl2 = T){
  # Base cases
  if(is_null(tbl1) | is_null(tbl2)) return(NULL)
  if((nrow(tbl1) == 0) | (nrow(tbl2) == 0)) return(NULL)
  if(is_null(link_instruction_tbl)) return(NULL)
  if(nrow(link_instruction_tbl) == 0) return(NULL)

  # Extract linkage params
  link_descr_tbl = link_instruction_tbl |> select(matches("^link_"))
  this_link_descr = link_descr_tbl |> slice(1)
  link_param_tbl = link_instruction_tbl |> select(matches("^l_"))
  these_link_vars = link_param_tbl |> slice_head(n=1) |> map_lgl(~ !is.na(.x) & .x == 0) |> enframe() |> filter(value) |> pull(name)
  these_filter_vars = link_param_tbl |> slice(1) |> map_lgl(~ !is.na(.x) & .x > 0) |> enframe() |> filter(value) |> pull(name)
  id1 = tbl1 |> select(match_id_vars()) |> names(); id2 = tbl2 |> select(match_id_vars()) |> names()

  # TESTS: tbl1 and tbl2 have all variable names
  link_param_check_tbl = tibble(link_params = names(link_param_tbl), tbl1 = link_params %in% names(tbl1), tbl2 = link_params %in% names(tbl2))
  if(!all(link_param_check_tbl$tbl1 == T)) {print(link_param_check_tbl); stop(paste("All link parameters not in tbl1!"))}
  if(!all(link_param_check_tbl$tbl2 == T)) {print(link_param_check_tbl); stop(paste("All link parameters not in tbl2!"))}
  # TODO ids present?
  # TODO link plan has link_step and link_step_num?

  # Perform current link step...
  joined_tbl = tbl1 |> inner_join_rename(tbl2, by_vars = these_link_vars, tags = name_tags) |>
    bind_cols(this_link_descr) |> select(matches("^link_"), match_id_vars(), everything())

  # TODO: ...then filter...

  # Drop IDs from next linkage if requested
  if(shrink_tbl1){ tbl1 = tbl1 |> filter(!(pull(select(tbl1, matches(id1)))) %in% pull(select(joined_tbl, matches(id1)))) }
  if(shrink_tbl2){ tbl2 = tbl2 |> filter(!(pull(select(tbl2, matches(id2)))) %in% pull(select(joined_tbl, matches(id2)))) }

  # recurse on rest of tables
  rest_of_joined_tbl = cascade_link(tbl1, tbl2,
                                    link_instruction_tbl |> slice_tail(n=-1), # Rest of instruction list
                                    name_tags, shrink_tbl1, shrink_tbl2)

  # join my results with child results
  joined_tbl_to_return = bind_rows(joined_tbl, rest_of_joined_tbl)

  return(joined_tbl_to_return)
}
