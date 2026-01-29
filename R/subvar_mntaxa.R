#' Replace accepted subspecies and varieties with species
#'
#' @returns Tibble with acc_taxon_id corresponding to the subspecies/variety and columns for accepted taxon attributes associated with the corresponding species and ending with "_rep" so that they can be used to replace subspecies/variety attributes.
#' @export
#'
#' @examples
#' acc_sub_var <- subvar_mntaxa()
subvar_mntaxa <- function(){

  # format accepted names
  acc <- accepted_mntaxa(
    taxonomy_levels = TRUE
  ) |>
    dplyr::rename_with(.fn = ~ paste("acc", .x, sep = "_")) |>
    dplyr::mutate(synonymy_id = acc_synonymy_id)

  # get species-level for sub-species and varieties
  # isolate subspecies/varieties with species in accepted taxa
  # repair missing taxon names with first two words (n = 2 taxa on 1/26/26)
  # repair missing IDs using taxon names (could create duplicates)
  # add all accepted info consistent with species-level
  acc_sub_var <- acc |>
    dplyr::filter(acc_rank %in% c("subspecies", "variety")) |>
    dplyr::transmute(
      acc_taxon_id,
      acc_taxon = dplyr::coalesce(acc_parent_taxon,
                                  stringr::word(acc_taxon, 1, 2)),
      acc_taxon_id_rep = dplyr::if_else(
        acc_parent_id %in% acc$acc_taxon_id,
        acc_parent_id,
        NA_real_)) |>
    dplyr::left_join(acc |>
                       dplyr::select(acc_taxon,
                                     temp_taxon_id = acc_taxon_id),
                     by = "acc_taxon") |>
    dplyr::mutate(acc_taxon_id_rep = dplyr::coalesce(
      acc_taxon_id_rep, temp_taxon_id)) |>
    dplyr::select(-temp_taxon_id) |>
    dplyr::left_join(acc |>
                       dplyr::select(-synonymy_id) |>
                       dplyr::rename(synonymy_id = acc_synonymy_id),
                     by = c("acc_taxon", "acc_taxon_id_rep" = "acc_taxon_id")
    ) |>
    dplyr::rename_with(~paste0(.x, "_rep"),
                       -starts_with("acc_taxon_id"))

  # return
  return(acc_sub_var)

}
