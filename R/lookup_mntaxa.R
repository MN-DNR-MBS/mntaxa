#' Create MNTaxa lookup table
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#' @param phys Binary option (TRUE, FALSE) to include physiognomy.
#' @param origin Binary option (TRUE, FALSE) to include origin.
#' @param common Binary option (TRUE, FALSE) to include common names.
#' @param cvals Binary option (TRUE, FALSE) to include c-values.
#' @param exclude Binary option (TRUE, FALSE) to include information about excluding taxa from analyses.
#' @param replace_sub_var Binary option (TRUE, FALSE) to replace subspecies and varieties with their species when the species is present in MNTaxa.
#' @param replace_family Binary option (TRUE, FALSE) to replace families with species or genus when only one species or genus, respectively, is included in MNtaxa.
#' @param replace_genus Binary option (TRUE, FALSE) to replace genera with species when only one species is included in MNtaxa.
#' @param excluded_duplicates Binary option (TRUE, FALSE) to reduce duplicate accepted name assignments by removing those with exclusions documented in MNTaxa.
#' @param clean_duplicates Binary option (TRUE, FALSE) to remove duplicate accepted name assignments by assigning a taxon to itself when available, assigning a subspecies/variety to the species when available, and assigning the most likely taxon for a handful of hand-curated duplicates.
#' @param group_accepted Binary option (TRUE, FALSE) to group all potential accepted names.
#'
#' @returns Tibble of taxon names paired with accepted names and optional attributes.
#' @export
#'
#' @examples
#' lookup <- lookup_mntaxa()
lookup_mntaxa <- function(taxonomy_levels = FALSE,
                          sources = FALSE,
                          phys = FALSE,
                          origin = FALSE,
                          common = FALSE,
                          cvals = FALSE,
                          exclude = FALSE,
                          replace_sub_var = FALSE,
                          replace_family = FALSE,
                          replace_genus = FALSE,
                          excluded_duplicates = FALSE,
                          clean_duplicates = FALSE,
                          group_accepted = FALSE) {
  # format taxa names
  taxa <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources
  )

  # turn on taxonomy levels if replacements are selected
  if(any(replace_sub_var, replace_family, replace_genus)){
    taxonomy_levels <- TRUE
  }

  # format accepted names
  acc <- accepted_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources,
    phys = phys,
    origin = origin,
    common = common,
    cvals = cvals,
    exclude = exclude
  ) |>
    dplyr::rename_with(.fn = ~ paste("acc", .x, sep = "_")) |>
    dplyr::mutate(synonymy_id = acc_synonymy_id)

  # synonymy table
  syns <- syns_raw |>
    dplyr::left_join(taxa)

  # synonyms of accepted names
  acc_syns <- acc |>
    dplyr::left_join(syns |>
      dplyr::select(synonymy_id, colnames(taxa)))

  # add new acc_id to the retired accepted names's synonymy group
  # some retired synonymies do not have current accepted matches
  acc_rep <- syns |>
    dplyr::filter(!is.na(d_list_beg_date) & !is.na(d_list_end_date) &
      !(synonymy_id %in% acc_syns$synonymy_id)) |>
    dplyr::distinct(taxon_id, synonymy_id) |>
    dplyr::inner_join(
      acc_syns |>
        dplyr::select(taxon_id, starts_with("acc")) |>
        dplyr::distinct(),
      relationship = "many-to-many"
    ) |>
    dplyr::select(colnames(acc)) |>
    dplyr::distinct()

  # update synonyms of acc
  # remove duplication from multiple synonymies
  acc_syns2 <- syns |>
    dplyr::select(taxon_id, synonymy_id, colnames(taxa)) |>
    dplyr::right_join(
      acc |>
        rbind(acc_rep),
      relationship = "many-to-many"
    ) |>
    dplyr::select(-synonymy_id) |>
    dplyr::distinct() |>
    dplyr::rename(synonymy_id = acc_synonymy_id)

  # replace subspecies/varieties
  if(replace_sub_var){
    # get species as replacement for subspecies/varieties
    # remove subspecies/varieties missing species in accepted dataset
    acc_sub_var <- subvar_mntaxa() |>
      dplyr::filter(acc_taxon_id_rep %in% acc$acc_taxon_id)

    # add replacements to data
    # remove duplicates (some already matched with species-level)
    acc_syns2 <- acc_syns2 |>
      dplyr::left_join(acc_sub_var, by = "acc_taxon_id") |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            if (rep_col %in% names(dplyr::cur_data())) {
              dplyr::if_else(!is.na(dplyr::cur_data()[[rep_col]]),
                             dplyr::cur_data()[[rep_col]],
                             .x)
            } else {
              .x
            }
          }
        )
      ) |>
      dplyr::select(-dplyr::ends_with("_rep")) |>
      dplyr::distinct()

  }




  # return lookup table
  return(acc_syns2 |>
           tibble::as_tibble())
}
