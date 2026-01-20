lookup_mntaxa <- function(taxonomy_levels = FALSE,
                            sources = FALSE,
                            phys = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE) {

  # format taxa names
  taxa <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources
  )

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
    dplyr::rename_with(.fn = ~paste("acc", .x, sep = "_")) |>
    dplyr::mutate(synonymy_id = acc_synonymy_id)

  # synonymy table
  syns <- syns_raw |>
    dplyr::left_join(taxa)

  # synonyms of d-list
  acc_syns <- acc |>
    dplyr::left_join(syns |>
                       dplyr::select(synonymy_id, colnames(taxa)))

  # add new acc_id to the retired d-list taxon's synonymy group
  # some retired synonymies do not have current d-list matches
  acc_rep <- syns |>
    dplyr::filter(!is.na(d_list_beg_date) & !is.na(d_list_end_date) &
             !(synonymy_id %in% acc_syns$synonymy_id)) |>
    dplyr::distinct(taxon_id, synonymy_id) |>
    dplyr::inner_join(acc_syns |>
                        dplyr::select(taxon_id, starts_with("acc")) |>
                        dplyr::distinct(),
               relationship = "many-to-many") |>
    dplyr::select(colnames(acc)) |>
    dplyr::distinct()

  # update synonyms of acc
  # remove duplication from multiple synonymies
  acc_syns2 <- syns |>
    dplyr::select(taxon_id, synonymy_id, colnames(taxa)) |>
    dplyr::right_join(acc |>
                 rbind(acc_rep),
               relationship = "many-to-many") |>
    dplyr::select(-synonymy_id) |>
    dplyr::distinct() |>
    dplyr::rename(synonymy_id = acc_synonymy_id)

  # return lookup table
  return(acc_syns2 |>
           tibble::as_tibble())


}
