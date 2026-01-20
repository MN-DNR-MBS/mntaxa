lookup_mntaxa <- function(taxonomy_levels = FALSE,
                            sources = FALSE,
                            phys = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE) {

  # check if accepted and all taxa tables are missing
  missing_synonymies <- !exists("syns_raw", envir = parent.frame())
  missing_taxa <- !exists("taxa_raw", envir = parent.frame())

  # load only if something is missing
  if (missing_synonymies || missing_taxa) {
    load_mntaxa(
      synonymies = missing_synonymies,
      all_taxa = missing_taxa
    )
  }

  # format taxa names
  taxa <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources
  )

  # synonymy table
  syns <- syns_raw |>
    dplyr::left_join(taxa)

  # get accepted names
  dlist <- accepted_mntaxa() |>
    dplyr::rename(dlist_id = taxon_id,
           dlist_taxon = taxon,
           dlist_hybrid = hybrid,
           dlist_rank = rank) |>
    dplyr::mutate(dlist_synonymy_id = synonymy_id)

  # synonyms of d-list
  dlist_syns <- dlist |>
    dplyr::left_join(syns |>
                       dplyr::select(synonymy_id, taxon_id, rank))

  # add new dlist_id to the retired d-list taxon's synonymy group
  # some retired synonymies do not have current d-list matches
  dlist_rep <- syns |>
    dplyr::filter(!is.na(d_list_beg_date) & !is.na(d_list_end_date) &
             !(synonymy_id %in% dlist_syns$synonymy_id)) |>
    dplyr::distinct(taxon_id, synonymy_id) |>
    dplyr::inner_join(dlist_syns |>
                        dplyr::distinct(taxon_id, dlist_id, dlist_taxon, dlist_hybrid,
                          dlist_rank, dlist_synonymy_id),
               relationship = "many-to-many") |>
    dplyr::distinct(dlist_id, dlist_taxon, dlist_hybrid, dlist_rank, synonymy_id,
             dlist_synonymy_id)

  # update synonyms of dlist
  # remove duplication from multiple synonymies
  dlist_syns2 <- syns |>
    dplyr::right_join(dlist |>
                 rbind(dlist_rep),
               relationship = "many-to-many") |>
    dplyr::distinct(taxon_id, taxon, hybrid, rank, dlist_id, dlist_taxon, dlist_rank,
             dlist_hybrid, dlist_synonymy_id) |>
    dplyr::rename(synonymy_id = dlist_synonymy_id)


}
