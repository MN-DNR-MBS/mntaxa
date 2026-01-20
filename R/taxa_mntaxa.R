#' Create table of all taxa
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#'
#' @returns Tibble of currently accepted taxa in MNTaxa and any additional information requested by the user.
#' @export
#'
#' @examples
#' taxa <- taxa_mntaxa()
taxa_mntaxa <- function(taxonomy_levels = FALSE,
                        sources = FALSE) {
  # check if all taxa table is missing
  missing_taxa <- !exists("taxa_raw", envir = parent.frame())

  # check each optional set individually
  missing_taxonomy <- taxonomy_levels && !all(c("pars_raw", "rank_raw") %in% ls(envir = parent.frame()))
  missing_sources <- sources && !all(c("auth_raw", "pubs_raw") %in% ls(envir = parent.frame()))

  # load only if something is missing
  if (missing_taxa || missing_taxonomy || missing_sources) {
    load_mntaxa(
      synonymies = FALSE,
      all_taxa = missing_taxa,
      taxonomy_levels = missing_taxonomy,
      sources = missing_sources,
      phys = FALSE,
      origin = FALSE,
      common = FALSE,
      cvals = FALSE,
      exclude = FALSE
    )
  }

  # save hybrid name with symbol and without
  # remove \t at end of one name
  dat <- taxa_raw |>
    dplyr::rename(taxon_id = id) |>
    dplyr::mutate(
      hybrid = dplyr::if_else(stringr::str_detect(taxon, "×") |
                                is_hybrid == 1, taxon,
                              NA_character_
      ) |>
        stringr::str_replace_all("\\ x\\ ", "\\ ×\\ ") |>
        stringr::str_replace_all("\\ x", "\\ ×"),
      hybrid = dplyr::if_else(stringr::str_sub(hybrid, 1, 1) == "x",
        sub("^.", "×", hybrid),
        hybrid
      ),
      taxon = dplyr::if_else(!is.na(hybrid),
        hybrid |>
          stringr::str_replace_all("×", "x ") |>
          stringr::str_replace_all("x  ", "x "),
        taxon |>
          stringr::str_remove("\\\t")
      ),
      hybrid_parents = stringr::str_replace(hybrid_parents, "\\ X\\ ",
                                            "\\ x\\ ") |>
        stringr::str_replace("\\ ×\\ ", "\\ x\\ "),
      hybrid_parents = if_else(hybrid_parents == "", NA_character_,
                               hybrid_parents)
    )


  # add taxonomy if selected
  if (taxonomy_levels) {
    dat <- dat |>
      dplyr::left_join(rank_raw |>
        dplyr::rename(rank_id = id) |>
        dplyr::select(rank_id, rank)) |>
      dplyr::left_join(pars_raw |>
        dplyr::select(taxon_id, parent_id) |>
        dplyr::left_join(dat |>
          dplyr::transmute(taxon_id,
            parent_taxon = taxon
          )))
  }

  # add sources if selected
  if (sources) {
    dat <- dat |>
      dplyr::left_join(auth_raw |>
        dplyr::rename(author_id = id) |>
        dplyr::select(author_id, author)) |>
      dplyr::left_join(pubs_raw |>
        dplyr::rename(
          publication_id = id,
          publication = title
        ) |>
        dplyr::select(publication_id, publication))
  }

  # remove unnecessary ID columns
  dat <- dat |>
    dplyr::select(-c(author_id, rank_id, publication_id, is_hybrid, begin_date,
                     end_date, created_at, updated_at))

  # return
  return(dat |>
    tibble::as_tibble())
}
