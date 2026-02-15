#' Create table of all taxa
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#' @param releve Binary option (TRUE, FALSE) to include taxa that are specific to releves and not in MNTaxa (recommended for analyses of older releves).
#'
#' @returns Tibble of currently accepted taxa in MNTaxa and any additional information requested by the user.
#' @export
#'
#' @examples
#' taxa <- taxa_mntaxa()
taxa_mntaxa <- function(taxonomy_levels = FALSE,
                        sources = FALSE,
                        releve = FALSE) {
  # save hybrid name with symbol and without
  # remove \t at end of one name
  taxa <- taxa_raw |>
    dplyr::rename(taxon_id = id) |>
    dplyr::mutate(
      hybrid = dplyr::case_when(
        stringr::str_detect(taxon, "×") ~ taxon,
        taxon %in% c(
          "Potentilla hippiana",
          "Staphylea trifolia"
        ) ~ NA_character_,
        is_hybrid == 1 ~ taxon,
        TRUE ~ NA_character_
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
      hybrid_parents = stringr::str_replace(
        hybrid_parents, "\\ X\\ ",
        "\\ x\\ "
      ) |>
        stringr::str_replace("\\ ×\\ ", "\\ x\\ "),
      hybrid_parents = dplyr::if_else(hybrid_parents == "", NA_character_,
        hybrid_parents
      )
    )

  # add taxonomy if selected
  if (taxonomy_levels) {
    taxa <- taxa |>
      dplyr::left_join(rank_raw |>
        dplyr::rename(rank_id = id) |>
        dplyr::select(rank_id, rank))

    taxa <- taxa |>
      dplyr::left_join(pars_raw |>
        dplyr::distinct(taxon_id, parent_id) |>
        dplyr::left_join(taxa |>
          dplyr::transmute(
            parent_id = taxon_id,
            parent_rank = rank,
            parent_taxon = taxon
          )))
  }

  # add sources if selected
  if (sources) {
    taxa <- taxa |>
      dplyr::left_join(auth_raw |>
        dplyr::rename(author_id = id) |>
        dplyr::select(author_id, author)) |>
      dplyr::mutate(author = dplyr::if_else(
        author == "", NA_character_, author
      )) |>
      dplyr::left_join(pubs_raw |>
        dplyr::rename(
          publication_id = id,
          publication = title
        ) |>
        dplyr::select(publication_id, publication))
  }

  # add taxa from releves (hard-coded groups, not in MNTaxa)
  if (releve) {
    # add rank if requested
    if (taxonomy_levels) {
      releve_taxa <- releve_taxa |>
        dplyr::mutate(rank = "group")
    }

    # add to taxa
    taxa <- taxa |>
      dplyr::full_join(releve_taxa |>
        dplyr::select(-synonymy_id))
  }

  # remove unnecessary ID columns
  taxa <- taxa |>
    dplyr::select(-c(
      author_id, rank_id, publication_id, is_hybrid, begin_date,
      end_date, created_at, updated_at
    ))

  # return
  return(taxa |>
    tibble::as_tibble())
}
