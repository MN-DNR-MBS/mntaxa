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
  missing_taxa <- !exists("taxa", envir = parent.frame())

  # check each optional set individually
  missing_taxonomy <- taxonomy_levels && !all(c("pars", "rank") %in% ls(envir = parent.frame()))
  missing_sources <- sources && !all(c("auth", "pubs") %in% ls(envir = parent.frame()))

  # load only if something is missing
  if (missing_taxa || missing_taxonomy || missing_sources) {
    load_mnnpc(
      accepted = FALSE,
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
  dat <- taxa |>
    dplyr::rename(taxon_id = id) |>
    dplyr::mutate(
      hybrid = if_else(str_detect(taxon, "×") | is_hybrid == 1, taxon,
        NA_character_
      ) |>
        str_replace_all("\\ x\\ ", "\\ ×\\ ") |>
        str_replace_all("\\ x", "\\ ×"),
      hybrid = if_else(str_sub(hybrid, 1, 1) == "x",
        sub("^.", "×", hybrid),
        hybrid
      ),
      taxon = if_else(!is.na(hybrid),
        hybrid |>
          str_replace_all("×", "x ") |>
          str_replace_all("x  ", "x "),
        taxon |>
          str_remove("\\\t")
      )
    )


  # add taxonomy if selected
  if (taxonomy_levels) {
    dat <- dat |>
      dplyr::left_join(rank |>
        dplyr::rename(rank_id = id) |>
        dplyr::select(rank_id, rank)) |>
      dplyr::left_join(pars |>
        dplyr::select(taxon_id, parent_id) |>
        dplyr::left_join(dat |>
          dplyr::transmute(taxon_id,
            parent_taxon = taxon
          )))
  }

  # add sources if selected
  if (sources) {
    dat <- dat |>
      dplyr::left_join(auth |>
        dplyr::rename(author_id = id) |>
        dplyr::select(author_id, author)) |>
      dplyr::left_join(pubs |>
        dplyr::rename(
          publication_id = id,
          publication = title
        ) |>
        dplyr::select(publication_id, publication))
  }

  # return
  return(dat |>
    tibble::as_tibble())
}
