#' Create table of accepted taxa
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#' @param phys Binary option (TRUE, FALSE) to include physiognomy.
#' @param origin Binary option (TRUE, FALSE) to include origin.
#' @param common Binary option (TRUE, FALSE) to include common names.
#' @param cvals Binary option (TRUE, FALSE) to include c-values.
#' @param exclude Binary option (TRUE, FALSE) to include information about excluding taxa from analyses.
#'
#' @returns Tibble of currently accepted taxa in MNTaxa and any additional information requested by the user.
#' @export
#'
#' @examples
#' accepted <- accpeted_mntaxa()
accepted_mntaxa <- function(taxonomy_levels = FALSE,
                            sources = FALSE,
                            phys = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE) {
  # check if accepted and all taxa tables are missing
  missing_accepted <- !exists("syns", envir = parent.frame())
  missing_taxa <- !exists("taxa", envir = parent.frame())

  # check each optional set individually
  missing_taxonomy <- taxonomy_levels && !all(c("pars", "rank") %in% ls(envir = parent.frame()))
  missing_sources <- sources && !all(c("auth", "pubs") %in% ls(envir = parent.frame()))
  missing_phys <- phys && !all(c("phys_codes", "syn_phys") %in% ls(envir = parent.frame()))
  missing_origin <- origin && !all(c("origin_codes", "syn_or") %in% ls(envir = parent.frame()))
  missing_common <- common && !exists("syn_comm", envir = parent.frame())
  missing_cvals <- cvals && !exists("syn_cvals", envir = parent.frame())
  missing_exclude <- exclude && !all(c("syn_exclude", "exclude_codes") %in% ls(envir = parent.frame()))

  # load only if something is missing
  if (missing_accepted || missing_taxa || missing_taxonomy || missing_sources || missing_phys || missing_origin || missing_common || missing_cvals || missing_exclude) {
    load_mnnpc(
      accepted = missing_accepted,
      all_taxa = missing_taxa,
      taxonomy_levels = missing_taxonomy,
      sources = missing_sources,
      phys = missing_phys,
      origin = missing_origin,
      common = missing_common,
      cvals = missing_cvals,
      exclude = missing_exclude
    )
  }

  # format taxa names
  taxa2 <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources
  )

  # add taxon name and any additional info
  dat <- syns |>
    dplyr::left_join(taxa2)

  # add physcodes
  if (phys) {
    # add phys names and codes to synonymy IDs
    syn_phys2 <- syn_phys |>
      dplyr::left_join(phys_codes |>
        dplyr::transmute(
          physiognomy_id = id,
          physiognomy,
          physcode = physiognomy_code
        )) |>
      dplyr::filter(!is.na(physiognomy) & is_acceptable == 1) |>
      dplyr::group_by(synonymy_id) |>
      dplyr::summarize(
        physiognomy = paste(sort(unique(physiognomy)), collapse = "/"),
        physcode = paste(sort(unique(physcode)), collapse = "/"),
        .groups = "drop"
      )

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_phys2)
  }

  # add origin
  if (origin) {
    # add status to synonymy IDs
    syn_or2 <- syn_or |>
      dplyr::left_join(origin_codes |>
        dplyr::transmute(
          native_status_id = id,
          native_status
        )) |>
      dplyr::transmute(
        synonymy_id = id,
        native_status
      ) |>
      dplyr::filter(!is.na(native_status)) |>
      dplyr::group_by(synonymy_id) |>
      dplyr::summarize(
        native_status = paste(sort(unique(native_status)), collapse = "/"),
        .groups = "drop"
      )


    # add to data
    dat <- dat |>
      dplyr::left_join(syn_or2)
  }

  # add origin
  if (common) {
    # add collapse names by synonymy ID
    syn_comm2 <- syn_comm |>
      dplyr::group_by(synonymy_id) |>
      dplyr::summarize(
        common_name = paste(sort(unique(common_name)), collapse = "/"),
        .groups = "drop"
      )

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_comm2)
  }

  # add c-values
  if (cvals) {
    # add collapse names by synonymy ID
    syn_cvals2 <- syn_cvals |>
      dplyr::filter(conservationism_coefficient %in% 0:10 & is.na(end_date)) |>
      dplyr::group_by(synonymy_id) |>
      dplyr::mutate(cvals = n_distinct(conservationism_coefficient)) |>
      dplyr::ungroup() |>
      dplyr::filter(cvals == 1 | publication_id == 55) |> # 55 = MPCA Rapid FQA
      dplyr::rename(c_value = conservationism_coefficient) |>
      dplyr::distinct(synonymy_id, c_value)

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_cvals2)
  }

  # exclusion info
  if (exclude) {
    # add descriptions to synonymy
    syn_exclude2 <- syn_exclude |>
      dplyr::filter(!is.na(begin_date) & is.na(end_date)) |>
      dplyr::left_join(exclude_codes |>
        dplyr::transmute(
          excluded_reason_id = id,
          excluded_code = excluded_reason_code,
          excluded_desc = description
        )) |>
      dplyr::distinct(synonymy_id, excluded_code, excluded_desc)

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_exclude2)
  }

  # return
  return(dat |>
    tibble::as_tibble())
}
