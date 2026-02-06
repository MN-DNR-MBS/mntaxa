#' Create table of accepted taxa
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#' @param phys Binary option (TRUE, FALSE) to include physiognomy.
#' @param strata Binary option (TRUE, FALSE) to include revised physiognomy and strata codes.
#' @param origin Binary option (TRUE, FALSE) to include origin.
#' @param common Binary option (TRUE, FALSE) to include common names.
#' @param cvals Binary option (TRUE, FALSE) to include c-values.
#' @param exclude Binary option (TRUE, FALSE) to include information about excluding taxa from analyses.
#' @param releve Binary option (TRUE, FALSE) to include taxa that are specific to releves and not in MNTaxa (recommended for analyses of older releves).
#'
#' @returns Tibble of currently accepted taxa in MNTaxa and any additional information requested by the user.
#' @export
#'
#' @examples
#' accepted <- accpeted_mntaxa()
accepted_mntaxa <- function(taxonomy_levels = FALSE,
                            sources = FALSE,
                            releve = FALSE,
                            phys = FALSE,
                            strata = FALSE,
                            origin = FALSE,
                            common = FALSE,
                            cvals = FALSE,
                            exclude = FALSE) {
  # check if accepted and all taxa tables are missing
  missing_synonymies <- !exists("syns_raw", envir = .GlobalEnv)
  missing_taxa <- !exists("taxa_raw", envir = .GlobalEnv)

  # check each optional set individually
  missing_taxonomy <- taxonomy_levels && !all(c("pars_raw", "rank_raw") %in% ls(envir = .GlobalEnv))
  missing_sources <- sources && !all(c("auth_raw", "pubs_raw") %in% ls(envir = .GlobalEnv))
  missing_phys <- phys && !all(c("phys_codes_raw", "syn_phys_raw") %in% ls(envir = .GlobalEnv))
  missing_origin <- origin && !all(c("origin_codes_raw", "syn_or_raw") %in% ls(envir = .GlobalEnv))
  missing_common <- common && !exists("syn_comm_raw", envir = .GlobalEnv)
  missing_cvals <- cvals && !exists("syn_cvals_raw", envir = .GlobalEnv)
  missing_exclude <- exclude && !all(c("syn_exclude_raw", "exclude_codes_raw") %in% ls(envir = .GlobalEnv))

  # load only if something is missing
  if (missing_synonymies || missing_taxa || missing_taxonomy || missing_sources || missing_phys || missing_origin || missing_common || missing_cvals || missing_exclude) {
    load_mntaxa(
      synonymies = missing_synonymies,
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
  taxa <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources,
    releve = releve
  )

  # add releve taxa if selected
  if (releve) {

    # add taxon name and optionally taxonomy levels and sources
    dat <- syns_raw |>
      dplyr::filter(!is.na(d_list_beg_date) & is.na(d_list_end_date)) |>
      dplyr::distinct(taxon_id, synonymy_id) |>
      dplyr::full_join(releve_taxa |>
                         dplyr::distinct(taxon_id, synonymy_id)) |>
      dplyr::left_join(taxa, by = "taxon_id")

  } else {

    # add taxon name and optionally taxonomy levels and sources
    dat <- syns_raw |>
      dplyr::filter(!is.na(d_list_beg_date) & is.na(d_list_end_date)) |>
      dplyr::distinct(taxon_id, synonymy_id) |>
      dplyr::left_join(taxa, by = "taxon_id")

  }

  # add physcodes
  if (phys) {
    # add phys names and codes to synonymy IDs
    syn_phys <- syn_phys_raw |>
      dplyr::left_join(phys_codes_raw |>
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
      dplyr::left_join(syn_phys)
  }

  # add modified physcodes and stratacodes
  if (strata) {
    # add to phys_strata data
    dat <- dat |>
      dplyr::left_join(
        phys_strata |>
          dplyr::rename(taxon = acc_taxon),
        by = "taxon"
      )
  }

  # add origin
  if (origin) {
    # add status to synonymy IDs
    syn_or <- syn_or_raw |>
      dplyr::left_join(origin_codes_raw |>
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
      dplyr::left_join(syn_or)
  }

  # add common name
  if (common) {
    # add collapse names by synonymy ID
    syn_comm <- syn_comm_raw |>
      dplyr::group_by(synonymy_id) |>
      dplyr::summarize(
        common_name = paste(sort(unique(common_name)), collapse = "/"),
        .groups = "drop"
      )

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_comm)
  }

  # add c-values
  if (cvals) {
    # add collapse names by synonymy ID
    syn_cvals <- syn_cvals_raw |>
      dplyr::filter(conservationism_coefficient %in% 0:10 & is.na(end_date)) |>
      dplyr::group_by(synonymy_id) |>
      dplyr::mutate(cvals = n_distinct(conservationism_coefficient)) |>
      dplyr::ungroup() |>
      dplyr::filter(cvals == 1 | publication_id == 55) |> # 55 = MPCA Rapid FQA
      dplyr::rename(c_value = conservationism_coefficient) |>
      dplyr::distinct(synonymy_id, c_value)

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_cvals)
  }

  # exclusion info
  if (exclude) {
    # add descriptions to synonymy
    syn_exclude <- syn_exclude_raw |>
      dplyr::filter(!is.na(begin_date) & is.na(end_date)) |>
      dplyr::left_join(exclude_codes_raw |>
        dplyr::transmute(
          excluded_reason_id = id,
          excluded_code = excluded_reason_code,
          excluded_desc = description
        )) |>
      dplyr::distinct(synonymy_id, excluded_code, excluded_desc)

    # add to data
    dat <- dat |>
      dplyr::left_join(syn_exclude)
  }

  # return
  return(dat |>
    tibble::as_tibble())
}
