#' Create MNTaxa look-up table
#'
#' @param taxonomy_levels Binary option (TRUE, FALSE) to include rank of taxa and taxonomic parents.
#' @param sources Binary option (TRUE, FALSE) to include authorities and publications.
#' @param releve Binary option (TRUE, FALSE) to include taxa that are specific to releves and not in MNTaxa (recommended for analyses of older releves).
#' @param phys Binary option (TRUE, FALSE) to include physiognomy.
#' @param strata Binary option (TRUE, FALSE) to include revised physiognomy and strata codes.
#' @param origin Binary option (TRUE, FALSE) to include origin.
#' @param common Binary option (TRUE, FALSE) to include common names.
#' @param cvals Binary option (TRUE, FALSE) to include c-values.
#' @param exclude Binary option (TRUE, FALSE) to include information about excluding taxa from analyses.
#' @param replace_sub_var Binary option (TRUE, FALSE) to replace subspecies and varieties with their species when the species is present in MNTaxa.
#' @param replace_family Binary option (TRUE, FALSE) to replace families with species or genus when only one species or genus, respectively, is included in MNtaxa.
#' @param replace_genus Binary option (TRUE, FALSE) to replace genera with species when only one species is included in MNtaxa.
#' @param drop_higher Binary option (TRUE, FALSE) to drop taxonomic levels above species from the accepted taxa list. Implemented after `replace_family` and `replace_genus`. See `higher_include` for exceptions.
#' @param higher_include Vector of accepted genera or family names to be include in look-up table. The default are included because they don't have lower taxa in the accepted species list (Belonia, Chara, Lychnothamnus, Nitella, Nitellopsis, Spirogyra, Tolypella). Only implemented if `drop_higher` = TRUE..
#' @param excluded_duplicates Binary option (TRUE, FALSE) to reduce multiple accepted name assignments for a taxon_id by removing those with exclusions documented in MNTaxa.
#' @param clean_duplicates Binary option (TRUE, FALSE) to remove duplicate accepted name assignments by assigning a taxon to itself when available and assigning the most likely taxon for a handful of hand-curated duplicates. Recommended for one-to-one taxon-to-accepted look-up table.
#' @param group_accepted Binary option (TRUE, FALSE) to group all potential accepted names. Recommended for one-to-one taxon id-to-taxonomic group look-up table.

#'
#' @returns Tibble of taxon names paired with accepted names and optional attributes. If both `clean_duplicates` and `group_accepted` are selected, `group_accepted` will be used.
#' @export
#'
#' @examples
#' lookup <- lookup_mntaxa()
lookup_mntaxa <- function(taxonomy_levels = FALSE,
                          sources = FALSE,
                          releve = FALSE,
                          phys = FALSE,
                          strata = FALSE,
                          origin = FALSE,
                          common = FALSE,
                          cvals = FALSE,
                          exclude = FALSE,
                          replace_sub_var = FALSE,
                          replace_family = FALSE,
                          replace_genus = FALSE,
                          drop_higher = FALSE,
                          higher_include = c(
                            "Belonia",
                            "Chara",
                            "Lychnothamnus",
                            "Nitella",
                            "Nitellopsis",
                            "Spirogyra",
                            "Tolypella"
                          ),
                          excluded_duplicates = FALSE,
                          clean_duplicates = FALSE,
                          group_accepted = FALSE,
                          group_analysis = FALSE) {
  # turn on taxonomy levels if replacements are selected
  if (any(replace_sub_var, replace_family, replace_genus)) {
    taxonomy_levels <- TRUE
  }

  # turn on excluded table if needed
  if (excluded_duplicates) {
    exclude <- TRUE
  }

  # format taxa names
  taxa <- taxa_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources,
    releve = releve
  )

  # format accepted names
  acc <- accepted_mntaxa(
    taxonomy_levels = taxonomy_levels,
    sources = sources,
    releve = releve,
    phys = phys,
    strata = strata,
    origin = origin,
    common = common,
    cvals = cvals,
    exclude = exclude
  ) |>
    dplyr::rename_with(.fn = ~ paste("acc", .x, sep = "_")) |>
    dplyr::mutate(synonymy_id = acc_synonymy_id)

  # add accepted names for releve taxa if needed
  if (releve) {
    # synonymy table
    syns <- syns_raw |>
      dplyr::full_join(releve_taxa |>
        dplyr::distinct(taxon_id, synonymy_id)) |>
      dplyr::left_join(taxa, by = "taxon_id")
  } else {
    # synonymy table
    syns <- syns_raw |>
      dplyr::left_join(taxa, by = "taxon_id")
  }

  # synonyms of accepted names
  acc_syns <- acc |>
    dplyr::left_join(
      syns |>
        dplyr::select(synonymy_id, colnames(taxa)),
      by = "synonymy_id"
    )

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
      by = "taxon_id",
      relationship = "many-to-many"
    ) |>
    dplyr::select(colnames(acc)) |>
    dplyr::distinct()

  # update synonyms of acc
  # remove duplication from multiple synonymies
  acc_lookup <- syns |>
    dplyr::select(taxon_id, synonymy_id, colnames(taxa)) |>
    dplyr::right_join(
      acc |>
        rbind(acc_rep),
      by = "synonymy_id",
      relationship = "many-to-many"
    ) |>
    dplyr::select(-synonymy_id) |>
    dplyr::distinct() |>
    dplyr::rename(synonymy_id = acc_synonymy_id)

  # replace subspecies/varieties
  if (replace_sub_var) {
    # get species as replacement for subspecies/varieties
    # remove subspecies/varieties missing species in accepted dataset
    acc_sub_var <- subvar_mntaxa(
      acc = acc,
      sources = sources,
      phys = phys,
      strata = strata,
      origin = origin,
      common = common,
      cvals = cvals,
      exclude = exclude
    ) |>
      dplyr::filter(acc_taxon_id_rep %in% acc$acc_taxon_id)

    # add replacements to data
    # remove duplicates (some already matched with species-level)
    acc_lookup <- acc_lookup |>
      dplyr::left_join(acc_sub_var, by = "acc_taxon_id") |>
      dplyr::mutate(
        has_replacement = !is.na(acc_taxon_id_rep),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            data <- dplyr::pick(dplyr::everything())
            if (rep_col %in% names(data)) {
              dplyr::if_else(data[["has_replacement"]],
                data[[rep_col]],
                .x
              )
            } else {
              .x
            }
          }
        )
      ) |>
      dplyr::select(-dplyr::ends_with("_rep"), -has_replacement) |>
      dplyr::distinct()
  }

  if (replace_family) {
    # add taxonomy levels to lookup
    acc_syns_levels <- acc_lookup |>
      dplyr::left_join(
        tax_levels |>
          dplyr::select(
            acc_taxon_id, acc_species, acc_genus,
            acc_family
          ),
        by = "acc_taxon_id"
      )

    # stop if family is missing (exclude group, don't use those)
    if (sum(is.na(acc_syns_levels$acc_family) &
      acc_syns_levels$rank != "group") > 0) {
      stop("Accepted taxa are missing family. Cannot replace families with species or genus.")
    }

    # single species per family in MNTaxa
    # add acc info from species
    # rename columns
    acc_single_fam <- acc_syns_levels |>
      dplyr::filter(acc_rank %in% c(
        "subspecies", "variety", "species",
        "genus"
      )) |>
      dplyr::group_by(acc_family) |>
      dplyr::mutate(
        n_sub = sum(acc_rank %in% c("subspecies", "variety")),
        n_spp = dplyr::n_distinct(acc_species, na.rm = T),
        n_gen = dplyr::n_distinct(acc_genus, na.rm = T)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(
        (n_spp == 0 & n_gen == 1 & acc_genus == acc_taxon) |
          (n_spp == 1 & n_gen == 1 & acc_species == acc_taxon) |
          (n_spp == 1 & n_gen == 1 & n_sub == 1 &
            !(acc_species %in% acc_lookup$acc_taxon))
      ) |>
      dplyr::mutate(acc_taxon_rep = acc_taxon) |>
      dplyr::distinct(acc_family, acc_taxon_rep) |>
      dplyr::rename(acc_taxon = acc_family) |>
      dplyr::left_join(
        acc |>
          dplyr::select(-acc_synonymy_id) |>
          dplyr::rename_with(.fn = ~ paste(.x, "rep", sep = "_")),
        by = "acc_taxon_rep"
      )

    # family that have multiple accepted families
    # can't explore taxa below family that map to other families because
    # only accepted taxa have explicit family assignments
    family_split <- acc_lookup |>
      dplyr::filter(rank == "family" & acc_rank == "family") |>
      dplyr::group_by(taxon) |>
      dplyr::mutate(n_acc = dplyr::n_distinct(acc_taxon)) |>
      dplyr::ungroup() |>
      dplyr::filter(n_acc > 1)

    # family names that have switched with accepted species using old name
    family_switch <- acc_lookup |>
      dplyr::filter(rank == "family" & acc_rank == "family" & taxon != acc_taxon &
        acc_taxon %in% acc_single_fam$acc_taxon) |>
      dplyr::distinct(taxon, acc_taxon) |>
      dplyr::rename(
        acc_family = taxon,
        new_family = acc_taxon
      ) |>
      dplyr::inner_join(
        tax_levels |>
          dplyr::filter(acc_rank != "family"),
        by = "acc_family",
        relationship = "many-to-many"
      )

    # replace the family as the acc assignment
    acc_lookup <- acc_lookup |>
      dplyr::left_join(
        acc_single_fam |>
          dplyr::filter(!(acc_taxon %in% c(family_split$acc_taxon, family_switch$new_family))),
        by = "acc_taxon"
      ) |>
      dplyr::mutate(
        has_replacement = !is.na(acc_taxon_id_rep) & !(taxon %in% family_split$taxon),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            data <- dplyr::pick(dplyr::everything())
            if (rep_col %in% names(data)) {
              dplyr::if_else(data[["has_replacement"]],
                data[[rep_col]],
                .x
              )
            } else {
              .x
            }
          }
        )
      ) |>
      dplyr::select(-dplyr::ends_with("_rep"), -has_replacement) |>
      dplyr::distinct()
  }

  if (replace_genus) {
    # add taxonomy levels to lookup
    acc_syns_levels <- acc_lookup |>
      dplyr::left_join(
        tax_levels |>
          dplyr::select(
            acc_taxon_id, acc_species, acc_genus,
            acc_family
          ),
        by = "acc_taxon_id"
      )

    # stop if genus is missing
    if (sum(is.na(acc_syns_levels$acc_genus) &
      !(acc_syns_levels$rank %in% c("family", "group"))) > 0) {
      stop("Accepted taxa are missing genera. Cannot replace  genera with species.")
    }

    # single species per genus in MNTaxa
    # add acc info from species
    # rename columns
    acc_single_gen <- acc_syns_levels |>
      dplyr::filter(acc_rank %in% c("subspecies", "variety", "species")) |>
      dplyr::group_by(acc_genus) |>
      dplyr::mutate(
        n_sub = sum(acc_rank %in% c("subspecies", "variety")),
        n_spp = dplyr::n_distinct(acc_species, na.rm = T)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter((n_spp == 1 & acc_species == acc_taxon) |
        (n_spp == 1 & n_sub == 1 &
          !(acc_species %in% acc_lookup$acc_taxon))) |>
      dplyr::mutate(acc_taxon_rep = acc_taxon) |>
      dplyr::distinct(acc_genus, acc_taxon_rep) |>
      dplyr::rename(acc_taxon = acc_genus) |>
      dplyr::left_join(
        acc |>
          dplyr::select(-acc_synonymy_id) |>
          dplyr::rename_with(.fn = ~ paste(.x, "rep", sep = "_")),
        by = "acc_taxon_rep"
      )

    # genera that have multiple accepted genera
    genus_split <- acc_lookup |>
      dplyr::filter(rank %in% c(
        "genus", "species", "subspecies", "variety",
        "subspecies variety"
      )) |>
      dplyr::left_join(
        tax_levels |>
          dplyr::select(acc_taxon_id, acc_genus),
        by = "acc_taxon_id"
      ) |>
      dplyr::mutate(genus = dplyr::case_when(
        rank == "genus" ~ taxon,
        rank == "species" & !is.na(parent_taxon) ~ parent_taxon,
        TRUE ~ stringr::word(taxon, 1)
      )) |>
      dplyr::group_by(genus) |>
      dplyr::mutate(n_acc = dplyr::n_distinct(acc_genus)) |>
      dplyr::ungroup() |>
      dplyr::filter(n_acc > 1)

    # genus-level taxon with accepted species using old name
    genus_switch <- acc_lookup |>
      dplyr::filter(rank == "genus" & acc_rank == "genus" & taxon != acc_taxon &
        acc_taxon %in% acc_single_gen$acc_taxon) |>
      dplyr::distinct(taxon, acc_taxon) |>
      dplyr::rename(
        acc_genus = taxon,
        new_genus = acc_taxon
      ) |>
      dplyr::inner_join(
        tax_levels |>
          dplyr::filter(acc_rank != "genus"),
        by = "acc_genus",
        relationship = "many-to-many"
      )

    # replace the genus as the acc assignment
    acc_lookup <- acc_lookup |>
      dplyr::left_join(
        acc_single_gen |>
          dplyr::filter(!(acc_taxon %in%
            c(
              genus_switch$new_genus,
              genus_split$genus
            ))),
        by = "acc_taxon"
      ) |>
      dplyr::mutate(
        has_replacement = !is.na(acc_taxon_id_rep) & !(taxon %in% genus_split$taxon),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            data <- dplyr::pick(dplyr::everything())
            if (rep_col %in% names(data)) {
              dplyr::if_else(data[["has_replacement"]],
                data[[rep_col]],
                .x
              )
            } else {
              .x
            }
          }
        )
      ) |>
      dplyr::select(-dplyr::ends_with("_rep"), -has_replacement) |>
      dplyr::distinct()
  }

  if (excluded_duplicates) {
    # number of accepted matches per taxon_id
    acc_matches <- acc_lookup |>
      dplyr::group_by(taxon_id) |>
      dplyr::mutate(n_acc = dplyr::n_distinct(acc_taxon)) |>
      dplyr::ungroup()

    # taxon assignments with multiple acc
    # acc assignments that are excluded
    # remove acc assignments that are the only option for a taxon
    acc_dups_exclude <- acc_matches |>
      dplyr::filter(n_acc > 1) |>
      dplyr::filter(!is.na(acc_excluded_code)) |>
      dplyr::select(starts_with("acc"), synonymy_id) |>
      dplyr::distinct() |>
      dplyr::anti_join(acc_matches |>
        dplyr::filter(n_acc == 1))

    # remove these excluded duplicates
    acc_lookup <- acc_lookup |>
      dplyr::anti_join(acc_dups_exclude)
  }

  if (clean_duplicates) {
    # if a taxon is accepted, make it's only accepted taxon itself
    # if its species-level parent is accepted and a match, use parent
    # manual corrections for the most likely taxon someone would be recording
    acc_lookup <- acc_lookup |>
      dplyr::group_by(taxon) |>
      dplyr::mutate(
        in_acc = dplyr::if_else(taxon %in% acc_taxon, 1, 0),
        species_in_acc = dplyr::if_else(rank %in% c(
          "variety", "subspecies",
          "subspecies variety"
        ) &
          stringr::word(taxon, 1, 2) %in%
            acc_taxon, 1, 0)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(((in_acc == 0 & species_in_acc == 0) |
        (in_acc == 1 & taxon == acc_taxon) |
        (species_in_acc == 1 & stringr::word(taxon, 1, 2) == acc_taxon)) &
        !(taxon == "Arabis holboellii" &
          acc_taxon == "Boechera retrofracta") &
        !(taxon == "Botrychium lunaria" &
          acc_taxon == "Botrychium crenulatum") &
        !(taxon == "Chamerion angustifolium subsp. angustifolium" &
          acc_taxon == "Eriophorum angustifolium") &
        !(taxon == "Quercus x schuettei" &
          acc_taxon == "Quercus x hillii") &
        !(taxon == "Solanum ptycanthum" &
          acc_taxon == "Solanum nigrum")) |>
      dplyr::select(-c(in_acc, species_in_acc))

    # group together for exporting clean table
    if (!group_accepted & !group_analysis) {
      # create full name column
      if (sources == TRUE) {
        acc_lookup <- acc_lookup |>
          dplyr::mutate(
            acc_full_name = trimws(paste(
              dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
              dplyr::if_else(is.na(acc_author), "", acc_author),
              dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
            ))
          ) |>
          dplyr::select(-c(acc_author, acc_ss_sl))
      } else {
        acc_lookup <- acc_lookup |>
          dplyr::mutate(
            acc_full_name = trimws(paste(
              dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
              dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
            ))
          ) |>
          dplyr::select(-acc_ss_sl)
      }

      # for taxa with multiple matches, summarize those
      acc_lookup <- acc_lookup |>
        dplyr::group_by(taxon, hybrid, rank) |>
        dplyr::summarize(
          dplyr::across(
            c(starts_with("acc_"), synonymy_id),
            ~ {
              non_na <- na.omit(.x)
              if (length(non_na) == 0) {
                NA_character_
              } else {
                paste(sort(unique(non_na)), collapse = "/")
              }
            }
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(acc_assignment = dplyr::if_else(
          stringr::str_detect(acc_taxon, "/"),
          purrr::map_chr(stringr::str_split(acc_taxon, "/"), combine_names),
          acc_taxon
        )) |>
        dplyr::select(-acc_taxon)
    }
  }

  if (drop_higher) {
    if (group_analysis) {
      # save higher taxa info if needed for analysis groups
      acc_lookup_higher <- acc_lookup
    }

    if (releve) {
      # keep releve taxa
      higher_include <- c(higher_include, releve_taxa$taxon)
    }

    # drop family and genera that have multiple species in MNTaxa
    # add parent
    # drop offspring if their parent species is an assignment and matches rank
    # of taxon
    acc_lookup <- acc_lookup |>
      dplyr::filter(acc_rank %in% c(
        "species", "subspecies", "variety",
        "subspecies variety"
      ) |
        acc_taxon %in% higher_include)
  }

  if (group_accepted | group_analysis) {
    # groups propagate through all assignments, such that assigned taxa in the
    # group are put in the group, even if they are only assigned to themselves

    # unique combos of taxon and acc IDs (graph edges)
    acc_edges <- acc_lookup |>
      dplyr::distinct(taxon_id, acc_taxon_id)

    # find connected components
    membership <- connected_components(acc_edges)

    # add groups from components to synonymy
    acc_lookup <- data.frame(
      taxon_id = as.numeric(names(membership)),
      acc_group = unname(membership)
    ) |>
      dplyr::left_join(acc_lookup, by = "taxon_id")

    # create full name column
    if (sources == TRUE) {
      acc_lookup <- acc_lookup |>
        dplyr::mutate(acc_full_name = trimws(paste(
          dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
          dplyr::if_else(is.na(acc_author), "", acc_author),
          dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
        ))) |>
        dplyr::select(-c(acc_author, acc_ss_sl))
    } else {
      acc_lookup <- acc_lookup |>
        dplyr::mutate(acc_full_name = trimws(paste(
          dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
          dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
        ))) |>
        dplyr::select(-acc_ss_sl)
    }

    # summarize by acc_group
    # name by concatenating names
    acc_lookup <- acc_lookup |>
      dplyr::group_by(acc_group) |>
      dplyr::mutate(dplyr::across(
        c(starts_with("acc_"), synonymy_id),
        ~ {
          non_na <- na.omit(.x)
          if (length(non_na) == 0) {
            NA_character_
          } else {
            paste(sort(unique(non_na)), collapse = "/")
          }
        }
      )) |>
      dplyr::ungroup() |>
      dplyr::mutate(acc_assignment = dplyr::if_else(
        stringr::str_detect(acc_taxon, "/"),
        purrr::map_chr(stringr::str_split(acc_taxon, "/"), combine_names),
        acc_taxon
      )) |>
      dplyr::select(-c(acc_taxon, acc_group)) |>
      dplyr::distinct()
  }

  if (group_analysis) {
    # get current acc_assignments based on taxa
    # add all taxa
    acodes_assigned <- analysis_codes_v2 |>
      dplyr::select(taxon_id, analysis_code) |>
      dplyr::inner_join(
        acc_lookup |>
          dplyr::select(taxon_id, acc_assignment),
        by = "taxon_id"
      ) |>
      dplyr::select(-taxon_id) |>
      dplyr::distinct() |>
      dplyr::left_join(acc_lookup, by = "acc_assignment") |>
      dplyr::select(c(names(acc_lookup), analysis_code))

    # if higher were dropped, add back in if they have an analysis group
    if (drop_higher) {
      # get acc info for dropped higher
      acodes_higher <- analysis_codes_v2 |>
        dplyr::select(taxon_id, analysis_code) |>
        dplyr::inner_join(
          acc_lookup_higher |>
            dplyr::anti_join(
              acodes_assigned |>
                dplyr::distinct(taxon_id),
              by = "taxon_id"
            ),
          by = "taxon_id"
        )

      # create full name column
      if (sources == TRUE) {
        acodes_higher <- acodes_higher |>
          dplyr::mutate(acc_full_name = trimws(paste(
            dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
            dplyr::if_else(is.na(acc_author), "", acc_author),
            dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
          ))) |>
          dplyr::select(-c(acc_author, acc_ss_sl))
      } else {
        acodes_higher <- acodes_higher |>
          dplyr::mutate(acc_full_name = trimws(paste(
            dplyr::if_else(is.na(acc_taxon), "", acc_taxon),
            dplyr::if_else(is.na(acc_ss_sl), "", acc_ss_sl)
          ))) |>
          dplyr::select(-acc_ss_sl)
      }

      # collapse if multiple acc_taxon for a given taxon_id
      acodes_higher <- acodes_higher |>
        dplyr::group_by(taxon_id) |>
        dplyr::mutate(dplyr::across(
          c(starts_with("acc_"), synonymy_id),
          ~ {
            non_na <- na.omit(.x)
            if (length(non_na) == 0) {
              NA_character_
            } else {
              paste(sort(unique(non_na)), collapse = "/")
            }
          }
        )) |>
        dplyr::ungroup() |>
        dplyr::mutate(acc_assignment = dplyr::if_else(
          stringr::str_detect(acc_taxon, "/"),
          purrr::map_chr(stringr::str_split(acc_taxon, "/"), combine_names),
          acc_taxon
        )) |>
        dplyr::select(names(acodes_assigned)) |>
        dplyr::distinct() |>
        dplyr::mutate(across(
          where(is.numeric) &
            (starts_with("acc") | all_of("synonymy_id")),
          as.character
        ))

      # add to acodes_assigned
      acodes_assigned <- acodes_assigned |>
        dplyr::bind_rows(acodes_higher)
    }

    # combine all analysis codes
    # analysis code taxa with no accepted taxa (a few hard-coded releve groups)
    acodes <- acodes_assigned |>
      dplyr::full_join(analysis_codes_v2 |>
        dplyr::select(taxon_id, taxon, analysis_code) |>
        dplyr::anti_join(
          acodes_assigned |>
            dplyr::select(taxon_id),
          by = "taxon_id"
        ))

    # add analysis codes
    acc_lookup <- acc_lookup |>
      dplyr::anti_join(acodes |>
        dplyr::select(taxon_id, taxon)) |>
      dplyr::bind_rows(acodes) |>
      dplyr::mutate(analysis_group = dplyr::if_else(!is.na(analysis_code),
        analysis_code,
        acc_assignment
      ))
  }

  # return lookup table
  return(acc_lookup |>
    tibble::as_tibble())
}
