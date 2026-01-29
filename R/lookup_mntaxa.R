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
#' @param drop_higher Binary option (TRUE, FALSE) to drop taxonomic levels above species from the accepted taxa list. Implemented after `replace_family` and `replace_genus`. See `higher_include` for exceptions.
#' @param higher_include Vector of accepted genera or family names to be include in look-up table. The default are included because they don't have lower taxa in the accepted species list (Belonia, Chara, Lychnothamnus, Nitella, Nitellopsis, Spirogyra, Tolypella). Only implemented if `drop_higher` = TRUE..
#' @param excluded_duplicates Binary option (TRUE, FALSE) to reduce duplicate accepted name assignments by removing those with exclusions documented in MNTaxa.
#' @param clean_duplicates Binary option (TRUE, FALSE) to remove duplicate accepted name assignments by assigning a taxon to itself when available and assigning the most likely taxon for a handful of hand-curated duplicates.
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
                          drop_higher = FALSE,
                          higher_include = c("Belonia",
                                             "Chara",
                                             "Lychnothamnus",
                                             "Nitella",
                                             "Nitellopsis",
                                             "Spirogyra",
                                             "Tolypella"),
                          excluded_duplicates = FALSE,
                          clean_duplicates = FALSE,
                          group_accepted = FALSE) {

  # turn on taxonomy levels if replacements are selected
  if(any(replace_sub_var, replace_family, replace_genus)){
    taxonomy_levels <- TRUE
  }

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
    #### final change: acc_syns3 > acc_syns2 ####
    acc_syns3 <- acc_syns2 |>
      dplyr::left_join(acc_sub_var, by = "acc_taxon_id") |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            data <- dplyr::pick(dplyr::everything())
            if (rep_col %in% names(data)) {
              dplyr::if_else(!is.na(data[[rep_col]]),
                             data[[rep_col]],
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

  if(replace_family){

    # add taxonomy levels to lookup
    acc_syns_levels <- acc_syns3 |>
      dplyr::left_join(tax_levels |>
                         dplyr::select(acc_taxon_id, acc_species, acc_genus,
                                       acc_family),
                       by = "acc_taxon_id")

    # stop if family is missing
    if(sum(is.na(acc_syns_levels$acc_family)) > 0){

      stop("Accepted taxa are missing family. Cannot replace families with species or genus.")

    }

    # single species per family in MNTaxa
    # add dlist info from species
    # rename columns
    acc_single_fam <- acc_syns_levels |>
      dplyr::filter(acc_rank %in% c("subspecies", "variety", "species",
                                    "genus")) |>
      dplyr::group_by(acc_family) |>
      dplyr::mutate(n_spp = dplyr::n_distinct(acc_species, na.rm = T),
             n_gen = dplyr::n_distinct(acc_genus, na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::filter((n_spp == 1 & !is.na(acc_species)) |
               (n_spp == 0 & n_gen == 1)) |>
      dplyr::mutate(acc_taxon_rep = dplyr::if_else(!is.na(acc_species), acc_species,
                                       acc_genus)) |>
      dplyr::distinct(acc_family, acc_taxon_rep) |>
      dplyr::rename(acc_taxon = acc_family) |>
      dplyr::left_join(acc|>
                         dplyr::select(-acc_synonymy_id) |>
                         dplyr::rename_with(.fn = ~paste(.x, "rep", sep = "_")),
                       by = "acc_taxon_rep")

# family in that have multiple families as acc matches
family_split <- acc_syns3 |>
  dplyr::filter(rank == "family") |>
  dplyr::distinct(taxon) |>
  dplyr::inner_join(acc_syns3 |>
                      dplyr::filter(acc_rank == "family")) |>
  dplyr::group_by(taxon) |>
  dplyr::mutate(n_acc = dplyr::n_distinct(acc_taxon)) |>
  dplyr::ungroup() |>
  dplyr::filter(n_acc > 1)

# family names that have switched with accepted species using old name
family_switch <- acc_syns3 |>
  dplyr::filter(rank == "family" & acc_rank == "family" & taxon != acc_taxon &
                  acc_taxon %in% acc_single_fam$acc_taxon) |>
  dplyr::distinct(taxon, acc_taxon) |>
  dplyr::rename(acc_family = taxon,
                new_family = acc_taxon) |>
  dplyr::inner_join(tax_levels |>
                      dplyr::filter(acc_rank != "family"),
                    by = "acc_family",
                    relationship = "many-to-many")


# replace the family as the acc assignment
acc_syns4 <- acc_syns3 |>
  dplyr::left_join(acc_single_fam |>
                     dplyr::filter(!(acc_taxon %in% c(family_split$acc_taxon, family_switch$new_family))),
                   by = "acc_taxon") |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ {
        rep_col <- paste0(dplyr::cur_column(), "_rep")
        data <- dplyr::pick(dplyr::everything())
        if (rep_col %in% names(data)) {
          dplyr::if_else(!is.na(data[[rep_col]]),
                         data[[rep_col]],
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

  if(replace_genus){

    # add taxonomy levels to lookup
    acc_syns_levels <- acc_syns4 |>
      dplyr::left_join(tax_levels |>
                         dplyr::select(acc_taxon_id, acc_species, acc_genus,
                                       acc_family),
                       by = "acc_taxon_id")

    # single species per genus in MNTaxa
    # add acc info from species
    # rename columns
    acc_single_gen <- acc_syns_levels |>
      dplyr::filter(acc_rank %in% c("subspecies", "variety", "species")) |>
      dplyr::group_by(acc_genus) |>
      dplyr::mutate(n_spp = dplyr::n_distinct(acc_species, na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::filter(n_spp == 1 & !is.na(acc_species)) |>
      dplyr::mutate(acc_taxon_rep = acc_taxon) |>
      dplyr::distinct(acc_genus, acc_taxon_rep) |>
      dplyr::rename(acc_taxon = acc_genus) |>
      dplyr::left_join(acc|>
                         dplyr::select(-acc_synonymy_id) |>
                         dplyr::rename_with(.fn = ~paste(.x, "rep", sep = "_")),
                       by = "acc_taxon_rep")

    # genera that have multiple genera as acc matches
    genus_split <- acc_syns4 |>
      dplyr::filter(rank == "genus") |>
      dplyr::distinct(taxon) |>
      dplyr::inner_join(acc_syns4 |>
                          dplyr::filter(acc_rank == "genus")) |>
      dplyr::group_by(taxon) |>
      dplyr::mutate(n_acc = dplyr::n_distinct(acc_taxon)) |>
      dplyr::ungroup() |>
      dplyr::filter(n_acc > 1)

    # genus-level taxon with accepted species using old name
    genus_switch <- acc_syns4 |>
      dplyr::filter(rank == "genus" & acc_rank == "genus" & taxon != acc_taxon &
                 acc_taxon %in% acc_single_gen$acc_taxon) |>
      dplyr::distinct(taxon, acc_taxon) |>
      dplyr::rename(acc_genus = taxon,
               new_genus = acc_taxon) |>
      dplyr::inner_join(tax_levels |>
                          dplyr::filter(acc_rank != "genus"),
                        by = "acc_genus",
                   relationship = "many-to-many")

    # replace the genus as the acc assignment
    acc_syns5 <- acc_syns4 |>
      dplyr::left_join(acc_single_gen |>
                         dplyr::filter(!(acc_taxon %in% c(genus_switch$new_genus,
                                            genus_split$taxon))),
                       by = "acc_taxon") |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ {
            rep_col <- paste0(dplyr::cur_column(), "_rep")
            data <- dplyr::pick(dplyr::everything())
            if (rep_col %in% names(data)) {
              dplyr::if_else(!is.na(data[[rep_col]]),
                             data[[rep_col]],
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

  if(drop_higher){

    # drop family and genera that have multiple species in MNTaxa
    # add parent
    # drop offspring if their parent species is an assignment and matches rank
    # of taxon
    acc_syns6 <- acc_syns5 |>
      dplyr::filter(acc_rank %in% c("species", "subspecies", "variety",
                               "subspecies variety")|
               acc_taxon %in% higher_include)


  }

  #### start here ####
  # add dplyr:: etc. where needed

  if(excluded_duplicates){

    # number of accepted matches per taxon
    acc_matches <- acc_syns6 |>
      group_by(taxon) |>
      mutate(n_acc = n_distinct(acc_taxon)) |>
      ungroup()

    # taxon assignments with multiple acc
    # acc assignments that are excluded
    # remove acc assignments that are the only option for a taxon
    acc_dups_exclude <- acc_matches |>
      filter(n_acc > 1) |>
      left_join(exclude2 |>
                  distinct(synonymy_id, excluded_description)) |>
      filter(!is.na(excluded_description)) |>
      select(starts_with("acc"), synonymy_id, excluded_description) |>
      distinct() |>
      anti_join(acc_matches |>
                  filter(n_acc == 1))

    # remove these excluded duplicates
    acc_syns7 <- acc_syns6 |>
      anti_join(acc_dups_exclude)


  }

  if(clean_duplicates){

    # there's a rule about sub > species, but I think it's redundant

    # if a taxon is accepted, make it's only accepted taxon itself
    # manual corrections that were developed within based on the most
    # likely taxon someone would be recording
    dlist_lookup <- dlist_syns7 %>%
      group_by(taxon) %>%
      mutate(in_dlist = if_else(taxon %in% dlist_taxon, 1, 0),
             species_in_dlist = if_else(rank %in% c("variety", "subspecies",
                                                    "subspecies variety") &
                                          word(taxon, 1, 2) %in%
                                          dlist_taxon, 1, 0)) %>%
      ungroup() %>%
      filter((in_dlist == 0 & species_in_dlist == 0) |
               (in_dlist == 1 & taxon == dlist_taxon) |
               (species_in_dlist == 1 & word(taxon, 1, 2) == dlist_taxon)) %>%
      select(-ends_with("in_dlist")) %>%
      filter(!(taxon == "Arabis holboellii" &
                 dlist_taxon == "Boechera retrofracta") &
               !(taxon == "Botrychium lunaria" &
                   dlist_taxon == "Botrychium crenulatum") &
               !(taxon == "Chamerion angustifolium subsp. angustifolium" &
                   dlist_taxon == "Eriophorum angustifolium") &
               !(taxon == "Quercus x schuettei" &
                   dlist_taxon == "Quercus x hillii"))

  }

  # return lookup table
  return(acc_syns2 |>
           tibble::as_tibble())
}
