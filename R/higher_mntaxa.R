higher_mntaxa <- function(species_only = FALSE){

  # get taxonomy info
  # format taxa names
  taxa <- taxa_mntaxa(taxonomy_levels = TRUE)

  # format accepted names
  acc <- accepted_mntaxa(
    taxonomy_levels = TRUE
  ) |>
    dplyr::rename_with(.fn = ~ paste("acc", .x, sep = "_")) |>
    dplyr::mutate(synonymy_id = acc_synonymy_id)

  # get species-level for sub-species and varieties
  # isolate subspecies/varieties with species in accepted taxa
  # repair missing taxon names with first two words (n = 2 taxa on 1/26/26)
  # repair missing IDs using taxon names (could create duplicates)
  # add all accepted info consistent with species-level
  acc_sub_var <- acc |>
    dplyr::filter(acc_rank %in% c("subspecies", "variety")) |>
    dplyr::transmute(
      acc_taxon_id,
      acc_taxon = dplyr::coalesce(acc_parent_taxon,
                                  stringr::word(acc_taxon, 1, 2)),
      acc_taxon_id_rep = dplyr::if_else(
        acc_parent_id %in% acc$acc_taxon_id,
        acc_parent_id,
        NA_real_)) |>
    dplyr::left_join(acc |>
                       dplyr::select(acc_taxon,
                                     temp_taxon_id = acc_taxon_id),
                     by = "acc_taxon") |>
    dplyr::mutate(acc_taxon_id_rep = dplyr::coalesce(
      acc_taxon_id_rep, temp_taxon_id)) |>
    dplyr::select(-temp_taxon_id) |>
    dplyr::left_join(acc |>
                       dplyr::select(-synonymy_id) |>
                       dplyr::rename(synonymy_id = acc_synonymy_id),
                     by = c("acc_taxon", "acc_taxon_id_rep" = "acc_taxon_id")
    ) |>
    dplyr::rename_with(~paste0(.x, "_rep"),
                       -starts_with("acc_taxon_id"))

  # stop if only species is needed
  if(species_only){
    # return table
    return(acc_sub_var)

    # stop function
    stop()
  }

  # format parents tables
  pars <- pars_raw %>%
    left_join(taxa %>%
                select(taxon_id, rank)) %>%
    left_join(taxa %>%
                select(taxon_id, rank) %>%
                rename(parent_id = taxon_id,
                       parent_rank = rank)) %>%
    mutate(rank = if_else(rank == "division", "phylum", rank),
           parent_rank = if_else(parent_rank == "division", "phylum",
                                 parent_rank)) %>%
    filter(!is.na(rank))

  # create temporary table for expansion
  pars_temp <- pars %>%
    transmute(taxon_id, rank,
              upstream_id   = parent_id,
              upstream_rank = parent_rank)

  # start parent expansion
  pars2 <- pars_temp

  # loop pthrough pars table
  while(any(!is.na(pars_temp$upstream_id))) {

    # overwrite pars_temp with next taxonomy levels
    # add next level
    pars_temp <- pars_temp %>%
      left_join(pars %>%
                  select(parent_id = taxon_id,
                         parent_rank  = rank,
                         grandparent_id = parent_id,
                         grandparent_rank  = parent_rank),
                by = c("upstream_id" = "parent_id")) %>%
      transmute(taxon_id, rank,
                upstream_id   = grandparent_id,
                upstream_rank = grandparent_rank)

    pars2 <- bind_rows(pars2, pars_temp)

  }

  # remove missing values
  pars3 <- pars2 %>%
    filter(!is.na(upstream_id)) %>%
    distinct() %>%
    left_join(taxa %>%
                select(taxon_id, taxon, hybrid)) %>%
    left_join(taxa %>%
                select(taxon_id, taxon, hybrid) %>%
                rename(upstream_id = taxon_id,
                       upstream_taxon = taxon,
                       upstream_hybrid = hybrid)) |>
    filter(!(taxon_id == 68535 & upstream_id == 52071 &
               upstream_rank == "genus"))

  # duplicates
  pars_dups <- janitor::get_dupes(pars3, taxon_id, upstream_rank)

  # error if there are multiple upstream taxa
  if(nrow(pars_dups) > 0){

    # get taxa
    taxa_report <- unique(pars_dups$taxon)

    # stop process
    stop(paste("Error: please request upstream taxa are resolved for:",
               paste(taxa_report, collapse = ", ")))

  }

  # select pars for accepted taxa
  # assign genus to all at genus or below
  acc_pars <- pars3 %>%
    select(acc_taxon_id = taxon_id, upstream_taxon, upstream_rank) %>%
    filter(acc_taxon_id %in% acc$acc_taxon_id)%>%
    tidyr::pivot_wider(names_from = upstream_rank,
                values_from = upstream_taxon) %>%
    full_join(acc %>%
                distinct(acc_taxon_id, acc_taxon, acc_ss_sl, acc_rank,
                         acc_hybrid, synonymy_id),
              by = "acc_taxon_id") %>%
    left_join(acc_sub_var |>
                select(acc_taxon_id, acc_taxon_rep),
              by = "acc_taxon_id") |>
    mutate(species = case_when(acc_rank == "species" ~ acc_taxon,
                               acc_rank %in% c("subspecies", "variety") ~
                                 acc_taxon_rep,
                               TRUE ~ NA_character_),
           genus = case_when(
             acc_rank %in% c("species", "subspecies", "variety", "genus") &
               word(acc_taxon, 1) != "x" ~ word(acc_taxon, 1),
             acc_rank %in% c("species", "subspecies", "variety", "genus") &
               word(acc_taxon, 1) == "x" ~ word(acc_taxon, 2, 2),
             TRUE ~ NA_character_),
           family = if_else(acc_rank == "family", acc_taxon, family)) |>
    select(-acc_taxon_rep)

  # get upstream for all genera
  genus_upstream <- acc_pars %>%
    filter(!is.na(genus) & (!is.na(family) | !is.na(order) | !is.na(class) |
                              !is.na(phylum) | !is.na(kingdom))) %>%
    distinct(genus, family, order, class, phylum, kingdom) %>%
    group_by(genus) %>%
    mutate(n_vals = n(),
           order_pres = sum(!is.na(order)) > 0) %>%
    ungroup() %>%
    filter((n_vals == 1 |
              (n_vals > 1 & order_pres == T & !is.na(order)) |
              (genus == "Aureolaria" & family == "Orobanchaceae")) &
             !(genus == "Scrophularia" & order == "Scrophulariales")) %>%
    select(-c(n_vals, order_pres))

  # get upstream of all families
  family_upstream <- acc_pars %>%
    filter(!is.na(family) & (!is.na(order) | !is.na(class) |
                               !is.na(phylum) | !is.na(kingdom))) %>%
    distinct(family, order, class, phylum, kingdom) %>%
    group_by(family) %>%
    mutate(n_vals = n(),
           order_pres = sum(!is.na(order)) > 0) %>%
    ungroup() %>%
    filter(n_vals == 1 |
             (n_vals > 1 & family == "Scrophulariaceae" & order == "Lamiales") |
             (n_vals > 1 & family == "Rubiaceae" & order == "Rubiales")) %>%
    select(-c(n_vals, order_pres))

  # standarize with upstream genus and family
  acc_pars2 <- acc_pars %>%
    left_join(genus_upstream %>%
                rename_with(~paste0(.x, "_rep"), -genus)) %>%
    mutate(family = if_else(!is.na(family_rep), family_rep, family),
           order = if_else(!is.na(order_rep), order_rep, order),
           class = if_else(!is.na(class_rep), class_rep, class),
           phylum = if_else(!is.na(phylum_rep), phylum_rep, phylum),
           kingdom = if_else(!is.na(kingdom_rep), kingdom_rep, kingdom)) %>%
    select(-ends_with("rep")) %>%
    left_join(family_upstream %>%
                rename_with(~paste0(.x, "_rep"), -family)) %>%
    mutate(order = if_else(!is.na(order_rep), order_rep, order),
           class = if_else(!is.na(class_rep), class_rep, class),
           phylum = if_else(!is.na(phylum_rep), phylum_rep, phylum),
           kingdom = if_else(!is.na(kingdom_rep), kingdom_rep, kingdom)) %>%
    select(-ends_with("rep"))

  # accepted taxa that need parents
  # exclude taxa in pars4 that have family
  acc_need_par <- acc_pars2 %>%
    filter(is.na(family) | is.na(order) | is.na(class) |
             is.na(phylum) | is.na(kingdom)) %>%
    distinct(acc_taxon_id, acc_taxon, acc_rank, acc_hybrid) %>%
    mutate(itis_name = if_else(!is.na(acc_hybrid),
                               acc_hybrid %>%
                                 str_replace_all("×", "X ") %>%
                                 str_replace_all("X  ", "X "),
                               acc_taxon) %>%
             str_replace("subsp.", "ssp.") %>%
             str_replace("\  ", "\ "),
           gbif_name = coalesce(acc_hybrid, acc_taxon),
           wfo_name = coalesce(acc_hybrid, acc_taxon))

  # download databases
  taxizedb::db_download_itis(overwrite = FALSE)
  taxizedb::db_download_wfo(overwrite = FALSE)
  taxizedb::db_download_gbif(overwrite = FALSE)

  # convert to taxon IDs for ITIS
  acc_itis_ids <- taxizedb::name2taxid(acc_need_par$itis_name, db = "itis",
                               out_type = "summary") %>%
    rename(itis_name = name,
           itis_id = id)

  #### make each database wide and clean up before searching next? ####

  # get parents
  acc_itis_pars <- taxizedb::classification(acc_itis_ids$itis_id, db = "itis") %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = "itis_id") %>%
    mutate(itis_id = as.numeric(itis_id)) %>%
    rename(upstream_taxon = name,
           upstream_rank = rank) %>%
    left_join(acc_itis_ids, by = "itis_id") |>
    left_join(acc_need_par |>
                select(itis_name, starts_with("acc_")),
              by = "itis_name") |>
    select(starts_with("acc_"), upstream_taxon, upstream_rank)

  # ids without classification
  acc_need_par2 <- acc_need_par %>%
      anti_join(acc_itis_pars)

  # try missing with WFO
  acc_wfo_ids <- taxizedb::name2taxid(acc_need_par2$wfo_name, db = "wfo",
                               out_type = "summary") %>%
      rename(wfo_name = name,
             wfo_id = id)

  # get parents
  acc_wfo_pars <- taxizedb::classification(acc_wfo_ids$wfo_id, db = "wfo") %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = "wfo_id") %>%
    filter(!is.na(name)) %>% # taxa with no classification
    rename(upstream_taxon = name,
           upstream_rank = rank) %>%
    left_join(acc_wfo_ids, by = "wfo_id") |>
    left_join(acc_need_par |>
                select(wfo_name, starts_with("acc_")),
              by = "wfo_name") |>
    select(starts_with("acc_"), upstream_taxon, upstream_rank)

  # ids without classification
  acc_need_par3 <- acc_need_par2 %>%
      anti_join(acc_wfo_pars) %>%
      inner_join(acc_need_par)

  # try missing with GBIF
  acc_gbif_ids <- taxizedb::name2taxid(acc_need_par3$gbif_name, db = "gbif",
                                out_type = "summary") %>%
      rename(gbif_name = name,
             gbif_id = id)

  # get parents
  acc_gbif_pars <- taxizedb::classification(acc_gbif_ids$gbif_id, db = "gbif") %>%
      lapply(as.data.frame) %>%
      bind_rows(.id = "gbif_id") %>%
      rename(upstream_taxon = name,
             upstream_rank = rank) %>%
      left_join(acc_gbif_ids, by = "gbif_id") |>
    left_join(acc_need_par |>
                select(gbifo_name, starts_with("acc_")),
              by = "gbif_name") |>
    select(starts_with("acc_"), upstream_taxon, upstream_rank) %>%
      mutate(upstream_taxon = case_when(
        upstream_rank == "species" ~ word(upstream_taxon, 1, 2), # remove authors
        upstream_rank == "genus" ~ word(upstream_taxon, 1, 1)))


  #### start here ####

  # missing all info
  # add in manually what's possible
  gbif_no_class <- wfo_no_class %>%
    distinct(acc_taxon) %>%
    anti_join(acc_gbif_pars2) %>%
    left_join(acc_need_par) %>%
    select(starts_with("acc")) %>%
    left_join(acc_pars2)
  # 21

  # combine taxonomy and make wide
  acc_db_pars <- acc_itis_pars2 %>%
    select(starts_with("acc"), upstream_taxon, upstream_rank) %>%
    mutate(taxonomy_source_rep = "ITIS") %>%
    full_join(acc_wfo_pars2 %>%
                select(starts_with("acc"), upstream_taxon, upstream_rank) %>%
                mutate(taxonomy_source_rep = "WFO")) %>%
    full_join(acc_gbif_pars2 %>%
                select(starts_with("acc"), upstream_taxon, upstream_rank) %>%
                mutate(taxonomy_source_rep = "GBIF")) %>%
    filter(upstream_rank %in% c("species", "genus", "family", "order", "class",
                                "division", "phylum", "kingdom")) %>%
    mutate(upstream_rank = str_replace(upstream_rank, "division", "phylum") %>%
             paste0(., "_rep")) %>%
    pivot_wider(names_from = upstream_rank, values_from = upstream_taxon) %>%
    mutate(genus_rep = if_else(acc_rank == "genus", acc_taxon, genus_rep),
           genus_rep = if_else(genus_rep != word(acc_taxon) & # comment out this line to see wrong genus entries
                                 acc_rank != "family",
                               word(acc_taxon, 1, 1), genus_rep),
           family_rep = if_else(acc_rank == "family", acc_taxon, family_rep))

  # check for wrong genus
  filter(acc_db_pars, genus_rep != word(acc_taxon) & acc_rank != "family")

  # still missing info
  db_no_class <- acc_db_pars %>%
    filter(is.na(family_rep) | is.na(order_rep) | is.na(class_rep) |
             is.na(phylum_rep) | is.na(kingdom_rep)) %>%
    rename_with(~str_remove(.x, "_rep"))

  # combine no class
  # correct two entries with only genus, which don't match the species
  all_no_class <- gbif_no_class %>%
    full_join(db_no_class) %>%
    select(acc_id, acc_taxon, acc_rank, species, genus, family, order,
           class, phylum, kingdom) %>%
    left_join(acc_syns2 %>%
                distinct(acc_id, synonymy_id) %>%
                left_join(exclude2 %>%
                            distinct(synonymy_id, excluded_description))) %>%
    mutate(genus = if_else(acc_rank != "family", word(acc_taxon, 1, 1),
                           NA_character_))
  all_no_class %>%
    group_by(acc_rank) %>%
    summarize(across(.cols = c(family, order, class, phylum, kingdom),
                     .fns = ~sum(is.na(.x))))

  # minimum units to fill in
  genus_itis_ids <- all_no_class %>%
    filter(is.na(family)) %>%
    pull(genus) %>%
    unique() %>%
    name2taxid(db = "itis", out_type = "summary") %>%
    rename(genus = name)

  family_itis_ids <- all_no_class %>%
    filter(!is.na(family)) %>%
    pull(family) %>%
    unique() %>%
    name2taxid(db = "itis", out_type = "summary") %>%
    rename(family = name)

  # get values from ITIS
  genus_itis_pars <- classification(genus_itis_ids$id, db = "itis")
  family_itis_pars <- classification(family_itis_ids$id, db = "itis")

  # format lists into dataframe
  # format these separately because they'll be added on afterwards
  # use GBIF to manually fill remaining
  genus_itis_pars2 <- genus_itis_pars %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = "query") %>%
    as_tibble() %>%
    mutate(id = as.numeric(query)) %>%
    select(-query) %>%
    rename(upstream_taxon = name,
           upstream_rank = rank) %>%
    left_join(genus_itis_ids) %>%
    filter(upstream_rank %in% c("family", "order", "class", "division",
                                "kingdom")) %>%
    mutate(upstream_rank = str_replace(upstream_rank, "division", "phylum") %>%
             paste0(., "_rep")) %>%
    pivot_wider(names_from = upstream_rank, values_from = upstream_taxon) %>%
    full_join(all_no_class %>%
                filter(is.na(family)) %>%
                select(genus, acc_taxon, acc_id, acc_rank),
              relationship = "many-to-many") %>%
    mutate(taxonomy_source_rep = "ITIS",
           genus_rep = genus) %>%
    select(starts_with("acc"), ends_with("rep"))

  family_itis_pars2 <- family_itis_pars %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = "query") %>%
    as_tibble() %>%
    mutate(id = as.numeric(query)) %>%
    select(-query) %>%
    rename(upstream_taxon = name,
           upstream_rank = rank) %>%
    left_join(family_itis_ids) %>%
    filter(upstream_rank %in% c("order", "class", "division", "kingdom")) %>%
    mutate(upstream_rank = str_replace(upstream_rank, "division", "phylum") %>%
             paste0(., "_rep")) %>%
    pivot_wider(names_from = upstream_rank, values_from = upstream_taxon) %>%
    full_join(all_no_class %>%
                filter(!is.na(family)) %>%
                select(family, acc_taxon, acc_id, acc_rank),
              relationship = "many-to-many") %>%
    mutate(taxonomy_source_rep = "ITIS",
           family_rep = family) %>%
    select(starts_with("acc"), ends_with("rep"))

  # add new taxonomy to acc
  # manually add by looking up first available match in dataset
  acc_pars3 <- acc_pars2 %>%
    mutate(taxonomy_source = "MNTaxa") %>%
    left_join(acc_db_pars) %>%
    mutate(species = if_else(!is.na(species_rep), species_rep, species),
           genus = if_else(!is.na(genus_rep), genus_rep, genus),
           family = if_else(!is.na(family_rep), family_rep, family),
           order = if_else(!is.na(order_rep), order_rep, order),
           class = if_else(!is.na(class_rep), class_rep, class),
           phylum = if_else(!is.na(phylum_rep), phylum_rep, phylum),
           kingdom = if_else(!is.na(kingdom_rep), kingdom_rep, kingdom),
           taxonomy_source = if_else(!is.na(taxonomy_source_rep),
                                     taxonomy_source_rep, taxonomy_source)) %>%
    select(-ends_with("rep")) %>%
    left_join(genus_itis_pars2) %>%
    mutate(family = if_else(!is.na(family_rep), family_rep, family),
           order = if_else(!is.na(order_rep), order_rep, order),
           class = if_else(!is.na(class_rep), class_rep, class),
           phylum = if_else(!is.na(phylum_rep), phylum_rep, phylum),
           kingdom = if_else(!is.na(kingdom_rep), kingdom_rep, kingdom),
           taxonomy_source = if_else(!is.na(taxonomy_source_rep),
                                     taxonomy_source_rep, taxonomy_source)) %>%
    select(-ends_with("rep")) %>%
    left_join(family_itis_pars2) %>%
    mutate(order = if_else(!is.na(order_rep), order_rep, order),
           class = if_else(!is.na(class_rep), class_rep, class),
           phylum = if_else(!is.na(phylum_rep), phylum_rep, phylum),
           kingdom = if_else(!is.na(kingdom_rep), kingdom_rep, kingdom),
           taxonomy_source = if_else(!is.na(taxonomy_source_rep),
                                     taxonomy_source_rep, taxonomy_source)) %>%
    select(-ends_with("rep")) %>%
    mutate(family = case_when(acc_taxon == "Chamaesyce" ~ "Euphorbiaceae",
                              TRUE ~ family),
           order = case_when(acc_taxon == "Chamaesyce" ~ "Euphorbiales",
                             acc_taxon == "Belonia" ~ "Gyalectales",
                             acc_taxon %in% c("Lychnothamnus", "Nitellopsis") ~
                               "Charales",
                             acc_taxon %in% c("Viscaceae", "Comandraceae") ~
                               "Santalales",
                             acc_taxon %in% c("Najadaceae",
                                                "Zannichelliaceae") ~
                               "Alismatales",
                             acc_taxon %in% c("Mimosaceae",
                                                "Caesalpiniaceae") ~ "Fabales",
                             acc_taxon == "Hippocastanaceae" ~ "Sapindales",
                             family == "Aquifoliaceae" ~ "Celastrales",
                             family %in% c("Boraginaceae", "Oleaceae") ~
                               "Lamiales",
                             family %in% c("Hydrocharitaceae",
                                           "Potamogetonaceae") ~ "Alismatales",
                             family == "Menyanthaceae" ~ "Asterales",
                             family == "Rubiaceae" ~ "Rubiales",
                             TRUE ~ order),
           class = case_when(acc_taxon == "Chamaesyce" ~ "Dicotyledoneae",
                             order %in% c("Caryophyllales", "Rosales",
                                          "Celastrales", "Asterales",
                                          "Lamiales", "Solanales",
                                          "Rubiales", "Apiales", "Ericales",
                                          "Gentianales", "Myrtales",
                                          "Geraniales") ~
                               "Dicotyledoneae",
                             acc_taxon == "Belonia" ~ "Lecanoromycetes",
                             acc_taxon %in% c("Lychnothamnus", "Nitellopsis") ~
                               "Charophyceae",
                             acc_taxon %in% c("Viscaceae", "Comandraceae") ~
                               "Magnoliopsida",
                             order == "Alismatales" ~ "Monocotyledoneae",
                             order == "Fabales" ~ "Dicotyledoneae",
                             acc_taxon == "Hippocastanaceae" ~ "Magnoliopsida",
                             order == "Salviniales" ~ "Filicopsida",
                             TRUE ~ class),
           phylum = case_when(acc_taxon == "Chamaesyce" ~ "Anthophyta",
                              order %in% c("Caryophyllales", "Fabales",
                                           "Alismatales", "Rosales",
                                           "Celastrales", "Asterales",
                                           "Lamiales", "Solanales",
                                           "Rubiales", "Apiales", "Ericales",
                                           "Gentianales", "Myrtales",
                                           "Geraniales") ~
                                "Anthophyta",
                              acc_taxon == "Belonia" ~ "Ascomycota",
                              acc_taxon %in% c("Lychnothamnus", "Nitellopsis") ~
                                "Charophyta",
                              acc_taxon %in% c("Viscaceae",
                                                 "Comandraceae",
                                                 "Hippocastanaceae") ~
                                "Tracheophyta",
                              class == "Polypodiopsida" ~ "Tracheophyta",
                              order == "Salviniales" ~ "Filicinophyta",
                              class == "Lycopodiopsida" ~ "Lycophyta",
                              TRUE ~ phylum),
           kingdom = case_when(acc_taxon %in% c("Chamaesyce", "Lychnothamnus",
                                                  "Nitellopsis", "Viscaceae",
                                                  "Najadaceae",
                                                  "Zannichelliaceae",
                                                  "Comandraceae",
                                                  "Mimosaceae",
                                                  "Hippocastanaceae",
                                                  "Caesalpiniaceae") ~ "Plantae",
                               acc_taxon %in% c("Belonia") ~ "Fungi",
                               TRUE ~ kingdom),
           taxonomy_source = case_when(acc_taxon %in% c("Chamaesyce",
                                                          "Lychnothamnus",
                                                          "Nitellopsis") ~
                                         "MNTaxa",
                                       order %in% c("Caryophyllales",
                                                    "Alismatales",
                                                    "Fabales", "Rosales",
                                                    "Celastrales", "Asterales",
                                                    "Lamiales", "Solanales",
                                                    "Rubiales", "Apiales",
                                                    "Ericales", "Gentianales",
                                                    "Myrtales", "Salviniales",
                                                    "Geraniales") ~
                                         "MNTaxa",
                                       acc_taxon %in% c("Belonia", "Viscaceae",
                                                          "Najadaceae",
                                                          "Zannichelliaceae",
                                                          "Comandraceae") ~
                                         "GBIF",
                                       acc_taxon == "Hippocastanaceae" ~ "ITIS",
                                       TRUE ~ taxonomy_source))

  # check for complete
  acc_pars3 %>%
    filter(is.na(family) | is.na(order) | is.na(class) |
             is.na(phylum) | is.na(kingdom)) %>%
    left_join(syn_phys2)
  # remaining are family-level or genus non-plants

  # check that all have same upstream
  acc_pars3 %>%
    filter(!is.na(genus)) %>%
    distinct(genus, family, order, class, phylum, kingdom) %>%
    get_dupes(genus)

  acc_pars3 %>%
    distinct(family, order, class, phylum, kingdom) %>%
    get_dupes(family)

  acc_pars3 %>%
    distinct(order, class, phylum, kingdom) %>%
    get_dupes(order)

  acc_pars3 %>%
    distinct(class, phylum, kingdom) %>%
    get_dupes(class)

  acc_pars3 %>%
    distinct(phylum, kingdom) %>%
    get_dupes(phylum)

  # rename columns
  acc_pars4 <- acc_pars3 %>%
    rename_with(.cols = c(species, genus, family, order, class, phylum,
                          kingdom),
                .fn = ~paste0("acc_", .x))

  # check for duplicates
  get_dupes(acc_pars4, acc_id)

  # add extra info and save
  acc_pars4 %>%
    left_join(syn_phys2 %>%
                rename(acc_physiognomy = physiognomy)) %>%
    left_join(syn_comms %>%
                rename(acc_common_name = common_name)) %>%
    left_join(syn_cvals) %>%
    left_join(syn_or2 %>%
                rename(acc_origin = native_status)) %>%
    write_csv(paste0("intermediate-data/mntaxa_acc_parents_",
                     str_remove_all(Sys.Date(), "-"), ".csv"))


  # itis removals
  # filter(!(id %in% c(170497, 997710, 98432, 51253, 162209, 849103))) # remove animals
  # gbif removals
  # filter(!(id %in% c(11056316, 7611998, 3237687, 7357191, 7824404, 7408099)))) # remove animal and plants with multiple authorities (Selected one with the most MN records on GBIF)

  #### gbif code written today ####

  # get parents
  acc_gbif_pars <- taxizedb::classification(acc_gbif_ids$gbif_id, db = "gbif")

  # format lists into dataframe
  acc_gbif_pars2 <- acc_gbif_pars %>%
      lapply(as.data.frame) %>%
      bind_rows(.id = "gbif_id") %>%
      transmute(gbif_id = as.numeric(gbif_id),
                upstream_taxon = name,
             upstream_rank = rank) %>%
      left_join(acc_gbif_ids, by = "gbif_id") %>%
      left_join(acc_need_par %>%
                  select(gbif_name, starts_with("acc_")),
                by = "gbif_name") %>%
      mutate(upstream_taxon = case_when(
        upstream_rank == "species" ~ word(upstream_taxon, 1, 2), # remove authors
        upstream_rank == "genus" ~ word(upstream_taxon, 1, 1),
        TRUE ~ upstream_taxon))

  # animals
  gbif_not_plants <- acc_gbif_pars2 |>
    filter(upstream_rank == "kingdom" &
             (is.na(upstream_taxon) | upstream_taxon != "Plantae")) |>
    pull(gbif_id)

  # mismatches
  gbif_mismatches <- acc_gbif_pars2 |>
    filter((acc_rank == upstream_rank & acc_taxon != upstream_taxon) |
             (acc_rank == "family" &
                upstream_rank == "genus") |
             (acc_rank %in% c("family", "genus") &
                upstream_rank == "species"))

  # remove non-plants
  # remove species and genus mismatches
  # pivot wider
  # remove mismatches with species and genus
  # remove upstream that don't match MNTaxa
  acc_gbif_pars3 <- acc_gbif_pars2 |>
    filter(!(gbif_id %in% gbif_not_plants)) |>
    anti_join(gbif_mismatches) |>
    tidyr::pivot_wider(names_from = upstream_rank,
                       values_from = upstream_taxon) |>
    select(-gbif_id) |>
    distinct() |>
    filter(!(acc_rank %in% c("subspecies", "variety", "species") &
               species != word(acc_taxon, 1, 2)) &
             !(acc_rank %in% c("subspecies", "variety", "species", "genus") &
                 genus != word(acc_taxon, 1))) |>
    left_join(genus_upstream |>
                select(genus, family_check = family),
              by = "genus") |>
    filter(is.na(family_check) | family == family_check) |>
    left_join(family_upstream |>
                select(family, order_check = order),
              by = "family") |>
    filter(is.na(order_check) | order == order_check) |>
    group_by(across(c(gbif_name, starts_with("acc_")))) |>
    summarize(species = first(species, na_rm = T),
              genus = first(genus, na_rm = T),
              family = first(family, na_rm = T),
              order = first(order, na_rm = T),
              class = first(class, na_rm = T),
              phylum = first(phylum, na_rm = T),
              kingdom = first(kingdom, na_rm = T),
              .groups = "drop")

}
