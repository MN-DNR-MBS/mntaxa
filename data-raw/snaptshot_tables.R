#### overview ####
# take a snapshot of all tables

# load library
load_all()

# load all tables
load_mntaxa(
  synonymies = TRUE,
  all_taxa = TRUE,
  taxonomy_levels = TRUE,
  sources = TRUE,
  phys = TRUE,
  origin = TRUE,
  common = TRUE,
  cvals = TRUE,
  exclude = TRUE,
  source = "database",
  envir = .GlobalEnv
)

# taxa_raw: used in taxa_mntaxa()
# id, taxon, is_hybrid, hybrid_parents, rank_id, author_id, publication_id
# retained alongside begin_date/end_date/created_at/updated_at because
# taxa_mntaxa() drops them explicitly — they must be present to be dropped
taxa_raw <- taxa_raw |>
  dplyr::select(
    id,
    taxon,
    is_hybrid,
    hybrid_parents,
    rank_id,
    author_id,
    publication_id,
    begin_date,
    end_date,
    created_at,
    updated_at
  )

# syns_raw: used in accepted_mntaxa() and lookup_mntaxa()
syns_raw <- syns_raw |>
  dplyr::select(taxon_id, synonymy_id, d_list_beg_date, d_list_end_date)

# pars_raw: used in taxa_mntaxa() (taxonomy_levels branch)
pars_raw <- pars_raw |>
  dplyr::select(taxon_id, parent_id)

# rank_raw: used in taxa_mntaxa() (taxonomy_levels branch)
rank_raw <- rank_raw |>
  dplyr::select(id, rank)

# auth_raw: used in taxa_mntaxa() (sources branch)
auth_raw <- auth_raw |>
  dplyr::select(id, author)

# pubs_raw: used in taxa_mntaxa() (sources branch)
pubs_raw <- pubs_raw |>
  dplyr::select(id, title)

# phys_codes_raw: used in accepted_mntaxa() (phys branch)
phys_codes_raw <- phys_codes_raw |>
  dplyr::select(id, physiognomy, physiognomy_code)

# syn_phys_raw: used in accepted_mntaxa() (phys branch)
syn_phys_raw <- syn_phys_raw |>
  dplyr::select(synonymy_id, physiognomy_id, is_acceptable)

# origin_codes_raw: used in accepted_mntaxa() (origin branch)
origin_codes_raw <- origin_codes_raw |>
  dplyr::select(id, native_status)

# syn_or_raw: used in accepted_mntaxa() (origin branch)
syn_or_raw <- syn_or_raw |>
  dplyr::select(id, native_status_id)

# syn_comm_raw: used in accepted_mntaxa() (common branch)
syn_comm_raw <- syn_comm_raw |>
  dplyr::select(synonymy_id, common_name)

# syn_cvals_raw: used in accepted_mntaxa() (cvals branch)
syn_cvals_raw <- syn_cvals_raw |>
  dplyr::select(synonymy_id,
                conservationism_coefficient,
                end_date,
                publication_id)

# syn_exclude_raw: used in accepted_mntaxa() (exclude branch)
syn_exclude_raw <- syn_exclude_raw |>
  dplyr::select(synonymy_id, begin_date, end_date, excluded_reason_id)

# exclude_codes_raw: used in accepted_mntaxa() (exclude branch)
exclude_codes_raw <- exclude_codes_raw |>
  dplyr::select(id, excluded_reason_code, description)

# save all tables as internal package data
usethis::use_data(
  taxa_raw,
  syns_raw,
  pars_raw,
  rank_raw,
  auth_raw,
  pubs_raw,
  phys_codes_raw,
  syn_phys_raw,
  origin_codes_raw,
  syn_or_raw,
  syn_comm_raw,
  syn_cvals_raw,
  syn_exclude_raw,
  exclude_codes_raw,
  internal  = FALSE,
  overwrite = TRUE
)
