ameacadasPR <- readr::read_rds("listas/ameacadasPR.rds")
ameacadasSC <- readr::read_rds("listas/ameacadasSC.rds")
ameacadasRS <- readr::read_rds("listas/ameacadasRS.rds")

port443 <- readxl::read_xlsx(
  "listas/especiesportaria443.xlsx",
  sheet = "especiesportaria443"
)

nome <- flora::get.taxa("Ocotea porosa")$search.str

port443_spp <-
  port443 |>
  dplyr::filter(!is.na(categoria)) |>
  dplyr::mutate(
    nome.oiginal = stringr::word(taxon, 1, 2),
    nome.BFG = flora::get.taxa(nome.oiginal)$search.str
  ) |>
  dplyr::rename(threat.status.mma14 = categoria) |>
  dplyr::select(nome.oiginal, nome.BFG, threat.status.mma14)

readr::write_rds(port443_spp, "listas/port443.rds")

status_cnc <- readr::read_csv("listas/CNCFLORA-Lista-Taxon-03072025.csv") |>
  dplyr::mutate(
    original.search = stringr::word(`Nome Científico`, 1, 2),
    nome.BFG = stringr::word(`Nome Científico`, 1, 2)
  ) |>
  dplyr::select(
    original.search,
    nome.BFG,
    threat.status.cnc = `Categoria de Risco`
  )

readr::write_rds(status_cnc, "listas/status_cnc.rds")

nomes_BFG <- flora::get.taxa(
  c(
    "Quillaja brasiliensis",
    "Ocotea porosa",
    "Trema micrantha"
  ),
  drop = c("threat.status.cnc", "threat.status.mma")
)


library(magrittr)

comparaTudo <- purrr::map2_df(
  list(dplyr::filter(nomes_BFG, !is.na(search.str))),
  list(
    ameacadasPR,
    ameacadasSC,
    ameacadasRS,
    port443,
    status_cnc
  ),
  dplyr::inner_join,
  by = c("search.str" = "nome.BFG")
) %>%
  dplyr::group_by(original.search) %>%
  dplyr::summarise(dplyr::across(
    .cols = c(pr, sc, rs, threat.status.mma14, threat.status.cnc),
    ~ toString(na.omit(.x))
  )) %>%
  dplyr::transmute(dplyr::across(
    .cols = dplyr::everything(),
    list(~ dplyr::na_if(., ""))
  )) %>%
  dplyr::mutate(dplyr::across(
    .cols = -original.search_1,
    ~ stringr::str_replace(., "([A-Z]{2})(,\\s[A-Z]{2})+", "\\1")
  )) %>%
  dplyr::rename_with(~ gsub("_1", "", .x, fixed = TRUE))

# Dividiu-se em dois objetos (comparaTudo e compara2)
# apenas para facilitar o entendimento.
compara2 <-
  dplyr::left_join(
    nomes_BFG,
    comparaTudo,
    by = "original.search"
  ) %>%
  dplyr::distinct(original.search, .keep_all = TRUE) %>%
  dplyr::mutate(
    nome_correto = dplyr::case_when(
      notes == "was misspelled" ~ "grafia incorreta",
      notes == "replaced synonym" ~ "sinônimo",
      notes == "not found" ~ "não encontrado",
      notes == "was misspelled|replaced synonym" ~ "grafia incorreta, sinônimo", 
      TRUE ~ "correto"
    )
  ) %>%
  dplyr::mutate(
    nome_aceito = dplyr::if_else(
      nome_correto == "correto",
      "\U2714",
      search.str
    ),
    dplyr::across(
      .cols = original.search,
      .fns = \(sp) stringr::str_to_sentence(sp)
    )
  ) %>%
  dplyr::select(
    familia = family,
    especie = original.search,
    cncflora = threat.status.cnc,
    mma14 = threat.status.mma14,
    mma22 = threat.status.mma2022,
    pr,
    sc,
    rs,
    nome_correto,
    nome_aceito
  )
