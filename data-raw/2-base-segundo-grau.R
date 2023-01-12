# Vamos baixar dados CPOSG agora, para obter
# informações de acórdãos e agravos de instrumento.

# Estratégia:
# 1) pesquisar todos os processos no CPOSG e parsear
# 2) filtrar as apelações e agravos
# 3) verificar quantos casos faltam e o que fazer com eles.
# 4) estudar o problema das cautelares


# download ----------------------------------------------------------------

path_cposg <- "data-raw/cposg"

purrr::walk(
  da_tidy$id,
  lex::tjsp_cposg_download,
  dir = path_cposg,
  .progress = TRUE
)


# source("data-raw/tjsp_cposg_parse.R")

# parse -------------------------------------------------------------------

da_cposg <- fs::dir_ls(path_cposg) |>
  purrr::map(lex::tjsp_cposg_parse, .progress = TRUE) |>
  purrr::list_rbind(names_to = "file") |>
  dplyr::filter(!is.na(classe))

readr::write_rds(da_cposg, "data-raw/da_cposg.rds", compress = "xz")

# filter ------------------------------------------------------------------

da_cposg |>
  dplyr::count(classe, sort = TRUE)

da_cposg_filter <- da_cposg |>
  dplyr::filter(classe %in% c(
    "Apelação Cível", "Agravo de Instrumento"
  )) |>
  dplyr::mutate(id = stringr::str_extract(
    tools::file_path_sans_ext(basename(file)), "[0-9]+"
  ))

da_cposg_tem_decisao <- da_cposg_filter |>
  dplyr::filter(purrr::map_lgl(decisoes, \(x) nrow(x) > 0))

# verificacoes ------------------------------------------------------------

## Existem casos que são processos duplos.

# da_tidy |>
#   dplyr::anti_join(da_cposg_filter, "id") |>
#   dplyr::select(id) |>
#   dplyr::slice_sample(n = 10)

# dos demais, podemos assumir que não tem decisão de apelação.

# dos 173 casos encontrados, 132 tem a decisão da apelação ou agravo

da_tidy |>
  dplyr::inner_join(da_cposg_tem_decisao, "id") |>
  dplyr::count(categoria, classe.y, sort = TRUE)

# cautelares --------------------------------------------------------------


da_cposg_filter$decisoes[[2]]

rx_negaram <- stringr::regex("negaram", TRUE)
rx_em_parte <- stringr::regex("em parte|parcialmente", TRUE)
rx_aceitaram <- stringr::regex("provimento|anularam a sentença", TRUE)
rx_prejud <- stringr::regex("prejudic|n[aã]o conheceram|reexame", TRUE)

aux_decisoes <- da_cposg_filter |>
  dplyr::select(id, decisoes) |>
  tidyr::unnest(decisoes) |>
  dplyr::arrange(dplyr::desc(data)) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::transmute(id, dec = dplyr::case_when(
    stringr::str_detect(decisao, rx_negaram) ~ "Improvido",
    stringr::str_detect(decisao, rx_em_parte) ~ "Provido em parte",
    stringr::str_detect(decisao, rx_aceitaram) ~ "Provido",
    stringr::str_detect(decisao, rx_prejud) ~ "Prejudicado / Não conhecido / Reexame",
    TRUE ~ NA_character_
  ))

da_cposg_tidy <- da_cposg_filter |>
  dplyr::transmute(
    id,
    classe,
    tem_decisao = purrr::map_lgl(decisoes, \(x) nrow(x) > 0)
  ) |>
  dplyr::left_join(aux_decisoes, "id")



# export ------------------------------------------------------------------

writexl::write_xlsx(da_cposg_tidy, "inst/da_cposg_tidy.xlsx")
usethis::use_data(da_cposg_tidy, overwrite = TRUE)

