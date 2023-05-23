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

# da_cposg |>
#   dplyr::count(classe, sort = TRUE)

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

# writexl::write_xlsx(da_cposg_tidy, "inst/da_cposg_tidy.xlsx")
# usethis::use_data(da_cposg_tidy, overwrite = TRUE)


da_cposg_tidy_sem_decisao <- da_cposg_tidy |>
  dplyr::filter(!tem_decisao) |>
  dplyr::select(id, classe, tem_decisao)

# validação manual --------------------------------------------------------

da_cposg_tidy_revisada <- readxl::read_excel("data-raw/planilha 2 grau.xlsx") |>
  janitor::clean_names() |>
  tidyr::replace_na(list(conferencia = "Provido")) |>
  dplyr::select(-dec, -tem_decisao) |>
  dplyr::rename(dec = conferencia) |>
    dplyr::inner_join(
      dplyr::select(da_tidy, id, categoria),
      "id"
    )

# da_cposg_tidy_revisada |>
#   dplyr::group_by(categoria, classe, alteracao) |>
#   dplyr::summarise(
#     processos = paste(unique(id), collapse = ","),
#     n = dplyr::n(),
#     .groups = "drop"
#   ) |>
#   dplyr::arrange(categoria, desc(n)) |>
#   dplyr::filter(!is.na(alteracao)) |>
#   writexl::write_xlsx("data-raw/planilha_2_grau_alteracao_decisao.xlsx")

planilha_2_grau_alteracao_decisao <- readxl::read_excel(
  "data-raw/planilha_2_grau_alteracao_decisao.xlsx"
)

da_cposg_tidy_com_decisao <- da_cposg_tidy_revisada |>
  dplyr::left_join(
    planilha_2_grau_alteracao_decisao,
    c("categoria", "classe", "alteracao")
  ) |>
  dplyr::select(
    id, classe, categoria, dec, unimed, reformou_sentenca_arbitral,
    comentario = alteracao
  ) |>
  dplyr::mutate(tem_decisao = TRUE)

ids_unimed_1inst <- da_tidy |>
  dplyr::filter(unimed == "Sim") |>
  with(unique(id))

da_cposg_tidy_improcedentes <- da_cposg_tidy |>
  dplyr::filter(tem_decisao, dec == "Improvido") |>
  dplyr::mutate(
    reformou_sentenca_arbitral = "Não"
  ) |>
  dplyr::inner_join(dplyr::select(da_tidy, id, categoria, unimed), "id")

da_cposg_tidy <- dplyr::bind_rows(
  da_cposg_tidy_com_decisao,
  da_cposg_tidy_sem_decisao,
  da_cposg_tidy_improcedentes
) |>
  # adicionando coluna status
  dplyr::inner_join(
    dplyr::select(da_cposg_filter, id, status),
    "id"
  ) |>
  dplyr::mutate(
    status = dplyr::if_else(status == "", "Ativo", status)
  )


da_cposg_tidy <- da_cposg_tidy |>
  dplyr::mutate(
    categoria = dplyr::if_else(categoria == "Compromisso", "Convencao", categoria)
  )

writexl::write_xlsx(da_cposg_tidy, "inst/da_cposg_tidy.xlsx")
usethis::use_data(da_cposg_tidy, overwrite = TRUE)

