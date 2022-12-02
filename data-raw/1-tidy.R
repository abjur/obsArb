da_cjpg <- readr::read_rds("data-raw/da_cjpg.rds")
da_cpopg <- readr::read_rds("data-raw/da_cpopg.rds")

# mencionar na análise
# retirar carta arbitral

# i) anulação de decisão arbitral,
# ii) cumprimento de sentença / execução,
# iii) medida cautelar antecedente ao tribunal arbitral
# iv) ação de instauração da arbitragem
# v) compromisso arbitral.

da_cjpg_tidy <- da_cjpg |>
  dplyr::mutate(
    resumo = abjutils::rm_accent(resumo),
    categoria = purrr::map_chr(resumo, get_category, rx_tipo()),
    resultado = purrr::map_chr(resumo, get_category, rx_resultado())
  ) |>
  dplyr::transmute(
    id = n_processo,
    categoria,
    classe,
    assunto,
    dt_disp = lubridate::dmy(data_de_disponibilizacao),
    # magistrado = stringr::str_to_title(magistrado),
    vara,
    resultado#,
    # resumo
  ) |>
  dplyr::arrange(dplyr::desc(dt_disp)) |>
  dplyr::distinct(id, .keep_all = TRUE)

da_cpopg_tidy <- da_cpopg |>
  dplyr::mutate(id = fs::path_ext_remove(fs::path_file(file))) |>
  dplyr::semi_join(da_cjpg_tidy, "id") |>
  dplyr::transmute(
    id,
    status = stringr::str_remove(status, "(, )?Tramitação prioritária"),
    status = dplyr::case_when(
      status %in% c("", NA_character_) ~ "(Vazio)",
      TRUE ~ status
    ),
    dt_dist = dplyr::coalesce(distribuicao, recebido_em),
    dt_dist = lubridate::dmy(stringr::str_extract(dt_dist, "[0-9/]+")),
    valor = readr::parse_number(
      valor_da_acao,
      locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
    ),
    digital = dplyr::if_else(digital, "Sim", "Não")
  )

da_tidy_auto <- da_cpopg_tidy |>
  dplyr::inner_join(da_cjpg_tidy, "id") |>
  dplyr::mutate(
    tempo = lubridate::interval(dt_dist, dt_disp) / lubridate::days(1),
    categoria = stringr::str_to_title(categoria),
    resultado = stringr::str_to_title(resultado)
  ) |>
  dplyr::filter(!is.na(resultado)) |>
  dplyr::select(-dt_dist)


# revisão manual dos casos ------------------------------------------------

aux_ajustes_manuais <- readxl::read_excel("data-raw/PLANILHA VALIDADA Versão Final - ajustada.xlsx") |>
  dplyr::filter(is.na(remover)) |>
  dplyr::transmute(
    id,
    categoria_validada = abjutils::rm_accent(categoria_validada),
    categoria_validada = dplyr::case_when(
      categoria_validada == "Instauracao/Cautelar" ~ "Cautelar",
      TRUE ~ categoria_validada
    ),
    resultado_validado,
    unimed,
    liminar = liminar_deferida_cautelar
  ) |>
  dplyr::filter(categoria_validada != "Carta Arbitral")

da_tidy <- da_tidy_auto |>
  dplyr::inner_join(aux_ajustes_manuais, "id") |>
  dplyr::mutate(
    categoria = categoria_validada,
    resultado = resultado_validado,
    unimed = dplyr::if_else(is.na(unimed), "Não", "Sim")
  ) |>
  dplyr::select(-categoria_validada, -resultado_validado)

# export ------------------------------------------------------------------

writexl::write_xlsx(da_tidy, "inst/da_arbitragem.xlsx")

usethis::use_data(da_tidy, overwrite = TRUE)
usethis::use_data(da_cjpg_tidy, overwrite = TRUE)

# piggyback::pb_release_create(tag = "relatorio_preliminar_v3")
piggyback::pb_upload("data-raw/da_cjpg.rds", tag = "relatorio_preliminar_v3", overwrite = TRUE)
piggyback::pb_upload("data-raw/da_cpopg.rds", tag = "relatorio_preliminar_v3", overwrite = TRUE)
piggyback::pb_upload("data-raw/cjpg.zip", tag = "relatorio_preliminar_v3", overwrite = TRUE)
piggyback::pb_upload("data-raw/cpopg.zip", tag = "relatorio_preliminar_v3", overwrite = TRUE)
piggyback::pb_upload("data-raw/PLANILHA VALIDADA Versão Final - ajustada.xlsx", tag = "relatorio_preliminar_v3", overwrite = TRUE)
piggyback::pb_upload("data-raw/PLANILHA VALIDADA Versão Final.xlsx", tag = "relatorio_preliminar_v3", overwrite = TRUE)

# da_cpopg_tidy |>
#   dplyr::count(status)
#
# da_cpopg_tidy$movimentacoes[[5]]
#
# da_cjpg_tidy |>
#   dplyr::filter(id == "00342147620218260100") |>
#   with(resumo) |>
#   cat()

# classificados |>
#   dplyr::count(categoria, sort = TRUE) |>
#   print(n = 100)
#
# classificados |>
#   dplyr::filter(categoria == "cautelar") |>
#   dplyr::slice_sample(n = 1) |>
#   dplyr::select(n_processo, resumo) |>
#   unlist() |>
#   cat(sep = "\n")

