da_cjpg <- readr::read_rds("data-raw/da_cjpg.rds")
da_cpopg <- readr::read_rds("data-raw/da_cpopg.rds")

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
    resultado
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
      status %in% c("", NA_character_) ~ "Em andamento",
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

da_tidy <- da_cpopg_tidy |>
  dplyr::inner_join(da_cjpg_tidy, "id") |>
  dplyr::mutate(
    tempo = lubridate::interval(dt_dist, dt_disp) / lubridate::days(1),
    categoria = stringr::str_to_title(categoria),
    resultado = stringr::str_to_title(resultado)
  ) |>
  dplyr::filter(categoria != "", !is.na(resultado)) |>
  dplyr::select(-dt_dist)


# export ------------------------------------------------------------------

writexl::write_xlsx(da_tidy, "inst/da_arbitragem.xlsx")

usethis::use_data(da_tidy, overwrite = TRUE)
usethis::use_data(da_cjpg_tidy, overwrite = TRUE)

piggyback::pb_release_create(tag = "relatorio_preliminar")
piggyback::pb_upload("data-raw/da_cjpg.rds", tag = "relatorio_preliminar")
piggyback::pb_upload("data-raw/da_cpopg.rds", tag = "relatorio_preliminar")
piggyback::pb_upload("data-raw/da_cjpg.zip", tag = "relatorio_preliminar")
piggyback::pb_upload("data-raw/da_cpopg.zip", tag = "relatorio_preliminar")

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

