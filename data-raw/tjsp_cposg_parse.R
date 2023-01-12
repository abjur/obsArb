#' Parsear processos de segundo grau do TJSP
#'
#' @param arq Arquivo baixado por [tjsp_cposg_download()]
#'
#' @export
tjsp_cposg_parse <- function(arq) {
  stopifnot(length(arq) == 1)

  # browser()

  html <- xml2::read_html(arq)

  if (length(xml2::xml_find_all(html, "//*[@class='linkProcesso']")) > 0) {
    message("Processo duplo")
    return(dplyr::tibble())
  }
  if (length(xml2::xml_find_first(html, '//*[@id="tabelaTodasMovimentacoes"]')) == 0) {
    message("Processo vazio")
    return(dplyr::tibble())
  }

  # Get some extra data
  cdp <- html |>
    xml2::xml_find_first("//*[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  digital <- html |>
    xml2::xml_find_all("//*[contains(@class, 'linkPasta')]") |>
    length() |>
    as.logical()

  id_original <- html |>
    xml2::xml_find_first("//a[contains(@href, 'processo.codigo')]") |>
    xml2::xml_text()

  # condicoes para garantir que todos os resultados sao aproveitados
  if (length(digital) == 0) digital <- FALSE
  if (length(cdp) == 0) cdp <- NA_character_

  xp_main <- "//div[contains(@class, 'unj-entity-header__summary')]//div[contains(@class, 'col-md')]"
  main <- xml2::xml_find_all(html, xp_main)
  processo <- main |>
    xml2::xml_find_all(".//span[contains(@class, 'unj-larger')]") |>
    xml2::xml_text() |>
    stringr::str_squish()
  status <- main |>
    xml2::xml_find_all(".//span[contains(@class, 'unj-tag')]") |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    paste(collapse = " / ")
  meta_main <- tibble::tibble(
    name = c("processo", "status"),
    value = c(processo, status)
  )
  meta1 <- lex:::parse_label_value(main[-1])
  xp_more <- "//div[contains(@class, 'unj-entity-header__details')]//div[contains(@class, 'col-lg')]"
  meta2 <- html |>
    xml2::xml_find_all(xp_more) |>
    lex:::parse_label_value()
  meta <- dplyr::bind_rows(meta_main, meta1, meta2) |>
    dplyr::mutate(name = lex:::tjsp_clean_nm(name)) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    tidyr::pivot_wider()

  # Movs
  movs <- html |>
    xml2::xml_find_first('//*[@id="tabelaTodasMovimentacoes"]') |>
    xml2::xml_parent() |>
    lex:::xml_table(fill = TRUE) |>
    dplyr::select(-2) |>
    dplyr::as_tibble() |>
    janitor::clean_names() |>
    tidyr::separate(movimento, c("movimento", "descricao"),
                    sep = "\\n\\t\\t", extra = "merge", fill = "right"
    ) |>
    dplyr::mutate(
      movimento = stringr::str_squish(movimento),
      descricao = stringr::str_squish(descricao)
    ) |>
    dplyr::filter(data != "") |>
    dplyr::distinct()

  # Partes
  if (stringr::str_detect(as.character(html), "N\u00e3o h\u00e1 Partes")) {
    partes <- dplyr::tibble()
  } else {
    partes <- html |>
      xml2::xml_find_first('//*[@id="tableTodasPartes"]') |>
      purrr::when(
        length(.) == 0 ~ xml2::xml_find_first(html, '//*[@id="tablePartesPrincipais"]'),
        ~.
      ) |>
      lex:::xml_table() |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        parte = X1,
        papel = stringr::str_split(X2, "\\t "),
        id_parte = seq_len(dplyr::n())
      ) |>
      tidyr::unnest(papel) |>
      dplyr::mutate(
        parte = stringr::str_remove_all(parte, "[^[:alpha:]]"),
        papel = stringr::str_replace_all(papel, "&nbsp", " "),
        papel = stringr::str_remove_all(stringr::str_squish(papel), "[^: [:alpha:]]"),
        nome = stringr::str_extract(papel, "(?<= |^)[[:alpha:] ]+$"),
        papel = stringr::str_extract(papel, "^[:alpha:]+(?=:)"),
        papel = ifelse(is.na(papel), parte, papel)
      ) |>
      dplyr::filter(!is.na(nome)) |>
      dplyr::select(id_parte, nome, parte, papel)
  }

  # Hist\u00f3rico
  hist <- html |>
    xml2::xml_find_first('//table[@id="tdHistoricoDeClasses"]') |>
    purrr::when(length(.) == 0 ~ dplyr::tibble(), ~ xml_table(., header = FALSE)) |>
    dplyr::as_tibble()

  # Possiveis tabelas
  tables <- html |>
    xml2::xml_find_all("//table[@style='margin-left:15px; margin-top:1px;']")

  # Julgamentos
  first_table <- tables |>
    xml2::xml_text() |>
    stringr::str_which("Situa\u00e7\u00e3o do julgamento") |>
    max()

  if (is.infinite(first_table)) {
    # Se nao existir
    decisoes <- tibble::tibble()
  } else {
    # Se existir
    last_table <- length(tables)
    decisoes <- tables[first_table:last_table] |>
      lex:::xml_table(fill = TRUE) |>
      # purrr::keep(~is.numeric(.x$X3)) |>
      dplyr::bind_rows() |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        X1 = lubridate::dmy(X1, quiet = TRUE),
        X2 = stringr::str_replace_all(X2, "[:space:]+", " "),
        X3 = stringr::str_replace_all(X3, "[:space:]+", " ")
      ) |>
      dplyr::select(-X2) |>
      dplyr::filter(!is.na(X1)) |>
      purrr::set_names("data", "decisao")
  }

  # Composicao do julgamento
  first_table <- tables |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    stringr::str_which("^Relator") |>
    max()

  if (is.infinite(first_table)) {
    # Se nao existir
    composicao <- tibble::tibble()
  } else {
    # Se existir
    composicao <- tables[first_table] |>
      lex:::xml_table(fill = TRUE) |>
      dplyr::bind_rows() |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        X1 = stringr::str_squish(X1),
        X2 = stringr::str_squish(X2)
      ) |>
      dplyr::filter(!is.na(X1)) |>
      purrr::set_names("participacao", "magistrado")
  }

  # Juntar
  meta |>
    dplyr::mutate(
      id_original = id_original,
      movimentacoes = list(movs),
      partes = list(partes),
      historico = list(hist),
      decisoes = list(decisoes),
      composicao = list(composicao)
    ) |>
    janitor::clean_names() |>
    purrr::set_names(stringr::str_remove_all, "_$")
}
