get_category <- function(text, rx) {
  purrr::map(
    rx,
    \(x) {
      purrr::map_lgl(x, \(z) stringr::str_detect(text, stringr::regex(paste(z, collapse = "|"), TRUE)))
    }
  ) |>
    purrr::map(all) |>
    purrr::keep(isTRUE) |>
    names() |>
    dplyr::first()
}

rx_tipo <- function() {
  list(
    compromisso = list(c(
      "compromisso arbitral"
    )),
    instauracao = list(c(
      "instauracao"
    )),
    cautelar = list(c(
      "cautelar", "antecipada"
    )),
    anulacao = list(c(
      "anulacao", "nulidade"
    )),
    cumprimento = list(c(
      "cumprimento"
    ))
  )

}

rx_resultado <- function() {
  list(
    improcedente = list(c(
      "julgo improcedente", "indefiro a tutela"
    )),
    parcial = list(c(
      "julgo procedente em parte", "parcialmente procedente",
      "defiro parcialmente a tutela"
    )),
    acordo = list(c(
      "homologo o acordo"
    )),
    procedente = list(c(
      "julgo (o pedido )?procedente", "homologo",
      "defiro a tutela"
    )),
    extinto = list(c(
      "julgo extinto", "julgo extinta",
      "determino a extincao",
      "determinar a extincao"
    ))
  )

}
