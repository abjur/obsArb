get_category <- function(text, rx) {
  purrr::map(
    rx,
    \(x) stringr::str_detect(text, stringr::regex(paste(x, collapse = "|"), TRUE))
  ) |>
    purrr::keep(isTRUE) |>
    names() |>
    dplyr::first()
}

rx_tipo <- function() {
  list(
    compromisso = c(
      "compromisso arbitral"
    ),
    instauracao = c(
      "instauracao"
    ),
    cautelar = c(
      "cautelar", "antecipada"
    ),
    anulacao = c(
      "anulacao", "nulidade"
    ),
    cumprimento = c(
      "cumprimento"
    )
  )

}

rx_resultado <- function() {
  list(
    improcedente = c(
      "julgo improcedente", "indefiro a tutela"
    ),
    parcial = c(
      "julgo procedente em parte", "parcialmente procedente",
      "defiro parcialmente a tutela"
    ),
    acordo = c(
      "homologo o acordo"
    ),
    procedente = c(
      "julgo (o pedido )?procedente", "homologo",
      "defiro a tutela"
    ),
    extinto = c(
      "julgo extinto", "julgo extinta",
      "determino a extincao",
      "determinar a extincao"
    )
  )

}
