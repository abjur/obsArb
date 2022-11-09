
## paths
path_cjpg <- "data-raw/cjpg"
path_cpopg <- "data-raw/cpopg"


# download cjpg -----------------------------------------------------------

arqs <- lex::tjsp_cjpg_download(
  '"arbitral" OU "lei de arbitragem" OU "9307" OU "9.307"',
  comarca = "100-1146,100-1145,260-2,260-1",
  dir = path_cjpg
)

# parse cjpg --------------------------------------------------------------

arqs <- fs::dir_ls(path_cjpg, regexp = "search", invert = TRUE)
da_cjpg <- purrr::map(arqs, lex::tjsp_cjpg_parse, .progress = TRUE) |>
  dplyr::bind_rows()

# download cpopg ----------------------------------------------------------

# add sleep because TJSP is blocking
purrr::walk(
  unique(da_cjpg$n_processo),
  \(x) {
    Sys.sleep(1)
    lex::tjsp_cpopg_download(x, path_cpopg)
  },
  .progress = TRUE
)

# parse cpopg -------------------------------------------------------------

da_cpopg <- fs::dir_ls(path_cpopg) |>
  purrr::map(lex::tjsp_cpopg_parse, .progress = TRUE) |>
  dplyr::bind_rows(.id = "file") |>
  dplyr::filter(is.na(erro)) |>
  dplyr::select(-erro)


da_cpopg <- da_cpopg |>
  dplyr::filter(is.na(erro)) |>
  dplyr::select(-erro)

# save --------------------------------------------------------------------

readr::write_rds(da_cjpg, "data-raw/da_cjpg.rds")
readr::write_rds(da_cpopg, "data-raw/da_cpopg.rds")
