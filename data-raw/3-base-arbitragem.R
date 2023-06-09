# Cria a tribble
sentencas_camaras <- tibble::tribble(
  ~sigla, ~nome_camara, ~qtd_sentencas,
  "AMCHAM", "Centro de Arbitragem e Mediação AMCHAM", 38,
  "CAM", "Câmara de Arbitragem do Mercado", 64,
  "CAMARB", "Câmara de Mediação e Arbitragem Empresarial", 45,
  "CBMA", "Centro Brasileiro de Mediação e Arbitragem CBMA", 2,
  "CCBC", "Centro de Arbitragem e Mediação da Câmara de Comércio Brasil-Canadá CAM-CCBC", 223,
  "CCI", "CCI", 47,
  "FGV", "Câmara FGV de Mediação e Arbitragem", 54,
  "FIESP", "Câmara de Conciliação, Mediação e Arbitragem CIESP/FIESP", 133
)

usethis::use_data(sentencas_camaras, overwrite = TRUE)
