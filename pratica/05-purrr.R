
# carregar pipe
`%>%` <- magrittr::`%>%`

# Motivação: ler e empilhar as bases IMDB separadas por ano
imdb_2000 <- readr::read_rds("data/imdb_por_ano/imdb_2000.rds")


# pegar o caminho completo dos arquivos
arquivos <- list.files("data/imdb_por_ano", full.names = TRUE)


# importar todas as bases dentro de uma lista "imdb"
imdb <- purrr::map(arquivos, readr::read_rds)


# importar todas as bases dentro de um data frame "imdb", empilhando as bases
imdb <- purrr::map_dfr(arquivos, readr::read_rds)


# Supondo que a coluna ano não existisse ----------------------------------

ler_base <- function(file) {

  # gerar um identificador para cada base
  ano_arquivo <-
    file %>%

    # extrair do nome do arquivo os 4 números seguidos de um ".rds"
    stringr::str_extract("[0-9]{4}\\.rds$") %>%

    # retirar o ".rds" do nome
    stringr::str_remove("\\.rds") %>%

    # formata como número
    as.numeric()

  # ler os arquivos e gerar uma coluna com o identificador criado anteriormente
  readr::read_rds(file) %>%
    dplyr::mutate(ano_2 = ano_arquivo)

}

# usando a função criada
imdb_2 <- purrr::map_dfr(arquivos, ler_base)


# Exemplo com número sequencial em vez de ano -----------------------------

# gera um identificador pra cada base
id_base <- seq_along(arquivos)

# modificar a função
ler_base <- function(file, id_base) {

  # ler os arquivos e gerar uma coluna com o identificador criado anteriormente
  readr::read_rds(file) %>%
    dplyr::mutate(id_ = id_base)

}

# usando a função modificada
imdb_3 <- purrr::map2_dfr(arquivos, id_base, ler_base)



# Gráficos de dispersão ---------------------------------------------------

# Motivação: fazer gráficos de dispersão do orçamento x receita para todos os anos da base

library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

imdb_nest <-
  imdb %>%

  # agrupando por ano
  dplyr::group_by(ano) %>%

  # gera uma lista com cada base respectiva ao ano dentro de uma coluna chamada "data"
  tidyr::nest()

# visualizando um ano (2009, nesse caso)
imdb_nest$data[[1]]


# criando função que gera o gráfico
fazer_grafico_receita_orcamento <- function(tab) {

  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()

}

# testando função em um ano
fazer_grafico_receita_orcamento(imdb_nest$data[[1]])


# fazer um gráfico para cada ano da base
imdb_graficos <-
  imdb %>%
  dplyr::group_by(ano) %>%
  tidyr::nest() %>%
  dplyr::mutate(grafico = purrr::map(data, fazer_grafico_receita_orcamento))


# verificando um gráfico
imdb_graficos$grafico[10]


# verificando outro gráfico com filtro
imdb_graficos %>%

  # filtra um ano
  dplyr::filter(ano == 1989) %>%

  # pull puxa somente uma "célula" e a transforma em vetor
  dplyr::pull(grafico)




# Dúvida extra ------------------------------------------------------------

criar_coluna_lucro <- function(tab) {

  tab %>%
    mutate(lucro = receita - orcamento)

}

# cria a coluna lucro dentro de cada uma das bases
# está dando erro, tenho que ver depois como arrumar
imdb_nest %>%
  dplyr::mutate(
    data = purrr::map(data, criar_coluna_lucro)
  )


