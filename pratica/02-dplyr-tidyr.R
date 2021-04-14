
# carregar pipe
`%>%` <- magrittr::`%>%`

# carregar base
casas <- dados::casas

View(casas)

# opções semelhantes ao código inteiro
skimr::skim(casas) %>%
  View()

# Objetivo: ver o número de categorias distintas em cada variável categórica
casas %>%
  dplyr::summarise(
    dplyr::across(
      .cols = where(is.character),
      .fns = dplyr::n_distinct
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variavel",
    values_to = "num_categorias"
  ) %>%
  dplyr::arrange(desc(num_categorias)) %>%
  View()


# Objetivo: ver quais colunas possuem NAs e quantos

sum(is.na(casas$piscina_qualidade)) # quantidade de NAs dentro da coluna de qualidade da piscina

casas %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~sum(is.na(.x))
    )
  ) %>%
  View()

casas %>% dplyr::select(starts_with("piscina"))


# visualizar somente o que realmente tem NA
casas %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~sum(is.na(.x))
    )
  ) %>%
  dplyr::select(
    where(~.x > 0)
  ) %>%
  View()


# visualizar em formato tidy
casas %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variavel",
    values_to = "num_na"
  ) %>%
  dplyr::filter(num_na > 0) %>%
  dplyr::arrange(desc(num_na))


# substituir NAs categóricos
casas %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      tidyr::replace_na,
      replace = "A casa não possui"
      )
    ) %>%
  View()


# substituir NAs númericos
casas %>%
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      tidyr::replace_na,
      replace = 0
    )
  ) %>%
  View()


# -------------------------------------------------------------------------

# Objetivo: Descobrir o ator com o maior lucro médio na base IMDB considerando as 3 colunas de elenco

imdb <- readr::read_rds("./data/imdb.rds")

imdb %>%
  tidyr::pivot_longer(
    cols = starts_with("ator"),
    names_to = "protagonismo",
    values_to = "ator_atriz"
  ) %>%

  #View()

  dplyr::mutate(lucro = receita - orcamento) %>%
  dplyr::group_by(ator_atriz) %>%
  dplyr::summarise(
    lucro_medio = mean(lucro, na.rm = TRUE),
    num_filmes = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(lucro_medio)) %>%
  dplyr::slice_max(lucro_medio, n = 10) # ordenada sozinho
  #dplyr::top_n(10, lucro_medio) #top_n não ordena, ai precisa do arrange antes ou depois

