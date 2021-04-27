
'%>%' <- magrittr::`%>%`
casas <- readr::read_rds("data/casas.rds")


# Item 1 ------------------------------------------------------------------

# Motivação: ver o número de categorias distintas em cada variável categórica

# opção 1
casas %>%
  dplyr::select(where(is.character)) %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = dplyr::n_distinct
    )
  )

# opção 2
casas %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = dplyr::n_distinct
    )
  )

# opção 3
casas %>%
  dplyr::select(where(is.character)) %>%
  dplyr::summarise(
    dplyr::across(
      .fns = dplyr::n_distinct
    )
  )



# Item 2 ------------------------------------------------------------------

# Motivação: substituir todos os NAs das variáveis categóricas por "sem informação"

# opção 1
casas %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      tidyr::replace_na,
      replace = "Sem informação"
    )
  )

# opção 2 - usando lanbda
casas %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      ~tidyr::replace_na(.x, replace = "Sem informação")
    )
  )

# opção 3 - usando uma função alternativa
subs_por_sem_info <- function(coluna) {
  tidyr::replace_na(coluna, replace = "Sem informação")
}

casas %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      tidyr::replace_na,
      subs_por_sem_info
    )
  )

# opção 4 - apenas nas 10 primeiras colunas
casas %>%
  dplyr::transmute(
    dplyr::across(
      1:10,
      .fns = ~tidyr::replace_na(.x, replace = "Sem informação")
    )
  )



# Item 3 - Pivotagem ------------------------------------------------------

# Motivação: fazer uma tabela do lucro médio anual dos filmes de comédia, ação e romance

imdb <- readr::read_rds("data/imdb.rds")

# opção 1
imdb %>%
  dplyr::filter(ano >= 2000) %>%
  dplyr::mutate(
    lucro = receita - orcamento,
    flag_comedia = stringr::str_detect(generos, "Comedy"),
    flag_acao = stringr::str_detect(generos, "Action"),
    flag_romance = stringr::str_detect(generos, "Romance")
  ) %>%
  dplyr::select(ano, lucro, starts_with("flag")) %>%
  tidyr::pivot_longer(
    starts_with("flag"),
    names_to = "genero",
    values_to = "flag"
  ) %>%
  dplyr::filter(flag == TRUE) %>%
  dplyr::group_by(genero, ano) %>%
  dplyr::summarise(
    lucro_medio = mean(lucro, na.rm = TRUE)
  ) %>%
  dplyr::arrange(ano) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = lucro_medio
  ) %>%
  View()


# opção 2 - com stringR - a vantagem é que não é necessário criar os "flags"
stringr::str_split(imdb$generos[1:2], "\\|") # exemplo

imdb %>%
  dplyr::filter(ano >= 2000) %>%
  dplyr::mutate(
    lucro = receita - orcamento,
    genero = stringr::str_split(generos, "\\|")
  ) %>%
  tidyr::unnest(genero) %>%
  dplyr::filter(genero %in% c("Comedy", "Action", "Romance")) %>%  # comentar essa linha para fazer pra todos os gêneros
  dplyr::group_by(genero, ano) %>%
  dplyr::summarise(
    lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  dplyr::arrange(ano) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = lucro_medio
  ) %>%
  View()





















