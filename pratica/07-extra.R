
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


# contar quantos gêneros existem em cada filme
imdb %>%
  dplyr::mutate(
    num_generos = stringr::str_count(generos, pattern = "\\|") + 1
  ) %>%
  dplyr::select(generos, num_generos) #%>%

  # descobrir o número máximo de gêneros que existem em um filme
  #dplyr::summarise(maximo = max(num_generos))



# Item 4 - StringR - Regex ------------------------------------------------

# Motivação: extrair o subtítulo dos filmes

imdb %>%
  dplyr::mutate(
    subtitulo = stringr::str_extract(titulo, ": .*"),
    subtitulo = stringr::str_remove(subtitulo, "^: ")
  ) %>%
  dplyr::select(titulo, subtitulo)



# Item 5 - lubridate ------------------------------------------------------

# Motivação: fazer uma análise decritiva do ozônio

cetesb <- readr::read_rds("data/cetesb.rds")

# correlação com lag (faz a contagem para trás, a base precisa estar ordenada)
cetesb %>%
  dplyr::mutate(
    concentracao_lag3 = dplyr::lag(concentracao, 3)
  ) %>%
  dplyr::relocate(concentracao_lag3, .after = concentracao)


cetesb %>%
  dplyr::filter(poluente %in% c("03", "NO2"),
                estacao_cetesb == "Ibirapuera",
                hora == 13) %>%
  dplyr::mutate(
    concentracao_lag3 = dplyr::lag(concentracao, 3)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = concentracao_lag3, y = concentracao)) +
  ggplot2::geom_point()


# Item 6 - purrr ----------------------------------------------------------

# Motivação: criar coluna de pontos do time de casa
# ganhos a partir de um placar ({ brasileirao })

# remotes::install_github("williamorim/brasileirao")

brasileirao::matches %>% View()


# entendendo o split
gols <- stringr::str_split("100x10", pattern = "x", simplify = TRUE)
gols[1]
gols[2]


# criando função para cálculo da pontuação
calcular_pontos <- function(placar) {

  gols <- stringr::str_split(placar, pattern = "x", simplify = TRUE)

  if (gols[1] > gols[2]) { #se o time da casa ganhar, então é 3 pontos
    return(3)
  } else if (gols[1] < gols[2]) { #se o time da casa perder, então é 0 pontos
    return(0)
  } else if (gols[1] == gols[2]) { #se o time empatar, então é 1 ponto
    return(1)
  }

}

# a função acima não é vetorizada, não aceita um vetor como input, pra isso podemos usar o purrr
purrr::map(c("3x2", "2x2"), calcular_pontos)


# criando coluna com o placar
brasileirao::matches %>%
  dplyr::mutate(
    pontos = purrr::map_dbl(score, calcular_pontos)
  )


# podemos criar a função vetoriada com case when, assim não seria necessário usar o purrr
calcular_pontos_vetorizada <- function(placar) {

  gols <- stringr::str_split(placar, pattern = "x", simplify = TRUE)
  gols_casa <- gols[,1]
  gols_visitante <- gols[, 2]

  dplyr::case_when(
    gols_casa > gols_visitante ~ 3,
    gols_casa < gols_visitante ~ 0,
    TRUE ~ 1
  )

}

# criando coluna com o placar sem purrr::map
brasileirao::matches %>%
  dplyr::mutate(
    pontos = calcular_pontos_vetorizada(score)
  )









