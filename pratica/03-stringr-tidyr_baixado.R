
# Motivação: Baixando e limpando dados do Rick and Morty
# da aula 1

# library(dplyr)
# library(stringr)

# carregar pipe
'%>%' <- magrittr::`%>%`

### scrape #### WEB SCRAPING
url <- "https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes"

res <- httr::GET(url)

wiki_page <- httr::content(res)


lista_tab <-
  wiki_page %>%
  # encontrar apenas as tabelas
  xml2::xml_find_all(".//table") %>%

  # escolhendo apenas as tabelas referentes aos episódios
  magrittr::extract(2:5) %>%

  # transformando em tabela como se tivessem sendo lidas pelo html
  rvest::html_table(fill = TRUE) %>%

  # aplica 'clean_names' em cada uma das tabelas
  purrr::map(janitor::clean_names) %>%

  # retirou o "_37" do nome das colunas que continham isso em cada tabela
  purrr::map(~dplyr::rename_with(.x, ~stringr::str_remove(.x, "_37")))


# quantidade de temporadas baseado no fato de que cada tabela é referente a uma temporada
num_temporadas <- 1:length(lista_tab)


tab <-
  lista_tab %>%

  # criando uma coluna em cada tabela que represente qual temporada que é
  purrr::map2(num_temporadas, ~dplyr::mutate(.x, no_season = .y)) %>%

  # empilha todas as tabelas
  dplyr::bind_rows()

################

tab %>% View()


tab %>%
  dplyr::mutate(

  # usar transmute para retornar somente as colunas que estou editando ou criando
  #dplyr::transmute(

    # retirar aspas dos títulos
    # usar aspas simples pra identificar as aspas duplas
    title = stringr::str_remove_all(title, pattern = '"'),

    # retirar conchentes e caracteres dentro deles dos títulos
    # escapa o conchentes, usa "." para pegar qualquer caractere, e "*"para qualquer coisa depois
    title = stringr::str_remove_all(title, pattern = "\\[.*\\]"),

    # retirar conchentes e caracteres detro deles na coluna de viewers
    # "?" serve como 0 ou 1, nesse caso serve pra ele parar na primeira ocorrência dos conchentes
    # nesse caso ele serve pra evitar retirar o que se encontraria entre conhecentes, como : [25] 0.68 [b],
    # onde ele iria retirar o 0.68 também pq está entre os dois conchentes nas extremidades
    u_s_viewers_millions = stringr::str_remove_all(u_s_viewers_millions, "\\[.*?\\]"),

    # transormar em valores númericos a coluna de viewers
    u_s_viewers_millions = as.numeric(u_s_viewers_millions),

    # extrair data da coluna "original_air_date"
    # esse não funciona pq nao tem nenhum padrão q é só número dentro dos parênteses
    #original_air_date = stringr::str_extract(original_air_date, "\\([0-9]\\)")

    # quando adiciono o "-" depois do 9, ele identifica que a sequência pode ter traço
    # esse também não funciona pq ele procura uma situação onde isso ocorre apenas uma vez, o que não é o caso
    #original_air_date = stringr::str_extract(original_air_date, "\\([0-9-]\\)")

    # quando adiciono o "+", ele considera toda a situação que isso ocorre e ai sim traz corretamente
    # esse funciona
    #original_air_date = stringr::str_extract(original_air_date, "\\([0-9-]+\\)")

    # quando adiciono o "{10}", ele extrai exatamente 10 caracteres dentro da condição dos parênteses, sendo número ou "-"
    # esse também funciona
    original_air_date = stringr::str_extract(original_air_date, "\\([0-9-]{10}\\)"),

    # retirar os parênteses da coluna de data
    # aqui ele usa o "|" como um argumento "ou", ou seja, "(" ou ")", e aqui é necessário escapar as barras
    #original_air_date = stringr::str_remove_all(original_air_date, "\\(|\\)")

    # esse também funciona
    # extrai o "(" ou ")" todas as vezes em que aparecerem, o [] serve como o argumento "ou"
    original_air_date = stringr::str_remove_all(original_air_date, "[()]"),

    # transforma a coluna de data em tipo data
    original_air_date = lubridate::as_date(original_air_date)

    ) %>%
  View()


  dplyr::select(
    num_episodio = no_overall,
    num_temporada = no_season,
    num_dentro_temporada = no_inseason,
    titulo = title,
    direcao = directed_by,
    roteiro = written_by,
    data_transmissao_original = original_air_date,
    qtd_espectadores_EUA = u_s_viewers_millions
  ) %>%
  tibble::as_tibble() %>% View()
