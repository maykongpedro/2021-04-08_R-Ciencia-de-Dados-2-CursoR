library(stringr)

# CEP
cep <- "04066-094"

# extrai os 5 primeiros caracteres
bairo <- str_sub(cep, start = 1, end = 5)
bairo

# extrai os 3 últimos caracteres
loco  <- str_sub(cep, start = 7, end = 9)
loco

loco  <- str_sub(cep, start = -3, end = -1)
loco


# Regex -------------------------------------------------------------------

# Caracteres especiais:
# . = qualquer caractere
# ^ = início da string
# $ = fim da dstring

# Nome
nome <- c("Ghion", "Guion", "Gxion")

#"G" seguido de "qualquer letra" seguido de "ion". O ponto é um curinga para qualquer símbolo ou letra.
str_detect(nome, "G.ion")


# Tentando detectar  o caractere ponto
# Escapando caracteres especiais. No Regex usamos "\\" para escapar de um caractere que tem significado dentro dele.
ponto <- c("Teste", "Teste.")
str_detect(ponto, "\\.")

# Strigs de exemplo
frutas <- c("banana", "TANGERINA", "maçã", "lima")
ois <- c("oi", "oii", "oiii!", "oiii!!!")



# Regex básico com str_subset (me traz as palavras)
str_subset(frutas, "na")  # Texto # palavras que contenham "na"
str_subset(frutas, "NA")  # Maiúscula - palavras que contenham "NA"
str_subset(frutas, "ma") # palavras que contenham "ma"
str_subset(frutas, "^ma") # Início - palavra que começa com "ma"
str_subset(frutas, "ma$") # Final  - palavra que acaba com "ma"
str_subset(frutas, ".a")  # Qualquer caractere que contenha "a"
str_subset("nao", "na") # caractere que contenha "na"
str_subset("nao", ".na") # qualquer letra seguida de "na", no caso, não tem

# Regex com quantidades
# extrair o que aparece 1 ou mais vezes
str_extract(ois, pattern = "i+")  # i seguido de quantos mais "i" tiverem
str_extract_all("oioiiioi", "i+") # i seguido de quantos mais "i" tiverem
str_extract(ois, "i+!") # extrai apenas o "i" seguidos de "!"
str_extract(ois, "i+!?")   # 0 ou 1 - extrai "i" seguido OU não de um "!"
str_extract(ois, "i+!*")   # 0 ou mais -  extrai o "i" seguido de "!" e quantos mais "!" tiverem
str_extract(ois, "i*")     # retorna o que é "zero" ou mais + "i", retorando sempre ""
str_extract(ois, "i{1,2}") # Entre m e n - retonra "i" entre o primeiro e segundo caractere

# Regex com conjuntos
str_extract(ois, "[i!]$") # Algum , extrai o caractere que acaba com "i" ou com "!"
str_extract(ois, "[i!]+") # Algum ,  extrai o caractere "i" ou "!" e suas repetições seguidas
str_extract("oi!i!i!i!", "[i!]+") # extrai o caractere "i" ou "!" e suas repetições seguidas

str_subset(frutas, "[nN]") # extrai palavras que contenham "n" ou "N"
str_subset(c("ghion", "Guion", "Gxion"), "[Gg][hu]ion") # extrai palavras que contenham "G"ou "g"e "h"ou "u" seguido de "ion"
str_extract(frutas, "[a-z]")  # extrai primeiros caracteres que contenham qualquer letra minúscula do alfaberto
str_extract(frutas, "[A-Z]") # extrai primeiros caracteres que conhteam qualquer letra maiúscula do alfabeto
str_extract(frutas, "[0-9]") # extrai primeiros caracteres que contenham números
str_extract(ois, "(oi)+") # Tudo - extrai qualquer "o" seguido de "i"
str_extract("oioioi!", "(oi)+") # extrai qualquer "o" seguido de "i"

str_remove("Meu número é 392023904", "[0-9]")     # Não funciona, pq extrai apenas o primeiro número
str_remove("Meu número é 392023904", "[0-9]+") # remove o caractere que é número seguido de outro número
str_remove_all("Meu número é 392023904", "[0-9]") # remove todos os caracteres que são números

# 1a texto
"teste "disso" aqui"
"teste \"disso\" aqui"
'teste \'disso\' aqui' # escapa a função "aspas simples" dentro do R
'teste "disso" aqui' # <- Usar isso aqui
"teste 'disso' aqui" # usando aspas simples e aspas duplas para permitir uma

# 2a regex  - dentro do Regex é necessário usar duas "\" para escapar um caractere especial
str_replace("Bom dia.", pattern = "\\.", replacement = "!") # substituindo ponto por "!"

# Regex com escapados
cat("Eu disse \"oi\"") # escapaou as aspas
cat('Eu disse "oi"') # <- Usar isso aqui  - para aspas é melhor usar aspas simples fora e aspas duplas dentro
str_replace("Bom dia.", ".", "!") # aqui o ponto não foi escapado, ai ele identifica "qualquer caractere" e troca por "!"
str_replace("Bom dia.", "\\.", "!")      # Escapando - aqui foi correto, ele escapou o ponto
str_replace("Bom. Dia.", "\\.", "!") # escapa o ponto e troca o primeiro "." que encontrar por "!"
str_replace_all("Bom. Dia.", "\\.", "!") # Lembrar do _all  -> Usar "all" para trocar todas as ocorrências
str_remove_all("Bom \"dia\"", "\\\"")    # Escapando escape -> O que quero remover é o ' \" ' , pra isso usa "\\"

# Para um filter()
dplyr::select(str_detect(frutas, "ma$")) # resultou em erro, retirar essa dúvida na aula


# Escapando a barra
str_replace("Uma barra: \\", "\\\\", "!") # escapando duas barras ( "\\" )

# Português
stringi::stri_trans_general("Váríös àçêntõs", "Latin-ASCII") # remover todos os acentos de strings

# esse código a seguir retorna só o "n" pq o "ú" não consta dentro do [a-z]
str_extract("número: (11) 91234-1234", "[a-z]+")     # Acentos - extrai uma seq. que contenha alguma letra do alfabeto

# esse código retorna corretamente, pois considera os acentos:
str_extract("número: (11) 91234-1234", "[:alpha:]+") # Acentos - extrai uma seq. de caractere alfabéticos com acentos

# substitui o nome Cassio , seja com "a" ou "á"
str_replace(string = c("Cassio", "Cássio"), pattern = "C[aá]ssio", "Teste")

# Extra |  ( \\W ) -> usado para identificar um caractere que não é letra nem número
# \\1  e \\2 -> usado para definir algo entre o primeiro conjunto e o segundo conjunto
str_replace(c("Cassio sio teste"), "(\\W)sio(\\W)", "\\1AQUI\\2")  # substitui a palavra "sio" entre o primeiro espaço (conjunto) e o segundo espaço

# Ignorando maíuscula
str_detect(frutas, "[nN][aA]") # da match em n minúsculo ou maiúsculo ou a minúsclo ou a maiúsculo
str_detect(frutas, regex("na", ignore_case = TRUE)) # ignora maículo ou minúsculo
