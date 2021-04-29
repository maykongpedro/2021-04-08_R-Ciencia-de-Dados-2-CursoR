# https://discourse.curso-r.com/t/duvida-regex-telefone-whatsapp/1176

entrada <- "+5511981365555"
m <- stringr::str_match(entrada, "^\\+[0-9]{2}([0-9]{2})([0-9]{5})([0-9]{4})")
m

stringr::str_match(entrada, "^\\+[0-9]{2}")
stringr::str_match(entrada, "^\\+[0-9]{2}([0-9]{2})")
stringr::str_match(entrada, "^\\+[0-9]{2}([0-9]{2})([0-9]{5})")
stringr::str_match(entrada, "^\\+[0-9]{2}([0-9]{2})([0-9]{5})([0-9]{4})")


saida <- stringr::str_glue("{m[1,2]} {m[1,3]}.{m[1,4]}")
saida
