# busca

#' Define a url da busca
#'
#'
url_busca <- function(){
  "http://search.folha.uol.com.br/?q="
}

#' Tratar data
#'
#' Faz tratamentos necessários na data para incluir na
#' query
#'
#' @param d data em formato dia-mes-ano
#'
#'
tratar_data <- function(d){
  if(d != ""){
    d <- d %>%
      lubridate::dmy() %>%
      as.character() %>%
      stringr::str_replace_all(stringr::fixed("-"), "%2F")
  }
  return(d)
}

#' Query
#'
#' Cria a query
#'
#' @param p palavras chave separadas por espaço
#' @param sd data de inicio da busca
#' @param ed data de fim da busca
#'
#' @note as datas devem ser passadas no formato dia-mes-ano
#'
#' @export
query <- function(p, sd = "", ed = ""){
  p <- stringr::str_split(p, " ") %>% unlist
  sd <- tratar_data(sd)
  ed <- tratar_data(ed)
  paste0(
    url_busca(),
    paste(p, collapse = "+"),
    "&sd=",
    sd,
    "&ed=",
    ed)
}

#' Quantidade de resultados
#'
#' @param p pagina recebida pelo httr::GET
#'
#' @return o número de resultados encontrados na busca
#'
qtd_resultados <- function(p){
  p %>% httr::content() %>%
    rvest::html_nodes(".search-title") %>%
    rvest::html_text() %>%
    stringr::str_extract(" [0-9]*\\)") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric()
}

#' Baixar páginas da busca
#'
#' Baixa todas as páginas que contém as listas de resultados da sua busca.
#'
#' @note É importante baixar os dados html para se precisar rodar novamente o
#' código, você não ter que baixá-los novamente.
#'
#' @export
baixar_pag_busca <- function(q, dir, t = 1){

  p <- httr::GET(q)
  n <- qtd_resultados(p)

  paginas <- seq(from = 1, to = 1639, by = 25)
  message(paste(length(paginas), "páginas serão baixadas."))

  for(i in 1:length(paginas)){
    p <- paste0(q, "&sr=", paginas[i])
    fn <- paste0(dir, "pagina", i, ".html")
    httr::GET(p, write_disk(path = fn))
    Sys.sleep(t)
  }

}

