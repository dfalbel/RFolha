#' Dá um nome p/ um url
#'
#' Apenas padrononiza o nome, já que o nome do arquivo não pode conter
#' barras, por exemplo.
#'
#'
nome_url <- function(url){
  url %>%
    stringr::str_replace_all("http://", "") %>%
    stringr::str_replace_all("/", "_")
}

#' Baixar lista de urls
#'
#' @param url vetor com as urls que deseja salvar
#' @param dir diretório em que deseja salvar as pastas
#' @param t tempo entre o download das páginas
#'
#' @export
baixar_urls <- function(url, dir, t = 1){
  a <- paste0(dir, nome_url(url))
  message(paste(length(url), "notícias serão baixadas"))
  for(i in 1:length(url)){
    httr::GET(url[i], httr::write_disk(path = a[i], overwrite = T))
    Sys.sleep(t)
  }
  return(dir)
}


#' Processar notícias
#'
#'
processar_noticias <- function(dir){
  noticias <- list.files(dir, full.names = T)
  textos <- plyr::llply(noticias, function(n){
    xml2::read_html(n) %>%
      rvest::html_nodes("p") %>%
      rvest::html_text()
  })
  textos_duplicados <- textos %>%
    unlist()
  textos_duplicados <- textos_duplicados[duplicated(textos_duplicados)] %>% unique

}
