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

  controle <- data.frame(
    url = url,
    arq = a,
    status = F
  )

  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(url), clear = FALSE)

  for(i in 1:length(url)){
    httr::GET(url[i], httr::write_disk(path = a[i], overwrite = T))
    controle$status[i] <- check_download(a[i])
    Sys.sleep(t)
    pb$tick()
  }

  return(controle)
}

#' Checar download
#'
#' @param f path para o arquivo baixado
#'
check_download <- function(f){
  info <- file.info(f)
  size <- info$size
  if(size < 2000){
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Processar notícias
#'
#' @param dir diretorio com todos os arquivos das notícias
#'
#' @export
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

# c <- baixar_urls(url = x$url, dir = "data-raw/noticias2/")
# c2 <- baixar_urls(url = as.character(c$url[!c$status]), dir = "data-raw/noticias2/")
# # rodar de novo p/ os false


