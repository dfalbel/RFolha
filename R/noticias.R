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

#' Procurar refresh
#'
#' Essa função serve apenas para procurar a url do refresh.
#' Algumas notícias foram movidas e se encontram em outra URL.
#'
procurar_refresh <- function(path){
  r <- xml2::read_html(path) %>%
    rvest::html_nodes("meta") %>%
    rvest::html_attr("content") %>%
    paste(collapse = " ") %>%
    stringr::str_extract("url=[^( )]+") %>%
    stringr::str_replace(stringr::fixed("url="), "")
  return(r)
}


#' GET_F
#'
#' Essa é a função GET do HTTR com wrap de failwith.
#' Assim, se der algum erro ela retorna FALSE ao invés de parar a
#' execução.
#'
GETF <- dplyr::failwith(FALSE, quiet = TRUE, f = function(url, path){
  httr::GET(url, httr::write_disk(path = path, overwrite = T))
  return(TRUE)
})

#' Baixar url
#'
#' Lógica necessária p/ baixar um URL. Depois essa função é usada p/ fazer
#' o download de diversas URLS's
#'
#'
baixar_url <- function(url, path){
  status <- GETF(url, path)
  if(!status){
    return(F)
  }
  status <- check_download(path)
  # estratégias se não baixou
  # algumas tem um refresh page
  if(!status){
    new_url <- procurar_refresh(path)
    if(stringr::str_length(new_url) > 4 & new_url != url){
      status <- baixar_url(new_url, path)
    } else {
      status <- FALSE
    }
  }
  return(status)
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
    controle$status[i] <- baixar_url(url[i], a[i])
    Sys.sleep(t)
    pb$tick()
  }

  return(controle)
}

#' Checar download
#'
#' Checa se o downlaod foi concluído com sucesso.
#' A única condição atualmente é que o arquivo possua pelo menos 1.8kB
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
#' @return retorna um vetor de chr com os textos de cada uma das notícias.
#'
#' @export
processar_noticias <- function(dir){
  noticias <- list.files(dir, full.names = T)
  textos <- plyr::llply(noticias, function(n){
    xml2::read_html(n) %>%
      rvest::html_nodes("p") %>%
      rvest::html_text()
  })

  # textso duplicados
  textos_duplicados <- textos %>%
    unlist()
  textos_duplicados <- textos_duplicados[duplicated(textos_duplicados)] %>% unique

  # remover duplicados
  textos <- plyr::llply(textos, function(x, duplicados){
    x <- x[!(x %in% duplicados)]
    return(x)
  },
  duplicados = textos_duplicados)

  # concatenar tudo
  textos <- plyr::laply(textos, function(x){
    paste(x, collapse = " ")
  })

  return(textos)
}
