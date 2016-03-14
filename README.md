<!-- README.md is generated from README.Rmd. Please edit that file -->
rfolha
======

Download de notícias do site da Folha de São Paulo

Instalação
==========

Para instalar o pacote use o comando.

``` r
devtools::install_github("dfalbel/rfolha")
```

Usando
======

Para evitar grande quantidade de acessos ao site, o pacote trabalha sempre fazendo o download de todas as páginas que serão utilizadas para em seguida fazer o processamento.

### Download das páginas de busca

``` r
library(rfolha)
baixar_pag_busca(q("olimpiadas"), dir = "data-raw/busca")
```

### Tabelar as páginas de busca

``` r
busca <- tabelar_busca(dir = "data-raw/busca")
```

### Baixar páginas encontradas

``` r
controle <- baixar_urls(busca$url, dir = "data-raw/noticias/")
```

### Obter o texto das notícias

``` r
busca$texto <- processar_noticias(dir = "data-raw/noticias/")
```
