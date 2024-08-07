#' Extrai os dados básicos das \emph{frentes parlamentares} da Câmara dos
#' Deputados de uma dada \emph{Legislatura}.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' de \emph{frentes parlamentares} de uma \emph{Legislatura} específica ou de
#' todas as legislaturas.
#'
#' @name extrair_frentes_parlamentares
#' @param url A URL do JSON que contém os dados básicos de frentes parlamentares
#' @param idLegilatura Identificador único da legislatura; caso não informado,
#' retorna os dados das frentes de todas as legislaturas.
#' @param itens Número de itens retornados por página; padrão = 100 (mesmo da API).
#' @return Um dataframe contendo os dados básicos da frente parlamentar.
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_locate
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_frentes_legislatura <- extrair_frentes_parlamentares()
#' df_frentes_legislatura_57 <- extrair_frentes_legislatura(idLegislatura = 57)

library(jsonlite)
library(stringr)
library(purrr)
library(tibble)

extrair_frentes_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/frentes",
    idLegislatura = NULL,
    itens = 100) {

  # Cria uma lista de parâmetros da consulta
  query_params <- list(
    idLegislatura = idLegislatura,
    itens = itens
  )

  # Remove os parâmetros que são NULL
  query_params <- query_params[!sapply(query_params, is.null)]

  # Constrói a string da query
  query_string <- paste0("?", paste0(names(query_params), "=", query_params,
                                     collapse = "&"))

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, query_string)

  tryCatch({
    # Faz a requisição inicial à API
    doc <- jsonlite::fromJSON(url_endpoint)

    # Extrai os links de paginação
    links <- doc$links
    last_link <- as.character(links[links$rel == "last", ]$href)

    # o número de páginas
    n_paginas <- as.numeric(substr(last_link,
                                   str_locate(last_link, "pagina=")[2] + 1,
                                   str_locate(last_link, "&itens")[1] - 1))

    # Itera sobre as páginas e junta os resultados usando map_dfr
    dados <- map_dfr(1:n_paginas, function(pagina) {
      url_endpoint_pag <- paste0(url_endpoint, "&pagina=", pagina)
      doc_pag <- jsonlite::fromJSON(url_endpoint_pag)
      return(doc_pag$dados)
    })

    # Retorna os dados como um tibble
    return(tibble::as_tibble(dados))

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })
}

# Testes

## Teste 1
df_frentes_legislatura <- extrair_frentes_legislatura()

## Teste 2
df_frentes_legislatura_57 <- extrair_frentes_legislatura(idLegislatura = 57)
