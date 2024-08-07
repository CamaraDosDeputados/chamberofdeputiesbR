#' Extrai os dados básicos dos \emph{deputados federais} de uma dada
#' \emph{legislatura}.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
#' os dados básicos dos \emph{deputados federais}, segundo os critérios informados.
#' Caso apenas o \emph{id} da \emph{Legislatura} seja passado,
#' a função retorna os dados dos deputados federais desta \emph{Legislatura} e
#' das seguintes, caso ele tenha sido reeleito e se encontre em exercício.
#' Caso, além do \emph{id} da \emph{Legislatura}, o data início e
#' data fim sejam passadas, a função retornará só os dados dos deputados que
#' atendam aos três critérios.
#'
#' @name extrair_lideres_legislatura
#' @param url_base A base da URL do JSON que contém os dados básicos dos deputados.
#' @param idLegislatura Identificador único da legislatura; caso não informado,
#' retorna os deputados em exercício.
#' @param itens Número de itens retornados por página; padrão = 15 (mesmo da API).
#' @return Dataframe contendo os dados básicos dos deputados.
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_locate
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_lideres_legislatura <- extrair_lideres_legislatura()

library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

extrair_lideres_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/legislaturas",
    idLegislatura = 57,
    itens = 15) {

  # Cria uma lista de parâmetros da consulta
  query_params <- list (
    idLegislatura = idLegislatura,
    itens = itens
  )

  # Remove os parâmetros que são NULL
  query_params <- query_params[!sapply(query_params, is.null)]

  # Constrói a string da query
  query_string <- paste0("/", query_params[1]$idLegislatura, "/lideres")
  query_string <- paste0(query_string, "?", names(query_params)[2], "=",
                         query_params[2]$itens)

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, query_string)

  tryCatch({
    # Faz a requisição inicial à API
    doc <- jsonlite::fromJSON(url_endpoint)

    # Extrai os links de paginação
    links <- doc$links
    last_link <- as.character(links[links$rel == "last", ]$href)

    # Determina o número de páginas
    n_paginas <- as.numeric(substr(last_link,
                                   str_locate(last_link, "pagina=")[2] + 1,
                                   str_locate(last_link, "&itens")[1] - 1))

    # Itera sobre as páginas e junta os resultados usando map_dfr
    dados <- purrr::map_dfr(1:n_paginas, function(pagina) {
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

#teste
df_lideres_legislatura <- extrair_lideres_legislatura()
