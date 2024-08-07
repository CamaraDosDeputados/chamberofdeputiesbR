#' Extrai os dados básicos dos \emph{deputados federais} de uma dada \emph{Legislatura}.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' dos \emph{deputados federais}, segundo os critérios informados.
#' Caso apenas o \emph{id} da \emph{Legislatura} seja passado, a função retorna
#' os dados dos deputados federais desta legislatura e das seguintes, caso ele
#' tenha sidoreeleito e se encontre em exercício.
#' Caso, além do \emph{id} da \emph{Legislatura}, a \emph{data inicial} e a
#' \emph{data final} sejam passadas, a função retornará só os dados dos deputados
#' que atendam aos três critérios.
#'
#' @name extrair_deputados_legislatura
#' @param url_base A base da URL do JSON que contém os dados básicos dos deputados.
#' @param idLegislatura Identificador único da legislatura; caso não informado,
#' retorna os deputados em exercício.
#' @param dataInicio Data início; caso não informado, é ignorado.
#' @param dataFim Data fim; caso não informado, é ignorado.
#' @param itens Número de itens retornados por página; padrão = 1000 (mesmo da API).
#' @param ordem Ordem dos dados (crescente ou decrescente).
#' @param ordenarPor Critério de ordenação dos dados.
#' @return Dataframe contendo os dados básicos dos deputados.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' extrair_deputados_legislatura(idLegislatura = 56)

library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

extrair_deputados_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/deputados",
    idLegislatura = NULL,
    dataInicio = NULL,
    dataFim = NULL,
    itens=1000,
    ordem = "ASC",
    ordenarPor = "nome") {

  # Cria uma lista de parâmetros da consulta
  query_params <- list (
    idLegislatura = idLegislatura,
    dataInicio = dataInicio,
    dataFim = dataFim,
    itens = itens,
    ordem = ordem,
    ordenarPor = ordenarPor
  )

  # Remove os parâmetros que são NULL
  query_params <- query_params[!sapply(query_params, is.null)]

  # Constrói a string da query
  query_string <- paste0("?", paste0(names(query_params), "=", query_params, collapse = "&"))

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, query_string)

  tryCatch({
    # Faz a requisição inicial à API
    doc <- jsonlite::fromJSON(url_endpoint)

    # Extrai os links de paginação
    links <- doc$links
    last_link <- as.character(links[links$rel == "last", ]$href)

    # Determina o número de páginas
    n_paginas <- as.numeric(substr(last_link, str_locate(last_link, "pagina=")[2] + 1, str_locate(last_link, "&itens")[1] - 1))

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
df_deputados_legislaturas <- extrair_deputados_legislatura(idLegislatura = 56)

## Teste 2
df_deputados_legislaturas <- extrair_deputados_legislatura(idLegislatura = 56,
                                                           dataInicio = "2019-02-01",
                                                           dataFim = "2023-01-31")
