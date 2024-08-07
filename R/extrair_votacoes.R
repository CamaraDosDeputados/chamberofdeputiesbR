#' Extrai informações básicas sobre as votações ocorridas em eventos nos órgãos
#' da Câmara dos Deputados.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' das votações ocorridas em órgãos da Câmara dos Deputados.
#'
#' @name extrair_votacoes
#' @param url_base A base da URL do JSON que contém os dados das votações.
#' @param id Identificador único da votação
#' @param idProposicao Identificador único da proposição.
#' @param idOrgao Identificador único do órgão.
#' @param dataInicio Data início. Caso não informado o intervalo de pesquisa,
#' são retornados dados de todas as votações ocorridas nos últimos 30 dias,
#' em eventos de todos os órgãos.
#' Quando informado intervalo, é necessário que as datas sejam de um mesmo ano.
#' Quando apenas uma delas está presente, são retornadas somente as votações
#' ocorridas no mesmo ano, antes de dataFim ou após dataInicio.
#' @param dataFim Data fim.
#' @param ordenarPor Critério de ordenação dos dados.
#' @param ordem Ordem dos dados (crescente ou decrescente).
#' @param itens Número de itens retornados por página; padrão = 1000 (mesmo da API).
#' @return Dataframe contendo os dados básicos dos membros do partido.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' df_votacoes <- extrair_votacoes(dataInicio = "2024-06-30")

library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(purrr)

extrair_votacoes <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/votacoes",
    id = NULL,
    idProposicao = NULL,
    idOrgao = NULL,
    dataInicio = NULL,
    dataFim = NULL,
    ordenarPor = "id",
    ordem = "desc",
    itens = 1000) {

  # Cria uma lista de parâmetros da consulta
  query_params <- list(
    id = id,
    idProposicao = idProposicao,
    idOrgao = idOrgao,
    dataInicio = dataInicio,
    dataFim = dataFim,
    ordenarPor = ordenarPor,
    ordem = ordem,
    itens = itens
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

    # Verifica se há links de paginação
    if (!is.null(doc$links)) {
      links <- doc$links
      last_link <- as.character(links[links$rel == "last", ]$href)

      # Determina o número de páginas
      n_paginas <- as.numeric(substr(last_link,
                                     str_locate(last_link, "pagina=")[2] + 1,
                                     str_locate(last_link, "&itens")[1] - 1))

      # Itera sobre as páginas e junta os resultados usando map_dfr
      dados <- map_dfr(1:n_paginas, function(pagina) {
        url_endpoint_pag <- paste0(url_endpoint, "&pagina=", pagina)
        doc_pag <- jsonlite::fromJSON(url_endpoint_pag)
        return(doc_pag$dados)
      })
    } else {
      # Se não houver paginação, retorna os dados diretamente
      dados <- doc$dados
    }

    # Retorna os dados como um tibble
    return(tibble::as_tibble(dados))

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })
}

# Teste
df_votacoes <- extrair_votacoes(dataInicio = "2024-06-30")

