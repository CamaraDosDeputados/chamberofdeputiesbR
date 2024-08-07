#' Extrai os blocos partidários da Câmara dos Deputados por \emph{id}.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos dos blocos partidários
#' a partir de um \emph{id} específico.
#'
#' @name extrair_blocos_id
#' @param url_base Identificador único do órgão.
#' @param id A URL do JSON que contém os dados básicos dos blocos partidários, a partir de seu id.
#' @return Um dataframe contendo os dados básicos do bloco partidário.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_blocos_id <- extrair_blocos_id(id = 584)

library(jsonlite)
library(tibble)

extrair_blocos_id <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/blocos",
    id) {

  url_endpoint <- paste0(url_base, "/", id)
  doc_json <- jsonlite::fromJSON(url_endpoint)
  dados <- doc_json$dados

  # Extrai os dados e transforma em um tibble
  dados <- tibble::as_tibble(doc_json$dados)

  return(dados)
}

#teste
df_blocos_id <- extrair_blocos_id(id = 584)
