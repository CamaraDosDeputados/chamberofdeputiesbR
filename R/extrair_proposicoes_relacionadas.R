#' Extrai dados das \emph{proposições legislativas} da Câmara dos Deputados
#' associadas a uma dada proposição.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e retorna
#' uma lista de informações básicas sobre proposições que de alguma forma se
#' relacionam com a proposição identificada por {id} (pareceres, requerimentos,
#' substitutivos, etc.).
#'
#' @name extrair_proposicoes_relacionadas
#' @param url_base A base da URL do JSON que contém os dados das proposições.
#' @param id Identificadores de uma ou mais proposições, separados por vírgulas.
#' @return Um dataframe contendo os dados das proposições ou uma mensagem de
#' erro se a consulta falhar.
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_proposicoes_relacionadas <- extrair_proposicoes_relacionadas(id = "18156")

library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)

extrair_proposicoes_relacionadas <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/proposicoes",
    id = NULL) {

  # Cria uma lista de parâmetros da query
  query_params <- list(
    id = id
  )

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, "/", id, "/", "relacionadas")

  tryCatch({

    # Faz a requisição inicial à API e extrai os dados
    doc_json <- jsonlite::fromJSON(url_endpoint)
    dados <- doc_json$dados

    # Verifica e substitui colunas que são NULL por NA
    dados <- purrr::map(dados, ~ modify_if(.x, is.null, ~ NA))

    # Converte a lista de listas em um data.frame
    dados_df <- tibble::as_tibble(bind_rows(dados))

    return(dados_df)

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)

    return(NULL)

  })

}

# Teste
df_proposicoes_relacionadas <- extrair_proposicoes_relacionadas(id = "18156")
