#' Extrai dados do histórico de \emph{tramitação} de
#' \emph{proposições legislativas} da Câmara dos Deputados pelo {id} da
#' proposição.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e
#' retorna dados sobre um estágio da tramitação da proposição, obtido por meio
#' do seu \emph{id}.
#' Esta representação das tramitações é provisória.
#'
#' @name extrair_proposicoes_tramitacoes
#' @param url_base A base da URL do JSON que contém os dados básicos das proposições.
#' @param id Identificadores de uma ou mais proposições, separados por vírgulas.
#' @return Um dataframe contendo os dados das proposições ou uma mensagem de erro se a consulta falhar.
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_proposicoes_tramitacoes <- extrair_proposicoes_tramitacoes(id = "18156")

library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)

extrair_proposicoes_tramitacoes <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/proposicoes",
    id = NULL) {

  # Cria uma lista de parâmetros da query
  query_params <- list(
    id = id
  )

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, "/", id, "/", "tramitacoes")

  tryCatch({

    # Faz a requisição inicial à API e extrai os dados
    doc_json <- jsonlite::fromJSON(url_endpoint)
    dados <- doc_json$dados

    # Verifica e substitui colunas que são NULL por NA
    dados <- purrr::map(dados, ~ modify_if(.x, is.null, ~ NA))

    # Converte a lista de listas em um data.frame
    dados_df <- tibble::as_tibble(dplyr::bind_rows(dados))

    return(dados_df)

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })

}

# Teste
df_proposicoes_tramitacoes <- extrair_proposicoes_tramitacoes(id = "18156")
