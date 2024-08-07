#' Extrai informações básicas sobre como cada parlamentar votou em uma votação
#' nominal e aberta.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
#' dos votos ocorridos nas \emph{votações} em órgãos da Câmara dos Deputados,
#' por meio do \emph{id} da votação.
#'
#' @name extrair_votacoes_id
#' @param url_base A base da URL do JSON que contém os dados das votações.
#' @param id Identificador único da votação.
#' @return Dataframe contendo os dados básicos das votações.
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_votacoes_id <- extrair_votacoes_id(id = "994689-78")

library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)

extrair_votacoes_id <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/votacoes",
    id = NULL) {

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, "/", id)

  tryCatch({

    # Faz a requisição inicial à API e extrai os dados
    doc_json <- fromJSON(url_endpoint, flatten = TRUE)
    dados <- doc_json$dados

    # Verifica e substitui colunas que são NULL por NA
    dados <-  purrr::map(dados, ~ modify_if(.x, is.null, ~ NA))

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
df_votacoes_id <- extrair_votacoes_id(id = "994689-78")
