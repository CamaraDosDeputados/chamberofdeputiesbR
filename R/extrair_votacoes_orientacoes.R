#' Extrai os dados das votações segundo as orientações dos partidos políticos,
#' blocos parlamentares, líder do Governo, Oposição, Maioria e Minoria.
#'
#' Uma liderança também pode orientar para liberar a bancada (para que cada
#' deputado vote como quiser), ou entrar em obstrução (parlamentares não são
#' contados para o quórum de votação), ou ainda podem votar sim ou não.
#' Atualmente, só existem dados de orientação de votação para as votações em
#' \emph{Plenário}.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados das
#' orientações dos partidos políticos, blocos e bancadas de Governo, Oposição,
#' Maioria e Minoria.
#'
#' @name extrair_votacoes_orientacoes
#' @param url_base A base da URL do JSON que contém os dados das votações.
#' @param id Identificador único da votação.
#' @return Dataframe contendo os dados básicos das votações.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' df_votacoes_orientacoes <- extrair_votacoes_orientacoes(id = "994689-78")

library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)

extrair_votacoes_orientacoes <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/votacoes",
    id) {

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, "/", id, "/", "orientacoes")

  tryCatch({

    # Faz a requisição inicial à API e extrai os dados
    doc_json <- fromJSON(url_endpoint)
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
df_votacoes_orientacoes <- extrair_votacoes_orientacoes(id = "994689-78")
