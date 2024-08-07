#' Extrai os membros de uma frente parlamentar da Câmara dos Deputados.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
#' os dados básicos dos membros de uma \emph{frente parlamentar} de uma
#' \emph{legislatura} específica.
#'
#' @name extrair_membros_frentes
#' @param url_base A URL do JSON que contém os dados básicos dos membros de uma frente parlamentar.
#' @param id Identificador único da uma frente parlamentar; caso não informado, retorna as frentes
#' de todas as legislaturas.
#' @return Um dataframe contendo os dados básicos da frente.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_membros_frentes <- extrair_membros_frentes(id = 55628)

library(jsonlite)
library(tibble)

extrair_membros_frentes <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/frentes/",
    id = NULL) {

  url_endpoint <- paste0(url_base, "/", id, "/membros")

  tryCatch({
    # Faz a requisição inicial à API
    doc_json <- jsonlite::fromJSON(url_endpoint)

    # Extrai os dados do documento JSON
    dados <- tibble::as_tibble(doc_json$dados)

    # Retorna dados extraídos com um "tibble"
    return(dados)

  }, error = function(e) {

    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)

    return(NULL)

  })

}

#teste
df_membros_frentes <- extrair_membros_frentes(id = 55628)
