#' Extrai os dados dos \emph{membros} de um dado \emph{partido político} na
#' Câmara dos Deputados.
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados dos
#' \emph{membros} de um dado \emph{partido político}, a partir de seu \emph{id}.
#'
#' Usar em conjunto com a função \emph{extrair_partidos()}, para obter o
#' \emph{id} do \emph{partido}.
#'
#' @name extrair_partidos_membros
#' @param url_base A base da URL do JSON que contém os dados básicos dos membros de um partido.
#' @param id Argumento que identifica um partido político.
#' @return Um dataframe contendo os dados dos membros do partido político.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_partido_membros <- extrair_partido_membros(id = 37906)

library(jsonlite)
library(tibble)

extrair_partido_membros <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/partidos",
    id = NULL) {

  url_endpoint <- paste0(url_base, "/", id, "/membros")

  tryCatch({
    doc_json <- jsonlite::fromJSON(url_endpoint)
    dados <- tibble::as_tibble(doc_json$dados)
    return(dados)

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)

  })

}

#teste
df_partido_membros <- extrair_partido_membros(id = 37906)
