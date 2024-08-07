#' Extrai os dados básicos de uma \emph{Legislatura} específica.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e
#' extrai os dados básicos de uma \emph{Legislatura} específica.
#
#' @name extrair_legislatura
#' @param url_base A base da URL do JSON que contém os dados básicos das
#' legislaturas.
#' @param id_legislatura O identificador da legislatura. Se nenhuma for passada,
#' retorna os dados da legislatura atual.
#' @return Um dataframe contendo os dados básicos de uma legislatura específica
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' df_legislatura <- extrair_legislatura()

library(jsonlite)

extrair_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/legislaturas",
    id_legislatura = 57) {

  url_endpoint <- paste0(url_base, "/", id_legislatura)

  tryCatch({
    doc_json <- jsonlite::fromJSON(url_endpoint)
    dados <- as.data.frame(doc_json$dados)

    return(dados)

  }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)

    return(NULL)

  })

}

#teste
df_legislatura <- extrair_legislatura()

