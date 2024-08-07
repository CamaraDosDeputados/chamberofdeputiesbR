#' Extrai os dados básicos dos \emph{partidos} de uma \emph{Legislatura}
#' específica da Câmara dos Deputados.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
#' os dados básicos dos \emph{partidos políticos} de uma \emph{Legislatura}
#' específica.
#'
#' @name extrair_partidos_legislatura
#' @param url_base A URL do JSON que contém os dados básicos de partidos.
#' @param id_legislatura O ID da legislatura. Se nada for passado, o padrão é a
#' legislatura atual.
#' @param ordem A ordem de classificação dos resultados.
#' @param criterio_ordenacao O critério de classificação dos resultados.
#' @param itens O número de itens por página.
#' @return Um dataframe contendo os dados básicos do partido.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_partidos <- extrair_partidos_legislatura()

library(jsonlite)
library(tibble)

extrair_partidos_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/partidos",
    id_legislatura = "57",
    ordem = "ASC",
    criterio_ordenacao = "sigla",
    itens = 50) {

  url_endpoint <- paste0(url_base, "?idLegislatura=", id_legislatura,
                         "&ordem=", ordem, "&ordenarPor=", criterio_ordenacao,
                         "&itens=", itens)

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
df_partidos <- extrair_partidos_legislatura()
