#' Extrair os dados dos membros da \emph{Mesa-Diretora} da Câmara dos Deputados,
#' em uma dada \emph{Legislatura}.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
#' os dados básicos dos membros da \emph{Mesa-Diretora}, em uma dada
#' \emph{Legislatura}.
#'
#' @name extrair_mesa_legislatura
#' @param url_base A base da URL do JSON que contém os dados básicos das legislaturas.
#' @param id_legislatura Argumento que identifica a Legislatura. Caso não seja
#' informado, a função retorna os dados da Legislatura atual.
#' @return Um dataframe contendo os dados dos membros da Legislatura.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_mesa <- extrair_mesa_legislatura()

library(jsonlite)
library(tibble)

extrair_mesa_legislatura <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/legislaturas",
    id_legislatura = 57) {

  url_endpoint <- paste0(url_base, "/", id_legislatura, "/mesa")

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
df_mesa <- extrair_mesa_legislatura()
