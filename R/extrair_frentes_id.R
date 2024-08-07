#' Extrai os dados básicos das \emph{frentes parlamentares} da Câmara dos
#' Deputados por \emph{id}.
#'
#' Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
#' os dados básicos de \emph{frentes parlamentares} a partir de um \emph{id}
#' específico.
#'
#' @name extrair_frentes_id
#' @param url_base A URL do JSON que contém os dados básicos de frentes parlamentares, a partir de seu id.
#' @param id Identificador único da frente parlamentar.
#' @return Um dataframe contendo os dados básicos da frente parlamentar.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' df_frentes <- extrair_frentes_id(id = 55640)

library(tibble)
library(tidyr)
library(jsonlite)

extrair_frentes_id <- function(
    url_base = "https://dadosabertos.camara.leg.br/api/v2/frentes",
    id = NULL) {

    url_endpoint <- paste0(url_base, "/", id)

  tryCatch({
    doc_json <- jsonlite::fromJSON(url_endpoint)
    dados <- doc_json$dados

    # identifica elementos que são do tipo lista
    index <- unname(unlist(lapply(dados, is.list)))

    # constrói um dataframe intermediário com elementos que *não* são do tipo lista
    df_frentes_1 <- dados[!index] %>%
      replace(lengths(.) == 0, NA) %>%
      setNames(names(dados[!index])) %>%
      tibble::as_tibble()

    # constrói um dataframe intermediário com elementos que *são* do tipo lista
    df_frentes_2 <- dados[[names(dados[index])]] %>%
      replace(lengths(.) == 0, NA) %>%
      setNames(paste0(names(dados[index]), ".", names(dados[[names(dados[index])]]))) %>%
      tibble::as_tibble()

    # une os dataframes intermediários pela coluna
    df_frentes <- cbind(df_frentes_1, df_frentes_2)

    return(df_frentes)

    }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })

}

#teste novo
df_frentes <- extrair_frentes_id(id = 55640)
