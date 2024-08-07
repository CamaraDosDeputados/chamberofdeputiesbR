#' Extrai os dados básicos de todas as legislaturas disponíveis
#'
#' Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos de todas as legislaturas disponíveis.
#'
#' @author Marcus Chevitarese
#'
#' @name extrair_legislaturas
#' @param url_base A base da URL do JSON que contém os dados básicos das legislaturas.
#' @param ordem Ordem dos dados das legislaturas.
#' @param ordenarPor Critério de ordenação dos dados das legislaturas.
#' @return Um dataframe contendo os dados básicos das legislaturas.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' extrair_legislaturas()

library(jsonlite)
library(stringr)
library(purrr)
library(dplyr)

extrair_legislaturas <- function(url_base = "https://dadosabertos.camara.leg.br/api/v2/legislaturas",
                                 ordem = "DESC",
                                 ordenarPor = "id") {

  # Cria uma lista de parâmetros da query
  query_params <- list(
   ordem = ordem,
   ordenarPor = ordenarPor
  )

  # Constrói a string da query
  query_string <- paste0("?", paste0(names(query_params), "=", query_params,
                                     collapse = "&"))

  # Constrói a URL final do endpoint
  url_endpoint <- paste0(url_base, query_string)

  tryCatch({
    # Faz a requisição inicial à API
    doc <- jsonlite::fromJSON(url_endpoint)

    # Extrai os links de paginação
    links <- doc$links
    last_link <- as.character(links[links$rel == "last", ]$href)

    # Determina o número de páginas
    n_paginas <- as.numeric(substr(last_link,
                                   str_locate(last_link, "pagina=")[2] + 1,
                                   str_locate(last_link, "&itens")[1] - 1))

    # Itera sobre as páginas e junta os resultados usando map_dfr
    dados <- map_dfr(1:n_paginas, function(pagina) {
      url_endpoint_pag <- paste0(url_endpoint, "&pagina=", pagina)
      doc_pag <- fromJSON(url_endpoint_pag)
      return(doc_pag$dados)
    })

    # Retorna os dados como um tibble
    return(tibble::as_tibble(dados))

    }, error = function(e) {
    # Em caso de erro, exibe a mensagem de erro
    message("Falha ao acessar a API: ", e$message)
    return(NULL)
  })

}

#teste
df_legislaturas <- extrair_legislaturas()
