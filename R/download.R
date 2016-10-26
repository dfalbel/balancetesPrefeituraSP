#' URL
#'
#'
url <- function(){
  "http://www.prefeitura.sp.gov.br/cidade/secretarias/financas/contaspublicas/index.php?p=3216"
}

#' Listar Balancetes
#'
#' @param url url da página com a lista dos balancetes.
#' @return vetor com todos os balancetes.
#'
listar_balancetes <- function(url){
  httr::GET(url) %>%
    httr::content(as = "parsed") %>%
    rvest::html_nodes("td a") %>%
    rvest::html_attr("href")
}

#' Baixar pdf
#'
#' Essa função já faz um parse inicial para colocar o nome no arquivo.
#'
#' @param url_bal url do balancete em pdf
#' @param folder caminho da pasta para salvar o balancete
#'
baixar_pdf <- function(url_bal, folder){
  nome <- stringr::str_split(url_bal, "/") %>% unlist() %>% dplyr::last()
  httr::GET(url_bal, httr::write_disk(sprintf("%s/%s", folder, nome)))
}

#' Renomear PDF'
#'
#'
renomear_pdf <- function(file, folder){
  txt <- pdftools::pdf_text(file)
  nome <- stringr::str_extract(txt[1], pattern = "Boletim.*\\b") %>%
    stringr::str_extract("[^\\s-].*\\b") %>%
    tolower() %>%
    stringr::str_replace_all("\\s", "_") %>%
    stringr::str_replace_all("/", "-")
  file.copy(file, to = sprintf("%s/%s.pdf", folder, nome))
}

#' Baixar Balancetes
#'
#'
baixar_pdfs <- function(urls_bal, folder){
  purrr::map(urls_bal, function(url_bal){
    baixar_pdf(url_bal, folder)
  })
}
