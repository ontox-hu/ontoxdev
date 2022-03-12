
#' render the ontox-dev static site
#' @param title the title to give the site
render_site = function(title="Ontox-Dev"){
  rmarkdown::render_site()
  # TODO can do some site modifications below
  root = xml2::read_html("./docs/index.html")

  m_text <- function(node,text){ xml2::xml_text(node) <- text }
  m_att  <- function(node,att,value){ xml2::xml_attr(node,att) <- value }

  update_title <- function(){
    xml2::xml_find_all(root,"//title") |> m_text(title)
    xml2::xml_find_all(root,"//meta[@property='og:title']") |> m_att("content",title)
    xml2::xml_find_all(root,"//meta[@property='twitter:title']") |> m_att("content",title)
  }

  update_title()
  xml2::write_xml(root,"./docs/index.html")
}
