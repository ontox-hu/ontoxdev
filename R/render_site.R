
#' render the ontox-dev static site
#' @param title the title to give the site
render_site = function(title="Ontox-Dev"){
  rmarkdown::render_site()

  root = xml2::read_html("./docs/index.html")

  m_text <- function(node,text){ xml2::xml_text(node) <- text }           # modify node text
  m_att  <- function(node,att,value){ xml2::xml_attr(node,att) <- value } # modify attribute

  # copy_html    <- function(){ # each article should live in a folder w/ an index.html
  #   inputs  = list.files("_posts",pattern="*.html",recursive = T,full.names = F)
  #   outputs = inputs |> map_chr(glue::glue)
  # }

  update_title <- function(){
    xml2::xml_find_all(root,"//title") |> m_text(title)
    xml2::xml_find_all(root,"//meta[@property='og:title']") |> m_att("content",title)
    xml2::xml_find_all(root,"//meta[@property='twitter:title']") |> m_att("content",title)
  }

  update_title()
  xml2::write_xml(root,"./docs/index.html")
}

render_article = function(file, pkg = ".", data = list(), lazy = FALSE, quiet = TRUE){
  # TODO we should copy pkgdown::build_article rather than distill
  library(pkgdown)
  pkg <- as_pkgdown(pkg)
  vig <- match(name, pkg$vignettes$name)
  if (is.na(vig)) {
    stop("Can't find article called ", src_path(name), call. = FALSE)
  }
  input <- pkg$vignettes$file_in[vig]
  output_file <- pkg$vignettes$file_out[vig]
  depth <- pkg$vignettes$depth[vig]
  input_path <- path_abs(input, pkg$src_path)
  output_path <- path_abs(output_file, pkg$dst_path)
  if (lazy && !out_of_date(input_path, output_path)) {
    return(invisible())
  }
  pkgdown:::local_envvar_pkgdown(pkg)
  local_options_link(pkg, depth = depth)
  front <- rmarkdown::yaml_front_matter(input_path)
  # front_opengraph <- check_open_graph(front$opengraph %||% list())
  # data$opengraph <- utils::modifyList(data$opengraph %||% list(), front_opengraph)
  # ext <- purrr::pluck(front, "pkgdown", "extension", .default = "html")
  # as_is <- isTRUE(purrr::pluck(front, "pkgdown", "as_is"))

  default_data <- list(pagetitle   = front$title,
                       pengraph    = list(description = front$description),
                       source      = repo_source(pkg, path_rel(input,pkg$src_path)),
                       filename    = path_file(input),
                       output_file = output_file,
                       as_is       = as_is)

  data <- utils::modifyList(default_data, data)
  if (as_is) {
    format <- NULL
    if (identical(ext, "html")) {
      data$as_is <- TRUE
      template <- rmarkdown_template(pkg, "article", depth = depth, data = data)
      output   <- rmarkdown::default_output_format(input_path)
      options  <- list(template = template$path, self_contained = FALSE)
      if (output$name != "rmarkdown::html_vignette") {
        options$theme <- output$options$theme
      }
    }
    else {
      options <- list()
    }
  }
  else {
    format <- build_rmarkdown_format(pkg = pkg, name = "article",
                                     depth = depth, data = data, toc = TRUE)
    options <- NULL
  }
  render_rmarkdown(pkg, input = input, output = output_file,
                   output_format = format, output_options = options, quiet = quiet)
}
