#' html describing all of the ontox-dev posts
#' @import purrr
#' @return
#' @export
post_description <- function(){
  rmd2yaml = purrr::compose(~ yaml::read_yaml(text = .),
                            ~ paste(., collapse="\n"),
                            ~ head_while(.,~ !grepl("---",.)),
                            ~ readr::read_lines(.,skip=1))

  posts     = list.files(path = "_posts",pattern = "*.Rmd",recursive = T,full.names = T) |>
    map( ~ list(yml=rmd2yaml(.),file = .)) |>
    keep(~ all(c("title","description") %in% names(.$yml)))

  post2link = function(file,node){
    file = gsub("(^|/)_","",file) |> gsub(pattern = "/[^/]*$",replacement = "")
    tags$a(href=file,node)
  }

  post2card = function(title,description,file,modified,...){
    div(post2link(file,h3(title,style="margin-bottom:2px;margin-top:2px;color:#1a5b7a")),
        div(tags$small("modified",tags$b(modified))),
        div(description,style="margin-left:5px,margin-top:4px"))
  }

  ptbl = posts |> transpose() |> as_tibble() |> unnest_wider(yml) |>
    dplyr::mutate(modified = map(file,~ file.info(.)$mtime |> lubridate::as_datetime())) %>%
    dplyr::mutate(card = pmap(.,post2card)) |>
    dplyr::select(title,description,modified,card) |> unnest(!card) |>
    dplyr::arrange(desc(modified))

  map(ptbl$card,~ tags$div(.,tags$br())) |> tags$div(class="display-block")
}
