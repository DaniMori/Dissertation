library(knitr)

render_response_scale <- function() {
  
  GS.RESPONSE.SCALE <- c(
    "Muy en desacuerdo", "En desacuerdo", "Neutral",
    "De acuerdo", "Muy de acuerdo"
  )
  
  1:5 %>% t %>% kable(col.names = GS.RESPONSE.SCALE, align = 'c')
}

notes <- function(...) {
  
  tag("aside", varArgs = list(class = "notes", ...))
}

