library(tidyverse)
library(magrittr)
library(knitr)
library(htmltools)
library(ggplot2)
library(plotly)
library(assertive)


POLARITIES <- c("-", "+")

init_ggplot <- function(theme = theme_minimal()) {

  theme_set(theme)
}

plotly_conf <- function(plotly_obj, zoom = FALSE) {
  
  plotly_obj %>% config(displayModeBar = FALSE, mathjax = "cdn") %>%
    layout(
      xaxis = list(
        showspikes = TRUE, spikethickness = .5, spikedash = "dot",
        fixedrange = !zoom
      ),
      yaxis = list(
        showspikes = TRUE, spikethickness = .5, spikedash = "dot",
        fixedrange = !zoom
      ),
      margin = list(l = 20, r = 20, t = 25, b = 20)
    )
}

render_response_scale <- function() {
  
  GS.RESPONSE.SCALE <- c(
    "Muy en desacuerdo", "En desacuerdo", "Neutral",
    "De acuerdo", "Muy de acuerdo"
  )
  
  1:5 %>% t %>% kable(col.names = GS.RESPONSE.SCALE, align = 'c')
}

render_FC_block <- function(option_1, option_2) {
  
  c(option_1, option_2) %>% t %>% kable(col.names = 1:2, align = 'c')
}

main_layout <- function(title, subtitle = NULL) {
  
  tagList(
    br(),
    br(),
    br(),
    br(),
    h1(title),
    br(),
    h2(subtitle)
  )
}

irf_2PL <- function(theta, a, b) {
  
  logit <- theta * a - b
  
  ir_probability <- 1 / (1 + exp(-logit))
  
  return(ir_probability)
}

parse_MUPP_2PL <- function(block, dim_names = c("theta_1", "theta_2")) {
  
  assert_is_character(dim_names)
  
  assert_is_subset(
    c("a1", "a2", "l", "pol1", "pol2", "dim1", "dim2"),
    block %>% names
  )
  
  assert_is_numeric(block$a1)
  assert_is_numeric(block$a2)
  assert_is_numeric(block$l)
  assert_is_character(block$pol1)
  assert_is_character(block$pol2)
  assert_is_character(block$dim1)
  assert_is_character(block$dim2)
  
  assert_all_are_positive(block %$% c(a1, a2))
  assert_is_subset(block$pol1, POLARITIES)
  assert_is_subset(block$pol2, POLARITIES)
  assert_is_subset(block$dim1, dim_names)
  assert_is_subset(block$dim2, dim_names)
}

prob_MUPP_2PL <- function(theta, block, parse_block = TRUE) {
  
  assert_is_a_bool(parse_block)
  if(parse_block) block %>% parse_MUPP_2PL(theta %>% names)
  assert_are_set_equal(block %>% nrow, 1L)

  if(block$pol1 == "-") block$a1 = -block$a1
  if(block$pol2 == "-") block$a2 = -block$a2
  
  logit <- block %$% {a1 * theta[[dim1]] - a2 * theta[[dim2]] + l}
  
  prob <- 1 / (1 + exp(-logit))
  
  return(prob)
}

brf_MUPP_2PL <- function(
  block, axis = seq(-3, 3, .1), dims = c("theta_1", "theta_2"),
  parse_block = TRUE
) {
  assert_is_a_bool(parse_block)
  if(parse_block) block %>% parse_MUPP_2PL(dim_names = dims)
  
  assert_is_numeric(axis)
  
  assert_is_character(dims)
  assert_is_of_length(dims, 2)
  

  theta_space <- axis %>% tibble(theta_1 = ., theta_2 = .) %>%
    expand(theta_1, theta_2) %>% setNames(dims)
  
  
  probs <- block %>% prob_MUPP_2PL(theta_space, ., parse_block = FALSE)
  
  dim(probs) <- axis %>% length %>% rep(2)
  dimnames(probs) <- axis %>% round(1) %>% list(., .)
  
  return(probs)
}

plot_MUPP_2PL_brf <- function(
  block, axis = seq(-3, 3, .1), parse_block = TRUE,
  dims = c("theta_1", "theta_2"), pov = c(x = -0.005, y = -.6, z = 2.5)
) {
  assert_is_a_bool(parse_block)
  if(parse_block) block %>% parse_MUPP_2PL(dim_names = dims)
  
  assert_are_set_equal(block %>% nrow, 1, severity = "warning")
  block <- block %>% slice(1)
  
  assert_is_character(dims)
  assert_is_of_length(dims, 2)
  
  assert_is_numeric(pov)
  assert_is_of_length(pov, 3)
  assert_has_names(pov)
  assert_are_set_equal(pov %>% names, c("x", "y", "z"))
  
  
  MUPP_2PL_probs <- block %>%
    brf_MUPP_2PL(axis = axis, dims = dims, parse_block = FALSE)

  hover_tags <- paste0(
    "P<sub><em>i</em></sub>(<em>Y<sub>ij</sub></em> = 1 | ",
    MUPP_2PL_probs %>% rownames, ", ",
    MUPP_2PL_probs %>% colnames, ") = ",
    MUPP_2PL_probs %>% round(3)
  )
  dim(hover_tags) <- dim(MUPP_2PL_probs)
  
  brf_plot <- plot_ly(
    x = ~rownames(MUPP_2PL_probs), y = ~colnames(MUPP_2PL_probs),
    z = ~MUPP_2PL_probs,
    # text = ~paste0(TeX("\\theta_1"), ": ", rownames(MUPP_2PL_probs)),
    text = ~hover_tags,
    type = "surface", colorscale = NA, opacity = .9, showscale = FALSE,
    hoverinfo = "text"
  ) %>% layout(
    scene = list(
      xaxis = list(title = HTML("\u03B81")),
      yaxis = list(title = HTML("\u03B82")),
      zaxis = list(
        title = "" #TeX("\\textrm{P}_i(Y_{ij} = 1 | {\\bf\\unicode[Times]{x3B8}})")
      ),
      camera = list(
        eye = pov %>% as.list
      )
    )
  ) %>% plotly_conf
  
  return(brf_plot)
}

#' Plot a MUPP-2PL item as a vector in a 2-dimensional space.
#'
#' @param blocks MUPP-2PL block parameters, given as a data.frame with 3 (double) columns: scale.1, scale.2, location
#' @param cor double 1-length vector with the correlation of the latent space
#'
#' @return a ggplot of the item
multidim_params_MUPP_2PL <- function(
  blocks, cor, parse_blocks = TRUE,
  dims = blocks %>% get_MUPP_2PL_dim_names
) {
  assert_is_a_bool(parse_blocks)
  if(parse_blocks) blocks %>% parse_MUPP_2PL(dim_names = get_MUPP_2PL_dim_names(.))

  assert_is_numeric(cor)
  
  assert_is_character(dims)
  assert_is_subset(blocks %>% get_MUPP_2PL_dim_names, dims)
  
  assert_is_of_length(dims, 2, severity = "warning")
  dims %<>% extract(1:2)

  items_MCLM <- MUPP2PL_to_MCLM(blocks, dims = dims, parse_blocks = FALSE) %>%
    select(dims, l)
  
  cor_matrix <- matrix(c(1, cor, cor, 1), nrow = 2)
  
  scales <- items_MCLM %>% select(dims) %>% as.matrix
  
  MBS <- scales %>% { . %*% cor_matrix %*% t(.) } %>% diag %>% sqrt
  
  MBL <- if(MBS == 0) 0 else -items_MCLM %>% pull(l) %>% divide_by(MBS)
  
  directions <- {
    if(MBS == 0) 0 %>% rep(dims %>% length) %>% t else (scales %*% cor_matrix) %>%
      divide_by(MBS)
  } %>% as_tibble %>% set_names(dims)
  
  multidim_params <- tibble(MBS, MBL) %>% bind_cols(directions)
  
  return(multidim_params)
}

plot_MUPP_2PL_vectors <- function(
  blocks, cor = 0, parse_blocks = TRUE,
  dims = c("theta_1", "theta_2"), colors = rainbow(blocks %>% nrow)
) {
  assert_is_a_bool(parse_blocks)
  if(parse_blocks) blocks %>% parse_MUPP_2PL(dim_names = get_MUPP_2PL_dim_names(.))
  
  assert_is_a_number(cor)
  
  assert_is_character(dims)
  assert_is_subset(blocks %>% get_MUPP_2PL_dim_names, dims)
  
  assert_is_of_length(dims, 2, severity = "warning")
  dims %<>% extract(1:2)
  
  
  vector_blocks <- blocks %>%
    multidim_params_MUPP_2PL(cor = cor, dims = dims, parse_blocks = FALSE)
  
  coords <- vector_blocks %>%
    transmute_at(dims, funs(origin = . * MBL, end = . * (MBL + MBS)))
  
  rotation <- matrix(c(1, cor, 0, sqrt(1 - cor^2)), nrow = 2) %>% solve
  
  orth_origin <- rotation %>%
    multiply_by_matrix(
      coords %>% select(ends_with("origin")) %>% t
    )
  orth_end <- rotation %>%
    multiply_by_matrix(
      coords %>% select(ends_with("end")) %>% t
    )
  
  coords <- rbind(orth_origin, orth_end) %>% t %>% as_tibble
  
  vector_plot <- coords %>%
    ggplot(mapping = aes(x = V1, y = V2, xend = V3, yend = V4, color = colors)) +
    scale_y_continuous(limits = c(-3, 3)) +
    scale_x_continuous(limits = c(-3, 3)) +
    labs(x = dims[1], y = dims[2]) +
    geom_hline(yintercept = 0) +
    (
      if(cor == 0) geom_vline(xintercept = 0) else
        geom_abline(slope = cor %>% acos %>% tan, intercept = 0)
    ) +
    geom_segment(
      arrow = arrow(angle = 20, length = unit(0.20, "inches"), type = "closed"),
      size = 2
    ) +
    theme(plot.margin = unit(c(50, 20, 40, 20), "points")) +
    scale_color_discrete(guide = "none") +
    xlab(expression(italic(theta)[1])) + ylab(expression(italic(theta)[2]))

  return(vector_plot)
}

MUPP2PL_to_MCLM <- function(
  blocks, dims = blocks %>% get_MUPP_2PL_dim_names,
  parse_blocks = TRUE
) {
  
  assert_is_a_bool(parse_blocks)
  if(parse_blocks) blocks %>% parse_MUPP_2PL(dim_names = dims)
  
  assert_is_character(dims)
  assert_is_subset(blocks %>% get_MUPP_2PL_dim_names, dims)
  
  blocks %<>% mutate(
    a1 = pol1 %>% equals("+") %>% ifelse(1, -1) %>% multiply_by(a1),
    a2 = pol2 %>% equals("+") %>% ifelse(1, -1) %>% multiply_by(a2)
  ) 

  items_MCLM <- matrix(
    0, nrow = blocks %>% nrow, ncol = dims %>% length,
    dimnames = list(NULL, dims)
  ) %>% as_tibble
  
  items_MCLM %<>% names %>% map(
    ~(
      items_MCLM[[.]] +
        blocks$a1 * (blocks$dim1 == .) -
        blocks$a2 * (blocks$dim2 == .)
    )
  ) %>% set_names(dims) %>% as_tibble %>% bind_cols(blocks %>% select(l))
  
  return(items_MCLM)
}

get_MUPP_2PL_dim_names <- function(blocks) {
  
  dims <- blocks %>% select(dim1, dim2) %>% as.matrix
  dim(dims) <- NULL
  dims %<>% unique
  
  return(dims)
}
