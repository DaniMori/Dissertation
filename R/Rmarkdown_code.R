library(tidyverse)
library(magrittr)
library(knitr)
library(kableExtra)
library(MplusAutomation)
library(glue)
library(htmltools)
library(ggplot2)
library(plotly)
library(assertive)
library(rlang)
library(stringr)
library(tidyselect)
library(ggrepel)

source("R/Models.R")
source("R/Bivariate_lognormal_toolbox.R")
# source("R/Functions.R")
# source("R/IRT_ipsative_toolbox.R")


### GLOBAL CONSTANTS: ----------------------------------------------------------

PALETTE.COLORS = c(
  blue   = '#6699FF',
  purple = '#FF66FF',
  green  = '#99FF66',
  orange = '#FF9966',
  yellow = '#FFFF66'
)


## Estudio 2: ------------------------------------------------------------------

ITEMS.DIM <- 12 * 1:3
N.DIMS <- 2:5
A.SCALES.CORR <- .35 * -2:2
# N.REPLICATION <- 1:100
THETA.CORRS <- 0:3 * .3

SCALE.CORR.TITLE <- expression(bold("Correlación entre escalas"))
ITEMS.DIM.TITLE <- "Ítems / dimensión"
CORR.TITLE <- "Corr. total rasgo = "

FIG.PATH <- "res/"
ERROR.C.I <- 1.96

GRAPH.REL.SIZE <- 1


## Estudio 3: ------------------------------------------------------------------

TRAIT.NAMES <- c("Neuroticism", "Extraversion", "Openness", "Agreeableness", "Conscientiousness")

names(TRAIT.NAMES) <- get.model.name.from.trait(trait = BIG.FIVE.TRAITS, abbreviation = TRUE) %>%
  paste0(".BY")

ESTIMATOR <- "ML"

names(BIG.FIVE.TRAITS) <- TRAIT.NAMES
# INVERT.DIMS <- BIG.FIVE.TRAITS[c("Neuroticism", "Agreeableness")]
INVERT.DIMS <- BIG.FIVE.TRAITS[c("Agreeableness")]

POLARITIES <- c("-", "+")


### FUNCTIONS: -----------------------------------------------------------------

#### Introduction: -------------------------------------------------------------

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
      margin = list(l = 20, r = 20, t = 80, b = 20)
    )
}

### Commented block ----
# render_response_scale <- function() {
#   
#   GS.RESPONSE.SCALE <- c(
#     "Muy en desacuerdo", "En desacuerdo", "Neutral",
#     "De acuerdo", "Muy de acuerdo"
#   )
#   
#   1:5 %>% t %>% kable(col.names = GS.RESPONSE.SCALE, align = 'c')
# }
# 
# render_FC_block <- function(option_1, option_2) {
#   
#   c(option_1, option_2) %>% t %>% kable(col.names = 1:2, align = 'c')
# }

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


### Estudio 1: -----------------------------------------------------------------

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

multidim_params_MUPP_2PL <- function(
  blocks, cor_matrix = dims %>% length %>% diag, parse_blocks = TRUE,
  dims = blocks %>% get_MUPP_2PL_dim_names
) {
  assert_is_a_bool(parse_blocks)
  if(parse_blocks) blocks %>% parse_MUPP_2PL(dim_names = get_MUPP_2PL_dim_names(.))

  assert_is_numeric(cor_matrix)
  assert_is_matrix(cor_matrix)
  assert_is_symmetric_matrix(cor_matrix)
  
  assert_is_character(dims)
  assert_is_subset(blocks %>% get_MUPP_2PL_dim_names, dims)
  
  assert_is_of_length(dims, cor_matrix %>% nrow)
  
  dimnames(cor_matrix) <- list(dims, dims)

  items_MCLM <- MUPP2PL_to_MCLM(blocks, dims = dims, parse_blocks = FALSE) %>%
    select(dims, l)
  
  scales <- items_MCLM %>% select(dims) %>% as.matrix
  
  MBS <- scales %>% { . %*% cor_matrix %*% t(.) } %>% diag %>% sqrt
  
  MBL <- (MBS == 0) %>% ifelse(0, -(items_MCLM %>% pull(l)) %>% divide_by(MBS))
  
  directions <- {
    zero_length <- MBS %>% equals(., 0)
    
    (scales %*% cor_matrix) %>% as_tibble %>% set_names(dims) %>%
      mutate_all(~ifelse(zero_length, 0, . / MBS))
  }
  
  multidim_params <- tibble(MBS, MBL) %>% bind_cols(directions)
  
  return(multidim_params)
}

#' Plot a MUPP-2PL item as a vector in a 2-dimensional space.
#'
#' @param blocks MUPP-2PL block parameters, given as a data.frame with 3
#'     (double) columns: scale.1, scale.2, location
#' @param cor double 1-length vector with the correlation of the latent space
#'
#' @return a ggplot of the item
plot_MUPP_2PL_vectors <- function(
  blocks, cor = 0, parse_blocks = TRUE,
  dims = c("theta_1", "theta_2"), colors = "", alpha = 1, thickness = 1.5
) {
  assert_is_a_bool(parse_blocks)
  if(parse_blocks) blocks %>% parse_MUPP_2PL(dim_names = get_MUPP_2PL_dim_names(.))
  
  assert_is_a_number(cor)
  
  assert_is_character(dims)
  assert_is_subset(blocks %>% get_MUPP_2PL_dim_names, dims)
  
  assert_is_of_length(dims, 2, severity = "warning")
  dims <- dims[1:2]
  
  cor_matrix <- matrix(c(1, cor, cor, 1), nrow = 2)
  
  vector_blocks <- blocks %>%
    multidim_params_MUPP_2PL(cor_matrix = cor_matrix, dims = dims, parse_blocks = FALSE)
  
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
      arrow = arrow(angle = 20, length = unit(0.10, "inches"), type = "closed"),
      size = thickness, alpha = alpha
    ) +
    theme(plot.margin = unit(c(50, 20, 40, 20), "points")) +
    scale_color_manual(values = PALETTE.COLORS[c(2, 1)] %>% unname, guide = "none") +
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


### Estudio 2: -----------------------------------------------------------------

error.bar <- function(x, y, upper, lower = upper, length = 0.03, ...) {
  
  if(
    length(x) != length(y) |
    length(y) !=length(lower) |
    length(lower) != length(upper)
  )
    stop("vectors must be same length")
  
  arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
}

index.names <- outer(c("mean", "sd"), c("rt", "ls"), paste, sep = ".")
dim(index.names) <- NULL


get.result <- function(
  type = c("index", index.names),
  items.dim = ITEMS.DIM, n.dims = N.DIMS, a.scales.corr = A.SCALES.CORR
) {
  
  type <- match.arg(type, several.ok = TRUE)
  
  index <- results$items.dim %in% items.dim & results$n.dims %in% n.dims &
    results$a.scales.corr %in% a.scales.corr
  
  if(identical("index", type)) return((1:nrow(results))[index])
  
  if("index" %in% type) {
    
    return(cbind(index = 1:nrow(results), results[, type])[index, ])
    
  } else {
    
    return(results[index, type])
  }
}


get.ev <- function(
  type = index.names,
  items.dim = ITEMS.DIM, n.dims = N.DIMS, a.scales.corr = A.SCALES.CORR,
  theta.corrs = THETA.CORRS
) {
  
  type <- match.arg(type, several.ok = TRUE)
  
  index <- results$items.dim %in% items.dim & results$n.dims %in% n.dims &
    results$a.scales.corr %in% a.scales.corr
  
  return(evrs[index, type, as.character(theta.corrs)])
}


# # TODO: function header
# save.plot <- function(
#   file.name, width = 3 * length(ITEMS.DIM),
#   height = 7, call, ...
# ) {
#   
#   # Open the file, setting the canvas properties
#   png(
#     paste0(FIG.PATH, file.name, ".png"),
#     width = width, height = height, units = "in", res = 600, type = "cairo-png"
#   )
#   
#   tryCatch(
#     call(...), # Call the plot function
#     finally = dev.off() # Close the file
#   )
# }

plot.svr.conditions <- function(
  type = c('ls', 'rt'), plot.err.bars = TRUE,
  ylim = range(get.result(paste("mean", type, sep = '.')), na.rm = TRUE),
  ylab = expression(italic("LSV")),
  dim.code = c("color", "symbol", "line")
) {
  
  type <- match.arg(type)
  dim.code <- match.arg(dim.code, several.ok = TRUE)
  
  n_item_conds <- length(ITEMS.DIM)
  
  par(mfrow = c(1, n_item_conds))
  
  for(items in seq_len(n_item_conds)) {
    
    par(mar = c(5, 6, 4, 0.5), family = "serif")
    
    plot(
      A.SCALES.CORR,
      get.result(paste("mean", type, sep = '.'), ITEMS.DIM[items], N.DIMS[1]),
      col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[1] else "black",
      pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL[1] else  19,
      lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE[1] else 1,
      type = "o", cex = GRAPH.REL.SIZE,
      ylim = ylim, xlim = range(A.SCALES.CORR),
      ylab = if(items == 1) ylab else "",
      xlab = SCALE.CORR.TITLE,
      xaxp = c(
        A.SCALES.CORR[1],
        A.SCALES.CORR[length(A.SCALES.CORR)],
        length(A.SCALES.CORR) - 1
      ),
      main = paste(ITEMS.DIM[items], ITEMS.DIM.TITLE),
      cex.main = 3, cex.lab = 3, cex.axis = 2.25, mgp = c(4, 1.5, 0)
    )
    
    for (dim in seq_along(N.DIMS)) {
      
      if(dim != 1) {
        
        lines(
          A.SCALES.CORR,
          get.result(
            paste("mean", type, sep = '.'), ITEMS.DIM[items], N.DIMS[dim]
          ),
          col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[dim] else "black",
          pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL[dim] else  19,
          lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE[dim] else 1,
          type = "o", cex = GRAPH.REL.SIZE
        )
      }
      
      if(plot.err.bars) {
        error.bar(
          A.SCALES.CORR,
          get.result(
            paste("mean", type, sep = '.'), ITEMS.DIM[items], N.DIMS[dim]
          ),
          get.result(
            paste("sd", type, sep = '.'), ITEMS.DIM[items], N.DIMS[dim]
          ) * ERROR.C.I / sqrt(length(N.REPLICATION - 1)),
          col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[dim] else "black"
        )
      }
    }
    
    if(items == length(ITEMS.DIM)) {
      
      legend(
        "topright",
        legend = N.DIMS,
        col = if("color" %in% dim.code) N.DIMS.LINE.COLOR else "black",
        pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL else  19,
        lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE else 1,
        lwd = 1.5, cex = 2, pt.cex = 0.7, title = expression(italic(D))
      )
    }
  }
}


plot.evr.conditions <- function(
  type = c('ls', 'rt'), plot.err.bars = TRUE,
  ylim = range(get.ev(index.names[1], theta.corrs = theta.corrs), na.rm = TRUE),
  theta.corrs = THETA.CORRS,
  ylab = expression(italic("LEV")), dim.code = c("color", "symbol", "line")
) {
  
  type <- match.arg(type)
  match.arg(
    as.character(theta.corrs), as.character(THETA.CORRS), several.ok = TRUE
  )
  dim.code <- match.arg(dim.code, several.ok = TRUE)
  
  n_item_conds <- length(ITEMS.DIM)
  
  par(mfrow = c(length(theta.corrs), n_item_conds), oma = c(0, 0, 3.5, 0))
  
  for(corr in theta.corrs) {
    for(items in seq_len(n_item_conds)) {
      
      par(mar = c(5, 6, 4, .5), family = "serif")
      
      plot(
        A.SCALES.CORR,
        get.ev(
          paste("mean", type, sep = '.'),
          ITEMS.DIM[items], N.DIMS[1], theta.corrs = corr
        ),
        col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[1] else "black",
        pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL[1] else  19,
        lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE[1] else 1,
        type = "o", cex = GRAPH.REL.SIZE,
        ylim = ylim, xlim = range(A.SCALES.CORR),
        ylab = if(items == 1) ylab else "",
        xlab = if(
          corr == theta.corrs[length(theta.corrs)]
        ) SCALE.CORR.TITLE else "",
        xaxp = c(
          A.SCALES.CORR[1],
          A.SCALES.CORR[length(A.SCALES.CORR)],
          length(A.SCALES.CORR) - 1
        ),
        main = if(length(theta.corrs) > 1)
          if(items == (n_item_conds %/% 2 + 1)) paste(CORR.TITLE, corr) else ""
        else "", font.main = 1,
        cex.main = 3, cex.lab = 3, cex.axis = 2.25, mgp = c(4, 1.5, 0)
      )
      
      for (dim in seq_along(N.DIMS)) {
        
        if(dim != 1) {
          
          lines(
            A.SCALES.CORR,
            get.ev(
              paste("mean", type, sep = '.'),
              ITEMS.DIM[items], N.DIMS[dim], theta.corrs = corr
            ),
            col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[dim] else "black",
            pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL[dim] else  19,
            lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE[dim] else 1,
            type = "o", cex = GRAPH.REL.SIZE
          )
        }
        
        if(plot.err.bars) {
          
          error.bar(
            A.SCALES.CORR,
            get.ev(
              paste("mean", type, sep = '.'),
              ITEMS.DIM[items], N.DIMS[dim], theta.corrs = corr
            ),
            get.ev(
              paste("sd", type, sep = '.'),
              ITEMS.DIM[items], N.DIMS[dim], theta.corrs = corr
            ) * ERROR.C.I / sqrt(length(N.REPLICATION - 1)),
            col = if("color" %in% dim.code) N.DIMS.LINE.COLOR[dim] else "black"
          )
        }
      }
      
      if(items == length(ITEMS.DIM) & corr == theta.corrs[1]) {
        
        legend(
          "topright",
          legend = N.DIMS,
          col = if("color" %in% dim.code) N.DIMS.LINE.COLOR else "black",
          pch = if("symbol" %in% dim.code) N.DIMS.SYMBOL else  19,
          lty = if("line" %in% dim.code) N.DIMS.LINE.TYPE else 1,
          lwd = 1.5, cex = 2, pt.cex = 0.7, title = expression(italic(D))
        )
      }
    }
    
    if(corr == theta.corrs[1])
      title(
        main = paste(
          ITEMS.DIM, ITEMS.DIM.TITLE,
          collapse = paste0(rep(" ", 30), collapse = "")
        ),
        outer = TRUE, cex.main = 3, adj = .64
      )
  }
}

plot_LEV <- function(data, var_LEV, max_LEV, title) {
  
  var_LEV <- enquo(var_LEV)
  
  plot <- data %>%
    ggplot(
      aes(
        `Correlación entre escalas`, !!var_LEV,
        group = `Nº dimensiones`, color = `Nº dimensiones`,
        text = HTML("<i>LEV</i>:") %>% paste(!!var_LEV %>% round(3))
      )
    ) +
    facet_grid(cols = vars(items.dim)) + geom_point() + geom_line() +
    scale_color_manual(values = PALETTE.COLORS %>% unname) +
    ylab(HTML("<i>LEV</i>")) + ylim(0, max_LEV) + ggtitle(title)
  
  plot %>% ggplotly(tooltip = "text") %>%
    layout(margin = list(r = 140)) %>%
    config(displayModeBar = FALSE)
}


### Estudio 3: -----------------------------------------------------------------

load.LL.ratio.tests <- function(blocks) {

  load("res/LL_ratio_tests.Rdata")

  # Rename appropriately the variables of the LL ratio test tibble
  LL.ratio.tests %<>% select(
    Block = block,
    `Scale 1 LR` = statistic.scale.1, `Scale 1 p-value` = sig.scale.1,
    `Scale 2 LR` = statistic.scale.2, `Scale 2 p-value` = sig.scale.2,
    `Intercept 1 LR` = statistic.th.1, `Intercept 1 p-value` = sig.intercept.th.1,
    `Intercept 2 LR` = statistic.th.2, `Intercept 2 p-value` = sig.intercept.th.2,
    `Intercept 3 LR` = statistic.th.3, `Intercept 3 p-value` = sig.intercept.th.3,
    `Intercept 4 LR` = statistic.th.4, `Intercept 4 p-value` = sig.intercept.th.4
  )

  LL.ratio.tests <- blocks %>% full_join(LL.ratio.tests, by = "Block")

  LL.ratio.tests %<>% mutate_at(
    vars(starts_with("Intercept")),
    ~if_else(LL.ratio.tests$`Item 2` %>% is.na, NA_real_, .)
  )

  return(LL.ratio.tests)
}

load.vars <- function() {

  vars <- read.APRA2.new.items()
  items <- get.instrument.items(vars$items, instrument = APRA2.INSTRUMENT, strings.as.factors = FALSE) %>%
    rownames_to_column(var = "Item") %>%
    select(
      Item, code, trait, facet, polarity, stem
    )
  blocks <- get.instrument.items(vars$blocks, instrument = APRA2.INSTRUMENT, strings.as.factors = FALSE) %>%
    rownames_to_column(var = "Block") %>%
    select(
      Block, code.item.1, code.item.2, `Trait 1` = trait.item.1, `Trait 2` = trait.item.2,
      `Polarity 1` = polarity.item.1, `Polarity 2` = polarity.item.2
    ) %>%
    left_join(items %>% select(`Item 1` = Item, code), by = c(`code.item.1` = "code")) %>%
    left_join(items %>% select(`Item 2` = Item, code), by = c(`code.item.2` = "code")) %>%
    select(-starts_with("code.item"))

  blocks %<>% mutate(
    `Trait 1` = `Trait 1` %>% factor(labels = TRAIT.NAMES),
    `Trait 2` = `Trait 2` %>% factor(labels = TRAIT.NAMES)
  )

  return(list(items = items, blocks = blocks))
}

### Commented block ----
# load.independent.estimates <- function(blocks) {
#   
#   load("output/Final_fit_results_ML.Rdata")
#   
#   item.loading.estimates <- tibble()
#   item.threshold.estimates <- tibble()
#   for(trait in names(gsq.models)) {
#     
#     new.loadings <- gsq.models[[trait]]$loadings[[trait]]
#     
#     item.loading.estimates %<>% bind_rows(
#       new.loadings %>% data.frame %>% rownames_to_column(var = "Item") %>% add_column(Trait = trait)
#     )
#     
#     item.threshold.estimates <- bind_rows(
#       item.threshold.estimates,
#       gsq.models[[trait]]$thresholds %>% data.frame %>% rownames_to_column(var = "collapsed") %>%
#         separate(collapsed, into = c("Item", "Threshold"), sep = "\\$") %>%
#         add_column(Trait = trait)
#     )
#   }
#   
#   item.parameters <- item.loading.estimates %>%
#     rename(`scale estimate` = estimate, `scale sd.err` = std.err) %>%
#     full_join(
#       item.threshold.estimates %>% filter(Threshold == 1) %>%
#         select(Item, `threshold 1 estimate` = estimate, `threshold 1 sd.err` = std.err)
#     ) %>% 
#     full_join(
#       item.threshold.estimates %>% filter(Threshold == 2) %>%
#         select(Item, `threshold 2 estimate` = estimate, `threshold 2 sd.err` = std.err)
#     ) %>% 
#     full_join(
#       item.threshold.estimates %>% filter(Threshold == 3) %>%
#         select(Item, `threshold 3 estimate` = estimate, `threshold 3 sd.err` = std.err)
#     ) %>% 
#     full_join(
#       item.threshold.estimates %>% filter(Threshold == 4) %>%
#         select(Item, `threshold 4 estimate` = estimate, `threshold 4 sd.err` = std.err)
#     )
#   
#   block.parameters <- fcq.model$loadings$estimate %>% data.frame %>% rownames_to_column(var = "Block") %>%
#     rename(`Block scale 1 estimate` = item.1, `Block scale 2 estimate`= item.2) %>%
#     full_join(
#       fcq.model$loadings$std.err %>% data.frame %>% rownames_to_column(var = "Block") %>% 
#         rename(`Block scale 1 sd.err` = item.1, `Block scale 2 sd.err`= item.2)
#     ) %>%
#     full_join(
#       fcq.model$thresholds %>% data.frame %>% rownames_to_column(var = "Block") %>% 
#         rename(`Intercept estimate` = estimate, `Intercept sd.err` = std.err)
#     ) %>%
#     full_join(blocks, .)
#   
#   total.params <- block.parameters %>% 
#     left_join(
#       item.parameters %>% rename_at(vars(matches("(threshold|scale)")), ~str_c("Item 1 ",.)) %>% 
#         select(starts_with("Item")),
#       by  = c(`Item 1` = "Item")
#     ) %>% 
#     left_join(
#       item.parameters %>% rename_at(vars(matches("(threshold|scale)")), ~str_c("Item 2 ",.)) %>% 
#         select(starts_with("Item")),
#       by  = c(`Item 2` = "Item")
#     )
#   
#   total.params %<>% mutate(
#     `Threshold 1 diff estimate` = `Item 1 threshold 1 estimate` - `Item 2 threshold 1 estimate`,
#     `Threshold 2 diff estimate` = `Item 1 threshold 2 estimate` - `Item 2 threshold 2 estimate`,
#     `Threshold 3 diff estimate` = `Item 1 threshold 3 estimate` - `Item 2 threshold 3 estimate`,
#     `Threshold 4 diff estimate` = `Item 1 threshold 4 estimate` - `Item 2 threshold 4 estimate`,
#     `Threshold 1 diff sd.err` = sqrt(`Item 1 threshold 1 sd.err`^2 + `Item 2 threshold 1 sd.err`^2),
#     `Threshold 2 diff sd.err` = sqrt(`Item 1 threshold 2 sd.err`^2 + `Item 2 threshold 2 sd.err`^2),
#     `Threshold 3 diff sd.err` = sqrt(`Item 1 threshold 3 sd.err`^2 + `Item 2 threshold 3 sd.err`^2),
#     `Threshold 4 diff sd.err` = sqrt(`Item 1 threshold 4 sd.err`^2 + `Item 2 threshold 4 sd.err`^2),
#     `Scale 1 diff estimate` = `Block scale 1 estimate` - `Item 1 scale estimate`,
#     `Scale 2 diff estimate` = `Block scale 2 estimate` - `Item 2 scale estimate`,
#     `Scale 1 diff sd.err` = sqrt(`Block scale 1 sd.err`^2 + `Item 1 scale sd.err`^2),
#     `Scale 2 diff sd.err` = sqrt(`Block scale 2 sd.err`^2 + `Item 2 scale sd.err`^2),
#     `Intercept - threshold 1 diff estimate` = `Intercept estimate` - `Threshold 1 diff estimate`,
#     `Intercept - threshold 2 diff estimate` = `Intercept estimate` - `Threshold 2 diff estimate`,
#     `Intercept - threshold 3 diff estimate` = `Intercept estimate` - `Threshold 3 diff estimate`,
#     `Intercept - threshold 4 diff estimate` = `Intercept estimate` - `Threshold 4 diff estimate`,
#     `Intercept - threshold 1 diff sd.err` = sqrt(`Intercept estimate`^2 + `Threshold 1 diff sd.err`^2),
#     `Intercept - threshold 2 diff sd.err` = sqrt(`Intercept estimate`^2 + `Threshold 2 diff sd.err`^2),
#     `Intercept - threshold 3 diff sd.err` = sqrt(`Intercept estimate`^2 + `Threshold 3 diff sd.err`^2),
#     `Intercept - threshold 4 diff sd.err` = sqrt(`Intercept estimate`^2 + `Threshold 4 diff sd.err`^2),
#     `Scale 1 WT` = `Scale 1 diff estimate` / `Scale 1 diff sd.err`,
#     `Scale 2 WT` = `Scale 2 diff estimate` / `Scale 2 diff sd.err`,
#     `Intercept - threshold 1 WT` = `Intercept - threshold 1 diff estimate` / `Intercept - threshold 1 diff sd.err`,
#     `Intercept - threshold 2 WT` = `Intercept - threshold 2 diff estimate` / `Intercept - threshold 2 diff sd.err`,
#     `Intercept - threshold 3 WT` = `Intercept - threshold 3 diff estimate` / `Intercept - threshold 3 diff sd.err`,
#     `Intercept - threshold 4 WT` = `Intercept - threshold 4 diff estimate` / `Intercept - threshold 4 diff sd.err`
#   )
#   
#   return(total.params)
# }
# ----

collapse.scale.params <- function(params, block.scale.1, block.scale.2, item.scale.1, item.scale.2) {

  block.scale.1 <- enquo(block.scale.1)
  block.scale.2 <- enquo(block.scale.2)
  item.scale.1 <- enquo(item.scale.1)
  item.scale.2 <- enquo(item.scale.2)


  all.scale.params <- params %>% transmute(
    Block, Item = `Item 1`, Position = 1L, `Item other` = `Item 2`, Trait = `Trait 1`, `Trait other` = `Trait 2`,
    Polarity = `Polarity 1`, `Polarity other` = `Polarity 2`,
    `Block scale` = UQ(block.scale.1), `Item scale` = UQ(item.scale.1)
  )
  all.scale.params.2 <- params %>% transmute(
    Block, Item = `Item 2`, Position = 2L, `Item other` = `Item 1`, Trait = `Trait 2`, `Trait other` = `Trait 1`,
    Polarity = `Polarity 2`, `Polarity other` = `Polarity 1`,
    `Block scale` = UQ(block.scale.2), `Item scale` = UQ(item.scale.2)
  )
  all.scale.params %<>% bind_rows(all.scale.params.2) %>% mutate(Position = factor(Position))

  return(all.scale.params)
}

### Commented block ----
# joint.estimation.block.item.params.diff <- function(params) {
#   
#   params %>% mutate(
#     `Scale 1 diff` = `Block scale 1` - `Scale item 1`,
#     `Scale 2 diff` = `Block scale 2` - `Scale item 2`,
#     `Intercept - threshold 1 diff` = Intercept - `Threshold 1 diff`,
#     `Intercept - threshold 2 diff` = Intercept - `Threshold 2 diff`,
#     `Intercept - threshold 3 diff` = Intercept - `Threshold 3 diff`,
#     `Intercept - threshold 4 diff` = Intercept - `Threshold 4 diff`
#   )
# }
# 
# correlate.params <- function(df, param.1, param.2) {
#   param.1 <- enquo(param.1)
#   param.2 <- enquo(param.2)
#   
#   group.var <- df %>% group_vars
#   
#   df %<>% select(param.1 = !!param.1, param.2 = !!param.2)
#   
#   result <- df %>% summarize(
#     corr = cor(param.1, param.2, use = "pairwise.complete.obs")
#   )
#   if(group.var %>% is_non_empty)
#     result %<>% spread(key = group.var, value = corr)
#   else names(result) <- param.1 %>% quo_name
#   
#   return(result)
# }
# 
# cor.LL.Wald <- function(LL.expr, Wald.expr) {
#   
#   LL.ratio.tests %>% select(Block_code, LL = LL.expr) %>%
#     full_join(
#       Wald.tests.2 %>% select(Block_code, WT = Wald.expr),
#       by = "Block_code"
#     ) %>% select(-Block_code) %>%
#     correlate.params(LL, WT) %>% magrittr::extract(1, 1)
# }
# 
# invariance.correlations <- function(
#   params,
#   block.scale.1, block.scale.2, intercept,
#   item.scale.1, threshold.1.item.1, threshold.2.item.1, threshold.3.item.1, threshold.4.item.1,
#   item.scale.2, threshold.1.item.2, threshold.2.item.2, threshold.3.item.2, threshold.4.item.2
# ) {
#   
#   block.scale.1				<- enquo(block.scale.1)
#   block.scale.2				<- enquo(block.scale.2)
#   intercept						<- enquo(intercept)
#   item.scale.1				<- enquo(item.scale.1)
#   item.scale.2				<- enquo(item.scale.2)
#   threshold.1.item.1	<- enquo(threshold.1.item.1)
#   threshold.2.item.1	<- enquo(threshold.2.item.1)
#   threshold.3.item.1	<- enquo(threshold.3.item.1)
#   threshold.4.item.1	<- enquo(threshold.4.item.1)
#   threshold.1.item.2	<- enquo(threshold.1.item.2)
#   threshold.2.item.2	<- enquo(threshold.2.item.2)
#   threshold.3.item.2	<- enquo(threshold.3.item.2)
#   threshold.4.item.2	<- enquo(threshold.4.item.2)
#   
#   params %<>% as_tibble %>% mutate(
#     `Intercept - threshold 1` = UQ(threshold.1.item.1) - UQ(threshold.1.item.2),
#     `Intercept - threshold 2` = UQ(threshold.2.item.1) - UQ(threshold.2.item.2),
#     `Intercept - threshold 3` = UQ(threshold.3.item.1) - UQ(threshold.3.item.2),
#     `Intercept - threshold 4` = UQ(threshold.4.item.1) - UQ(threshold.4.item.2)
#   )
#   
#   all.scale.params <- collapse.scale.params(
#     params, UQ(block.scale.1), UQ(block.scale.2), UQ(item.scale.1), UQ(item.scale.2)
#   )
#   
#   correlations	 <- params %>% correlate.params(UQ(block.scale.1), UQ(item.scale.1))
#   correlations.2 <- params %>% correlate.params(UQ(block.scale.2), UQ(item.scale.2))
#   correlations %<>% bind_cols(correlations.2) %>%
#     rename(`Block scale 1` = UQ(block.scale.1), `Block scale 2` = UQ(block.scale.2))
#   
#   correlations %<>% bind_cols(all.scale.params %>% correlate.params(`Block scale`, `Item scale`))
#   
#   correlations.2 <- params %>% correlate.params(`Intercept - threshold 1`, UQ(intercept))
#   correlations %<>% bind_cols(correlations.2)
#   correlations.2 <- params %>% correlate.params(`Intercept - threshold 2`, UQ(intercept))
#   correlations %<>% bind_cols(correlations.2)
#   correlations.2 <- params %>% correlate.params(`Intercept - threshold 3`, UQ(intercept))
#   correlations %<>% bind_cols(correlations.2)
#   correlations.2 <- params %>% correlate.params(`Intercept - threshold 4`, UQ(intercept))
#   correlations %<>% bind_cols(correlations.2)
#   
#   correlations %<>% bind_cols(
#     all.scale.params %>% group_by(Polarity)						%>%	correlate.params(`Block scale`, `Item scale`),
#     all.scale.params %>% group_by(Trait)							%>%	correlate.params(`Block scale`, `Item scale`)
#   ) %>% rename(`Scales` = `Block scale`, `Polarity -` = `-`, `Polarity +` = `+`) %>% 
#     rename_at(vars(all.scale.params$Trait %>% levels), ~str_c("Trait ", .)) %>%
#     bind_cols(
#       all.scale.params %>% group_by(`Polarity other`)	%>% correlate.params(`Block scale`, `Item scale`),
#       all.scale.params %>% group_by(`Trait other`)		%>% correlate.params(`Block scale`, `Item scale`)
#     ) %>% rename(`Polarity other -` = `-`, `Polarity other +` = `+`) %>% 
#     rename_at(vars(all.scale.params$Trait %>% levels), ~str_c("Trait other ", .))
#   
#   correlations %>% gather(key = "Parameters", value = "Value")
# }
# ----

get.range <- function(params, ...) {

  dots <- quos(...)

  params %>% summarize_at(
    vars(UQS(dots)), c("min", "max"), na.rm = TRUE
  ) %>% gather %>% summarize(min = value %>% min, max = value %>% max) %>% as.numeric
}

scales.scatter.plot <- function(
  params, color, labels, smooth = TRUE, filename = NULL, ...
) {

  if(!missing(color)) {

    color <- enquo(color)

    params %<>% mutate(Color = UQ(color)) %>% filter(`Block scale` %>% is_not_na)
    legend.title <- color %>% quo_name
  } else {

    params %<>% mutate(Color = "1")
    legend.title <- ""
  }

  if(!missing(labels)) {

    quo_labels <- enquo(labels)

    params %<>% mutate(Labels = UQ(quo_labels)) %>%
      filter(`Block scale` %>% is_not_na)

  } else {

    params %<>% mutate(Labels = NA_character_)
  }

  scatter.plot <- params %>% ggplot(
    aes(
      x = `Item scale`, y = `Block scale`,
      group = Block_code,
      text = paste0(
        "</br>Position: ", Position,
        "</br>Trait: ", Trait,
        "</br>Trait other: ", `Trait other`,
        "</br>Polarity: ", Polarity,
        "</br>Polarity other: ", `Polarity other`
      ),
      color = Color, label = Labels
    )
  )

  if(smooth) {

    reg <- params %>% glm(`Block scale` ~ `Item scale`, data = .) %>% coefficients

    scatter.plot <- scatter.plot +
      geom_abline(
        slope = reg["`Item scale`"], intercept = reg["(Intercept)"],
        color = PALETTE.COLORS["yellow"], alpha = .75, size = 1
      )
  }

  if(!missing(labels))
    scatter.plot <- scatter.plot + geom_label_repel(size = 2.7, min.segment.length = 0, family = "serif", show.legend = FALSE)

  if(filename %>% is_not_null)
    scatter.plot %>% scatter.plot.style(
      params %>% get.range(`Item scale`, `Block scale`),
      legend.title = legend.title, output = "ggplot"
    ) %>% ggsave(filename = filename, plot = ., path = OUTPUT.PATH, ...)

  scatter.plot %<>% scatter.plot.style(params %>% get.range(`Item scale`, `Block scale`), legend.title = legend.title)

  return(scatter.plot)
}

intercept.threshold.scatter.plot <- function(params, intercept, threshold.diff,
                                             color, smooth = TRUE,
                                             filename = NULL, ...) {
  intercept <- enquo(intercept)
  threshold.diff <- enquo(threshold.diff)

  params %<>% rename(x = UQ(threshold.diff), y = UQ(intercept))

  if(!missing(color)) {

    color <- enquo(color)

    params %<>% rename(Color = UQ(color))
    legend.title <- color %>% quo_name

  } else {

    params %<>% mutate(Color = "1")
    legend.title <- ""
  }

  scatter.plot <- params %>% ggplot(
    aes(
      x, y,
      color = Color,
      text = paste0(
        "</br>Block: ", `Block`,
        "</br>Trait 1: ", `Trait 1`,
        "</br>Trait 2: ", `Trait 2`,
        "</br>Polarity 1: ", `Polarity 1`,
        "</br>Polarity 2: ", `Polarity 2`
      )
    )
  ) + xlab(threshold.diff %>% quo_name) + ylab(intercept %>% quo_name)

  if(smooth) {

    reg <- params %>% glm(y ~ x, data = .) %>% coefficients

    scatter.plot <- scatter.plot + geom_abline(
      slope = reg["x"], intercept = reg["(Intercept)"],
      color = PALETTE.COLORS["yellow"], alpha = .75, size = 1
    )
  }

  if(filename %>% is_not_null)
    scatter.plot %>% scatter.plot.style(
      params %>% get.range(x, y),
      legend.title = legend.title, output = "ggplot", base_size = 9
    ) %>% ggsave(filename = filename, plot = ., path = OUTPUT.PATH, ...)

  scatter.plot %<>% scatter.plot.style(params %>% get.range(x, y), legend.title = legend.title)

  return(scatter.plot)
}

### Commented block ----
# ll.scale.scatter.plot <- function(params, x, y, color, alpha = .05) {
#   
#   x <- enquo(x)
#   y <- enquo(y)
#   
#   params %<>% mutate(x = UQ(x), y = UQ(y))
#   
#   if(!missing(color)) {
#     
#     color <- enquo(color)
#     
#     params %<>% mutate(Color = UQ(color))
#     legend.title <- color %>% quo_name
#     
#   } else {
#     
#     params %<>% mutate(Color = "1")
#     legend.title <- ""
#   }
#   
#   scatter.plot <- params %>% mutate(diff = `Block scale` - `Item scale`) %>%  ggplot(
#     aes(
#       x, y,
#       color = Color,
#       text = paste0(
#         "</br>LL ratio test: ", LLR %>% sprintf("%.3f", .),
#         "</br>Block scale: ", `Block scale` %>% sprintf("%.3f", .),
#         "</br>Param diff.: ", diff %>% sprintf("%.3f", .),
#         "</br>Position: ", Position,
#         "</br>Trait: ", Trait,
#         "</br>Trait other: ", `Trait other`,
#         "</br>Polarity: ", Polarity,
#         "</br>Polarity other: ", `Polarity other`
#       )
#     )
#   ) + xlab(x %>% quo_name) + ylab(y %>% quo_name) +
#     theme_minimal(base_family = "serif") +
#     geom_hline(yintercept = alpha, linetype = "dashed") +
#     geom_vline(xintercept = 0) + 
#     geom_point(alpha = .75) +
#     scale_x_continuous() +
#     scale_y_continuous(
#       trans = scales::trans_new(
#         name = "log10rev",
#         transform = function(x) -log10(x),
#         inverse = function(x) exp(-x * log(10)),
#         breaks = function(range) { range %<>% log10 %>% round; 10^(range[1]:range[2]) }
#       )
#     ) +
#     # scale_color_brewer(palette = "Set2", guide = guide_legend(title = legend.title))
#     scale_color_manual(values = PALETTE.COLORS, guide = guide_legend(title = legend.title))
#   
#   scatter.plot %<>% ggplotly(tooltip = "text") %>%
#     layout(
#       margin = list(l = 60, r = if(legend.title %>% is_non_empty_character) 150 else 40, b = 40, t = 50, pad = 0),
#       showlegend = legend.title %>% is_non_empty_character,
#       hoverlabel = list(bordercolor = "white"),
#       xaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot"),
#       yaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot")
#     ) %>%
#     config(displayModeBar = FALSE)
#   
#   return(scatter.plot)
# }
# 
# ll.intercept.scatter.plot <- function(params, x, y, real, stat, color, alpha = .05, smooth = TRUE) {
#   
#   x <- enquo(x)
#   y <- enquo(y)
#   real <- enquo(real)
#   stat <- enquo(stat)
#   
#   params %<>% mutate(x = UQ(x), y = UQ(y), stat = UQ(stat), real = UQ(real))
#   
#   if(!missing(color)) {
#     
#     color <- enquo(color)
#     
#     params %<>% mutate(Color = UQ(color))
#     legend.title <- color %>% quo_name
#     
#   } else {
#     
#     params %<>% mutate(Color = "1")
#     legend.title <- ""
#   }
#   
#   scatter.plot <- params %>% mutate(diff = real - x) %>%  ggplot(
#     aes(
#       x, y,
#       color = Color,
#       text = paste0(
#         "</br>LL ratio test: ", stat %>% sprintf("%.3f", .),
#         "</br>Block intercept: ", real %>% sprintf("%.3f", .),
#         "</br>Param diff.: ", diff %>% sprintf("%.3f", .),
#         "</br>Trait 1: ", `Trait 1`,
#         "</br>Trait 2: ", `Trait 2`,
#         "</br>Polarity 1: ", `Polarity 1`,
#         "</br>Polarity 2: ", `Polarity 2`
#       )
#     )
#   ) + xlab(x %>% quo_name) + ylab(y %>% quo_name) +
#     theme_minimal(base_family = "serif") +
#     geom_hline(yintercept = alpha, linetype = "dashed") +
#     geom_vline(xintercept = 0)
#   
#   if(smooth) {
#     
#     scatter.plot <- scatter.plot + geom_smooth(
#       mapping = aes(text = NULL), span = 1,
#       color = PALETTE.COLORS["yellow"], alpha = .75, size = 1, fill = PALETTE.COLORS["yellow"]
#     )
#   }
#   
#   scatter.plot <- scatter.plot + geom_point(alpha = .75) +
#     scale_x_continuous() +
#     scale_y_continuous(
#       trans = scales::trans_new(
#         name = "log10rev",
#         transform = function(x) -log10(x),
#         inverse = function(x) exp(-x * log(10)),
#         breaks = function(range) { range %<>% log10 %>% round; 10^(seq(range[1], range[2], by = -2)) }
#       )
#     ) +
#     # scale_color_brewer(palette = "Set2", guide = guide_legend(title = legend.title))
#     scale_color_manual(values = PALETTE.COLORS, guide = guide_legend(title = legend.title))
#   
#   scatter.plot %<>% ggplotly(tooltip = "text") %>%
#     layout(
#       margin = list(l = 60, r = if(legend.title %>% is_non_empty_character) 150 else 40, b = 40, t = 50, pad = 0),
#       showlegend = legend.title %>% is_non_empty_character,
#       hoverlabel = list(bordercolor = "white"),
#       xaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot"),
#       yaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot")
#     ) %>%
#     config(displayModeBar = FALSE)
#   
#   return(scatter.plot)
# }
# 
# ll.params.scatter.plot <- function(params, x, y, color, chisq.sig.line = .05, axes = FALSE) {
#   
#   x <- enquo(x)
#   y <- enquo(y)
#   
#   params %<>% mutate(x = UQ(x), y = UQ(y))
#   
#   if(!missing(color)) {
#     
#     color <- enquo(color)
#     
#     params %<>% mutate(Color = UQ(color))
#     legend.title <- color %>% quo_name
#     
#   } else {
#     
#     params %<>% mutate(Color = "1")
#     legend.title <- ""
#   }
#   
#   scatter.plot <- params %>% ggplot(
#     aes(
#       x, y,
#       color = Color,
#       text = paste0(
#         "</br>Trait 1: ", `Trait 1`,
#         "</br>Trait 2: ", `Trait 2`,
#         "</br>Polarity 1: ", `Polarity 1`,
#         "</br>Polarity 2: ", `Polarity 2`
#       )
#     )
#   ) + xlab(x %>% quo_name) + ylab(y %>% quo_name) +
#     theme_minimal(base_family = "serif")
#   
#   if(chisq.sig.line) {
#     
#     chisq.val <- qchisq(chisq.sig.line, 1, lower.tail = FALSE)
#     
#     scatter.plot <- scatter.plot + geom_hline(yintercept = chisq.val, linetype = "dashed") +
#       geom_vline(xintercept = chisq.val, linetype = "dashed")
#   }
#   
#   if(axes) scatter.plot <- scatter.plot + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
#   
#   scatter.plot <- scatter.plot + geom_point(alpha = .75) +
#     # scale_color_brewer(palette = "Set2", guide = guide_legend(title = legend.title))
#     scale_color_manual(values = PALETTE.COLORS, guide = guide_legend(title = legend.title))
#   
#   scatter.plot %<>% ggplotly(tooltip = "text") %>%
#     layout(
#       margin = list(l = 60, r = if(legend.title %>% is_non_empty_character) 150 else 40, b = 40, t = 50, pad = 0),
#       showlegend = legend.title %>% is_non_empty_character,
#       hoverlabel = list(bordercolor = "white"),
#       xaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot"),
#       yaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot")
#     ) %>%
#     config(displayModeBar = FALSE)
#   
#   return(scatter.plot)
# }
# 
# ll.unidim.scatter.plot <- function(params, x, y, color, facet, chisq.sig.line = .05, axes = TRUE, smooth = TRUE) {
#   
#   x <- enquo(x)
#   y <- enquo(y)
#   
#   params %<>% mutate(x = UQ(x), y = UQ(y)) %>% filter(x %>% is_not_na, y %>% is_not_na)
#   
#   if(!missing(color)) {
#     
#     color <- enquo(color)
#     
#     params %<>% mutate(Color = UQ(color))
#     legend.title <- color %>% quo_name
#     
#   } else {
#     
#     params %<>% mutate(Color = "1")
#     legend.title <- ""
#   }
#   
#   if(!missing(facet)) {
#     
#     facet <- enquo(facet)
#     
#     params %<>% mutate(Facet = UQ(facet))
#     
#   } else {
#     
#     params %<>% mutate(Facet = "1")
#   }
#   
#   scatter.plot <- params %>% mutate(diff = `Block scale` - `Item scale`) %>%  ggplot(
#     aes(
#       x, y,
#       color = Color,
#       text = paste0(
#         "</br>LL ratio test: ", LLR %>% sprintf("%.3f", .),
#         "</br>Block scale: ", `Block scale` %>% sprintf("%.3f", .),
#         "</br>Param diff.: ", diff %>% sprintf("%.3f", .),
#         "</br>Position: ", Position,
#         "</br>Trait: ", Trait,
#         "</br>Trait other: ", `Trait other`,
#         "</br>Polarity: ", Polarity,
#         "</br>Polarity other: ", `Polarity other`
#       )
#     )
#   ) + xlab(x %>% quo_name) + ylab(y %>% quo_name) +
#     theme_minimal(base_family = "serif")
#   
#   if(!missing(facet)) scatter.plot <- scatter.plot + facet_grid(. ~ Facet)
#   
#   if(chisq.sig.line) {
#     
#     scatter.plot <- scatter.plot +
#       geom_hline(yintercept = qchisq(chisq.sig.line, 1, lower.tail = FALSE), linetype = "dashed")
#   }
#   
#   if(axes) scatter.plot <- scatter.plot + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
#   
#   if(smooth) {
#     
#     scatter.plot <- scatter.plot + geom_smooth(
#       mapping = aes(text = NULL), span = 2,
#       color = PALETTE.COLORS["yellow"], alpha = .75, size = 1, fill = PALETTE.COLORS["yellow"]
#     )
#   }
#   
#   scatter.plot <- scatter.plot + geom_point(alpha = .75) + scale_x_continuous() + scale_y_continuous() +
#     # scale_color_brewer(palette = "Set2", guide = guide_legend(title = legend.title))
#     scale_color_manual(values = PALETTE.COLORS, guide = guide_legend(title = legend.title))
#   
#   scatter.plot %<>% ggplotly(tooltip = "text") %>%
#     layout(
#       margin = list(l = 60, r = if(legend.title %>% is_non_empty_character) 150 else 40, b = 40, t = 50, pad = 0),
#       showlegend = legend.title %>% is_non_empty_character,
#       hoverlabel = list(bordercolor = "white"),
#       xaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot"),
#       yaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot")
#     ) %>%
#     config(displayModeBar = FALSE)
#   
#   return(scatter.plot)
# }
# ----

scatter.plot.style <- function(
  input.plot,
  range.x, range.y = range.x, legend.title = "",
  bisector = range.y %>% identical(range.x),
  output = c("plotly", "ggplot"), base_size = 11
) {

  fig.margins <- list(l = 40, r = if(legend.title %>% is_non_empty_character) 150 else 40, b = 40, t = 50, pad = 0)

  output <- match.arg(output)

  output.plot <- input.plot +
    theme_minimal(base_family = "serif", base_size = base_size)

  if(bisector) output.plot <- output.plot + geom_abline(slope = 1, intercept = 0, color = "lightgrey", linetype = "dashed")

  output.plot <- output.plot + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_point(alpha = .75) +
    scale_x_continuous(limits = range.x) + scale_y_continuous(limits = range.y) +
    scale_color_manual(
      values = PALETTE.COLORS %>% unname,
      # scale_color_brewer(
      # 	palette = "Set2",
      guide = if(legend.title == "") "none"
      else guide_legend(title = legend.title, title.hjust = .5)
    )

  if(output == "plotly") {

    output.plot %<>% ggplotly(tooltip = "text") %>%
      layout(
        margin = fig.margins,
        showlegend = legend.title %>% is_non_empty_character,
        hoverlabel = list(bordercolor = "white"),
        xaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot"),
        yaxis = list(showspikes = TRUE, spikethickness = .5, spikedash = "dot")
      ) %>%
      config(displayModeBar = FALSE)
  }

  return(output.plot)
}

plot_joint_threshold_per_block <- function(
  block_data, block_intercept, predicted
) {
  block_intercept <- enquo(block_intercept)
  predicted <- enquo(predicted)


  plot_obj <- block_data %>% intercept.threshold.scatter.plot(
    !!block_intercept, !!predicted,
    width = 3, height = 2.6, units = "in", dpi = "print"
  )

  return(plot_obj)
}

assess.unidimensionality <- function(items, traits = TRAIT.NAMES) {

  set.seed(11111)

  model.prefix <- "res/apra2_unidimensional_iteration_0_"

  traits <- match.arg(traits, several.ok = TRUE)

  item.ff.var.props <- numeric()
  fit.unidim.diagnostics <- list()
  item.scales <- data.frame(
    Unidim = numeric(), Bifactor = numeric(), Trait = character(),
    I_ECV = numeric()
  )
  for(trait in traits) {

    trait <- BIG.FIVE.TRAITS[trait]

    filename <- paste0(
      model.prefix,
      get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE),
      "_ml.out"
    )

    model.result <- readModels(filename)
    if(trait == "Neuroticismo")
      model.result$parameters$unstandardized$est <-
      -model.result$parameters$unstandardized$est

    unidim.loadings <- get.CFA.parameters(
      model.result, traits = trait, values = ESTIMATE.HEADERS[c("estimate", "p_value")]
    )$loadings$estimate

    trait.items <- items[unidim.loadings %>% rownames, ]

    unidim.assessment <- assess.Mplus.unidimensional.model(
      model.result, trait, trait.items, invert = trait %in% INVERT.DIMS,
      estimator = ESTIMATOR,
      estimate.bifactor = FALSE
    )

    fit.unidim.diagnostics <- rbind(fit.unidim.diagnostics, unidim.assessment$fit)
    item.ff.var.props %<>% c(unidim.assessment$common.vars)

    item.scales <- bind_rows(
      item.scales,
      tibble(
        Item = unidim.loadings %>% rownames,
        Unidim = unidim.loadings[, 1],
        Bifactor = unidim.assessment$bf.general,
        Trait = trait,
        Polarity = trait.items[unidim.loadings %>% rownames, "polarity"],
        I_ECV = unidim.assessment$common.vars
      )
    )
  }

  item.scales %<>% mutate(
    Trait = BIG.FIVE.TRAITS[
      match(
        item.scales$Trait %>% as.character,
        BIG.FIVE.TRAITS
      )
      ] %>% names,
    Trait = Trait %>% parse_factor(
      levels = names(BIG.FIVE.TRAITS)[BIG.FIVE.TRAITS %>% names %in% traits]
    ),
    Rel_bias = (Unidim - Bifactor) / Bifactor * 100
  )

  return(
    list(
      fit = fit.unidim.diagnostics,
      item.scales = item.scales
    )
  )
}
