## INCLUDES

library(MplusAutomation)
library(psych)
library(assertive)
library(mco)
library(GPArotation)
library(magrittr)

source("R/Data.R", encoding = 'UTF-8')
source("R/SLi_functions.R", encoding = 'UTF-8')
source("R/Output.R", encoding = 'UTF-8')


## CONSTANTS

# PARAMETERS OF THE NSGA2 ALGORITHM:
N.GENERATIONS <- 5000
POP.SIZE <- 40


# FILE SYSTEM CONSTANTS:
MODEL.PATH <- "res"
MODEL.FILE.SUFFIX <- "_exploratory_bifactor.out"

MPLUS.MODEL.SUFFIX <- ".out"
MPLUS.MODEL.SUFFIX.REGEXP <- paste0("\\", MPLUS.MODEL.SUFFIX)
MPLUS.INPUT.SUFFIX <- ".inp"

MPLUS.THRESHOLD.HEADER <- "Thresholds"
MPLUS.LV.MEAN.HEADER <- "Means"
MPLUS.UNICITY.HEADER <- "Residual.Variances"
MPLUS.LV.VAR.HEADER <- "Variances"

MPLUS.LOADING.SUFFIX <- ".BY"
MPLUS.CORR.SUFFIX <- ".WITH"
MPLUS.GENERAL.FACTOR <- "GEN"
MPLUS.NA <- "999"

MPLUS.COLUMN.HEADERS <- c("est", "se", "pval")
names(MPLUS.COLUMN.HEADERS) <- MPLUS.COLUMN.HEADERS


MODEL.NAMES <- c("Emotional Stability", "Extraversion", "Openness", "Agreeableness", "Conscientiousness")
MODEL.ABREVIATIONS <- c("ES", "Ex", "Op", "Ag", "Co")
names(MODEL.ABREVIATIONS) <- MODEL.NAMES

FACET.NAMES <- matrix(
  c(
    "ANX",    "HOST",   "DEPR",    "SOC_ANX", "IMPULS",  "VULN",
    "CORD",   "GREG",   "ASSERT",  "ACTIV",   "SEEK",    "POSIT",
    "FANT",   "AESTH",  "FEEL",    "ACTION",  "IDEAS",   "VALUES",
    "CONF",   "FRANK",  "ALTR",    "CONCEAL", "MODESTY", "SENSIT",
    "COMPET", "ORDER",  "SEN_DUT", "ACHIEV",  "SELF_DIS", "DELIB"
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(BIG.FIVE.TRAITS, NULL)
)

GENERAL.FACTOR.NAME <- "General"


ESTIMATE.HEADERS <- c("estimate", "std.err", "p_value")
names(ESTIMATE.HEADERS) <- ESTIMATE.HEADERS

MODEL.FILE.NAMES <- paste0(MODEL.NAMES, MODEL.FILE.SUFFIX)
names(MODEL.FILE.NAMES) <- BIG.FIVE.TRAITS

MODEL.EXCEL.OUTPUT <- "Model_summary.xlsx"
PARALLEL.ANALYSES.OUTPUT <- "Parallel_analyses.Rdata"
OPTIMIZATION.OUTPUT <- "Optimization_result.Rdata"
OPTIMIZATION.BLOCKS.OUTPUT <- "Blockset_optimization_result.Rdata"

FIT.INDICES <- c("CFI", "TLI", "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "RMSEA_pLT05", "WRMR")

DEFAULT.SIG.LEVEL <- .05

MPLUS.MODEL.CLASS <- "mplus.model"

PARAMETERS <- NULL


# OPTIMIZATION PARAMETERS:
OPTIM.THRESHOLD <- .2

VAR.CRITERIA <- paste0("-", MODEL.ABREVIATIONS, " dir var")
ANG.CRITERIA <- paste(MODEL.ABREVIATIONS, "ang var")
MIN.LOAD.CRITERIA <- paste0("-", MODEL.ABREVIATIONS, " min abs loading")
ES.DIR.ITEMS.CRITERION <- "-ES direct items"
DD.BLOCKS.LSV.CRITERION <- "-DD blocks LSV"
DD.EQ.TRAITS.CRITERION <- "DD EQUATED PAIRS"
HP.EQ.TRAITS.CRITERION <- "HETEROPOLAR EQUATED PAIRS"
OPTIM.CRITERIA.NAMES <- c(ANG.CRITERIA, MIN.LOAD.CRITERIA, VAR.CRITERIA, ES.DIR.ITEMS.CRITERION)
OPTIM.CRITERIA.BLOCKS <- c(
  ANG.CRITERIA, MIN.LOAD.CRITERIA,
  ES.DIR.ITEMS.CRITERION, DD.BLOCKS.LSV.CRITERION,
  DD.EQ.TRAITS.CRITERION, HP.EQ.TRAITS.CRITERION
)

OPTIM.CONSTRAINT.NAMES <- c(
  paste0(BIG.FIVE.TRAITS, "_total"),
  paste0(BIG.FIVE.TRAITS[-1], "_direct"),
  paste0(BIG.FIVE.TRAITS, "_inverse"),
  "Total direct", "Total inverse"
)
# OPTIM.CONSTRAINT.NAMES <- c(
#   paste0(BIG.FIVE.TRAITS, "_total"),
#   paste0(BIG.FIVE.TRAITS[-1], "_direct"),
#   paste0(BIG.FIVE.TRAITS, "_inverse"),
#   "Total direct", "Total inverse",
#   "Total DD blocks",
#   "Valid blocks",
#   "Total inverse blocks"
# )

# MODEL CONSTRAINTS:
ITEMS.BY.POLARITY <- c(285, 95)
MIN.ITEMS.PER.TRAIT.AND.POL <- c(57, 10)
names(ITEMS.BY.POLARITY) <- c("DIRECT", "INVERSE")
names(MIN.ITEMS.PER.TRAIT.AND.POL) <- c("DIRECT", "INVERSE")

ITEMS.BY.TRAIT <- 76
TOTAL.ITEMS <- ITEMS.BY.TRAIT * length(BIG.FIVE.TRAITS)
ITEMS.PER.BLOCK <- 2
N.BLOCKS <- TOTAL.ITEMS / ITEMS.PER.BLOCK
DD.BLOCKS <- 95


## FUNCTIONS

get.traits.from.model.files <- function(file.names) {
  
  return(BIG.FIVE.TRAITS[match(file.names, MODEL.FILE.NAMES)])
}

get.model.name.from.trait <- function(
  trait = c(BIG.FIVE.TRAITS, BIG.FIVE.FACETS),
  abbreviation = FALSE, upper.case = TRUE
) {
  
  trait <- match.arg(trait, several.ok = TRUE)
  
  result <- MODEL.NAMES[BIG.FIVE.TRAITS %in% trait]
  if(abbreviation) result <- MODEL.ABREVIATIONS[result]
  
  result <- c(result, FACET.NAMES[BIG.FIVE.FACETS %in% trait])
  
  if(upper.case) result <- toupper(result)
  
  return(result)
}

get.trait.name.from.dimension <- function(
  dimension = toupper(c(MODEL.ABREVIATIONS, FACET.NAMES)),
  header.suffix = c("none", MPLUS.LOADING.SUFFIX, MPLUS.CORR.SUFFIX)) {
  
  header.suffix <- match.arg(header.suffix)
  if(header.suffix != "none") {
    
    dimension <- gsub(header.suffix, "", dimension)
  }
  
  dimension <- toupper(dimension)
  dimension <- match.arg(dimension, several.ok = TRUE)
  
  traits <- c(BIG.FIVE.TRAITS, BIG.FIVE.FACETS)
  result <- traits[toupper(c(MODEL.ABREVIATIONS, FACET.NAMES)) %in% dimension]
  
  return(result)
}

get.model.facets <- function(traits, ordered = FALSE) {
  
  result <- FACET.NAMES[rownames(BIG.FIVE.FACETS) %in% traits]
  if(ordered) result <- result[order(result)]
  return(result)
}


get.fit.indices <- function(path = MODEL.PATH) {
  
  summaries <- readModels(path, what="summaries")$summaries
  rownames(summaries) <- get.traits.from.model.files(summaries$Filename)
  
  return(summaries[FIT.INDICES])
}

get.parameters <- function(
  path = MODEL.PATH,
  type = c("unstandardized", "std.standardized", "stdyx.standardized", "stdy.standardized")
) {
  
  type <- match.arg(type)
  
  parameters <- extractModelParameters(path)
  names(parameters) <- get.traits.from.model.files(names(parameters))
  
  PARAMETERS <<- lapply(parameters, function(trait) trait[[type]])
  
  return(NULL)
}

get.loading.headers <- function(trait, facets = TRUE) {
  
  return(get.Mplus.headers(trait, facets, MPLUS.LOADING.SUFFIX))
}

get.corr.headers <- function(trait, facets = TRUE) {
  
  return(get.Mplus.headers(trait, facets, MPLUS.CORR.SUFFIX))
}

get.Mplus.headers <- function(trait, facets = TRUE, suffix) {
  
  factor.names <- get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = TRUE)
  
  if(facets) factor.names <- c(factor.names, FACET.NAMES[trait, ])
  
  result <- paste0(factor.names, suffix)
  
  return(result)
}

item.order.index <- function(item.names) {
  
  return(order(as.numeric(substr(item.names, start = 2, stop = nchar(item.names)))))
}

get.loading.estimates <- function(traits = BIG.FIVE.TRAITS, ...) {
  
  if(is.null(PARAMETERS)) {
    
    if(!exists("path")) path = MODEL.PATH
    get.parameters(path, ...)
  }
  
  loading.estimates <- sapply(
    traits,
    function(trait) {
      
      loadings <- paramExtract(PARAMETERS[[trait]], params = "loading")
      
      loading.estimates <- numeric()
      
      for(header in get.loading.headers(trait)) {
        
        factor.loadings <- loadings[loadings$paramHeader == header, ]
        order.index <- item.order.index(factor.loadings$param)
        
        loading.estimates <- cbind(loading.estimates, factor.loadings$est[order.index])
      }
      
      rownames(loading.estimates) <- get.trait.items(trait, instrument = "APRA")$code
      colnames(loading.estimates) <- c(trait, get.facets(trait))
      
      return(loading.estimates)
    },
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(loading.estimates)
}


get.loading.pvalues <- function(traits = BIG.FIVE.TRAITS, ...) {
  
  if(is.null(PARAMETERS)) {
    
    if(!exists("path")) path = MODEL.PATH
    get.parameters(path)
  }
  
  loading.pvalues <- sapply(
    traits,
    function(trait) {
      
      loadings <- paramExtract(PARAMETERS[[trait]], params = "loading")
      
      loading.pvalues <- numeric()
      
      for(header in get.loading.headers(trait)) {
        
        factor.loadings <- loadings[loadings$paramHeader == header, ]
        order.index <- item.order.index(factor.loadings$param)
        
        loading.pvalues <- cbind(loading.pvalues, factor.loadings$pval[order.index])
      }
      
      rownames(loading.pvalues) <- get.trait.items(trait, instrument = "APRA")$code
      colnames(loading.pvalues) <- c(trait, get.facets(trait))
      
      return(loading.pvalues)
    },
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(loading.pvalues)
}

get.sinificance.levels <- function(pvalues.list) {
  
  return(sapply(pvalues.list, pval.to.significance, simplify = FALSE, USE.NAMES = TRUE))
}


pval.to.significance <- function(pvalues) {
  
  sig.levels <- pvalues
  sig.levels[, ] <- 0
  
  for(level in 1:3) sig.levels <- ifelse(pvalues < sig.level.to.alpha(level), level, sig.levels)
  
  return(sig.levels)
}

sig.level.to.alpha <- function(sig.level) {
  
  return(10^(-sig.level) * .5)
}

create.all.loading.tables <- function(loading.estimates, sig.levels) {
  
  tables <- sapply(
    BIG.FIVE.TRAITS,
    function(trait) {
      
      loadings <- loading.estimates[[trait]]
      sig <- sig.levels[[trait]]
      
      table <- create.loading.table(loadings, sig)
      
      return(table)
    },
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(tables)
}

create.loading.table <- function(loadings, sig.levels) {
  
  sig.marks <- sig.level.to.mark(sig.levels)
  
  table <- data.frame(row.names = rownames(loadings))
  for(factor in colnames(loadings)) table <- cbind(table, loadings[, factor], sig.marks[, factor])
  colnames(table) <- t(outer(colnames(loadings), c('', '.sig'), FUN = paste0))
  
  return(table)
}

sig.level.to.mark <- function(sig.levels) {
  
  mark <- sapply(
    sig.levels,
    function(level) {
      
      return(paste0(rep('*', level), collapse = ''))
    },
    USE.NAMES = TRUE
  )
  dim(mark) <- dim(sig.levels)
  dimnames(mark) <- dimnames(sig.levels)
  
  return(mark)
}

combined.trait.tables <- function(loading.tables) {
  
  table.list <- sapply(
    BIG.FIVE.TRAITS,
    function(trait) {
      
      items <- get.trait.items(trait, instrument = c("APRA"))
      
      table <- cbind(items[c("facet", "polarity")], loading.tables[[trait]])
      rownames(table) <- rownames(loading.tables[[trait]])
      
      return(table)
    },
    USE.NAMES = TRUE, simplify = FALSE
  )
  
  return(table.list)
}

write.model.summaries <- function(fit.indices, parallel.analyses, item.estimates, file = MODEL.EXCEL.OUTPUT) {
  
  output.file <- loadWorkbook(paste0(OUTPUT.PATH, file), create = TRUE)
  
  createSheet(output.file, "Ajuste")
  
  writeWorksheet(output.file, fit.indices, "Ajuste", rownames = "Rasgo")
  
  for(trait in BIG.FIVE.TRAITS) {
    
    createSheet(output.file, trait)
    writeWorksheet(output.file, item.estimates[[trait]], trait, rownames = "Item")
  }
  
  saveWorkbook(output.file)

  return(NULL)
}

run.parallel.analyses <- function(
  traits = BIG.FIVE.TRAITS,
  responses = RESPONSE.DATA,
  filename = PARALLEL.ANALYSES.OUTPUT
) {
  
  tryCatch(
    load(paste0(OUTPUT.PATH, PARALLEL.ANALYSES.OUTPUT)),
    error = function(e) {}
  )
  
  if(!exists("pa.data")) {
    
    pa.data <- sapply(
      traits,
      function(trait) {
        
        responses <- get.trait.responses(trait, instrument = "APRA")
        
        parallel.analysis <- fa.parallel(responses, fa = "fa", cor = 'poly')
        
        return(parallel.analysis)
      },
      USE.NAMES = TRUE, simplify = FALSE
    )
    save(pa.data, file = paste0(OUTPUT.PATH, filename))
  }
  
  return(pa.data)
}

get.pa.factors <- function(pa.data) {
  
  pa.factors <- sapply(pa.data, function(trait) trait$nfact, USE.NAMES = TRUE)
  
  return(pa.factors)
}


create.target <- function(items, trait = BIG.FIVE.TRAITS) {
  
  trait <- match.arg(trait)
  
  items <- select.items.by.trait(items, trait)
  
  facets <- get.facets(trait, order = TRUE)
  
  target.matrix <- matrix(
    nrow = nrow(items), ncol = get.n.factors(trait),
    dimnames = list(item = rownames(items), factor = c(GENERAL.FACTOR.NAME, facets))
  )
  
  for(facet in facets) {
    
    target.matrix[items$facet != facet, facet] <- 0
  }
  
  return(target.matrix)
}

create.FC.target <- function(blocks) {
  
  target.matrix <- matrix(
    nrow = nrow(blocks), ncol = length(BIG.FIVE.TRAITS),
    dimnames = list(block = rownames(blocks), factor = BIG.FIVE.TRAITS)
  )
  
  for(trait in BIG.FIVE.TRAITS) {
    
    target.matrix[blocks$trait.item.1 != trait & blocks$trait.item.2 != trait, trait] <- 0
  }
  
  return(target.matrix)
}

### WARNING: Old code (prior to 29-5-2017) may not work!!!
revert.items <- function(item.loadings) {
  
  return(item.loadings * -1)
}

invert.polarity <- function(item.data, trait, instrument = c("APRA", APRA2.INSTRUMENT)) {
	
	item.codes <- get.trait.items(trait, items = item.data, instrument = instrument)$code
	
	polarity <- item.data$polarity[item.data$code %in% item.codes]
	
	item.data$polarity[item.data$code %in% item.codes] <- as.factor(ifelse(polarity == "+", "-", "+"))
	
	return(item.data)
}

filter.items <- function(loading.estimates, loading.pvals) {
  
  final.loadings <- sapply(
    BIG.FIVE.TRAITS,
    function(trait) {
      
      loadings <- loading.estimates[[trait]]
      
      significant <- pval.to.significance(loading.pvals[[trait]])[, 1] > 0
      sign.correct <- xor(loadings[, 1] > 0, ITEM.DATA$polarity[ITEM.DATA$code %in% rownames(loadings)] == "-")
      
      return(loadings[significant & sign.correct, ])
    },
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(final.loadings)
}

get.angles <- function(loadings) {
  
  modules <- compute.modules(loadings)
  return(acos(loadings/modules))
}

compute.angles <- function(loadings) {
  
  angles <- sapply(
    loadings,
    get.angles,
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(angles)
}

get.angle.vars <- function(angles) {
  
  if(is.null(dim(angles))) dim(angles) <- c(1, 7)
  
  for(col in seq_len(ncol(angles)))
    angles[, col] <- ifelse(angles[, 1] > pi/2, angles[, col] - pi, angles[, col])
  
  vars <- apply(angles, 2, var)
  
  return(vars)
}

compute.angle.vars <- function(angles) {
  
  angle.vars <- sapply(
    angles,
    get.angle.vars,
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(angle.vars)
}

get.total.angle.vars <- function(angles) {
  
  vars <- compute.angle.vars(angles)
  total.vars <- sapply(vars, sum)
  
  return(total.vars)
}

get.gen.angle.vars <- function(angles) {
  
  vars <- compute.angle.vars(angles)
  gen.vars <- sapply(vars, function(trait.vars) trait.vars[1])
  
  return(gen.vars)
}

get.abs.loading.min <- function(loadings) {
  
  abs.loading.mins <- sapply(
    loadings,
    function(trait.loadings) apply(abs(trait.loadings), 2, min)[1],
    simplify = TRUE, USE.NAMES = FALSE
  )
  
  return(abs.loading.mins)
}

get.loading.vars <- function(loadings) {
  
  if(is.null(dim(loadings))) dim(loadings) <- c(1, 7)
  
  vars <- apply(loadings, 2, var)
  
  return(vars)
}

compute.loading.vars <- function(loadings) {
  
  loading.vars <- sapply(
    loadings,
    get.loading.vars,
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(loading.vars)
}

get.general.factor.variances <- function(loadings) {
  
  vars <- compute.loading.vars(loadings)
  
  sapply(
    vars,
    function(trait.vars) {
      
      trait.vars[1]
    },
    USE.NAMES = TRUE
  )
}

get.mean.vector.angle <- function(trait.loadings) {
  
  mean.loadings <- colMeans(trait.loadings)
  dim(mean.loadings) <- c(1, length(mean.loadings))
  colnames(mean.loadings) <- colnames(trait.loadings)
  
  return(get.angles(mean.loadings))
}

compute.modules <- function(loadings) {
  
  return(sqrt(rowSums(loadings^2)))
}

get.loadings.by.polarity <- function(trait, loadings, polarity = c("+", "-")) {
  
  polarity <- match.arg(polarity)
  
  loadings <- loadings[[trait]]
  item.codes <- get.trait.items(trait, instrument = "APRA", polarity = polarity)
  
  return(loadings[rownames(loadings) %in% item.codes$code, ])
}

loadings.by.polarity <- function(loadings, polarity = c("+", "-")) {
  
  polarity <- match.arg(polarity)
  
  final.loadings <- sapply(
    BIG.FIVE.TRAITS,
    get.loadings.by.polarity,
    loadings, polarity,
    simplify = FALSE, USE.NAMES = TRUE
  )
  
  return(final.loadings)
}

all.item.codes.from.values <- function(values) {
  
  codes <- character()
  
  for(trait in values) {
    
    codes <- c(codes, rownames(trait))
  }
  
  return(codes)
}

vars.in.codes <- function(vars, item.codes) {
  
  selection <- lapply(
    vars,
    function(trait.vars) {
      
      return(trait.vars[rownames(trait.vars) %in% item.codes, , drop = FALSE])
    }
  )
  
  return(selection)
}

create.item.selection <- function(loading.estimates, angles) {
  
  tryCatch(
    load(paste0(OUTPUT.PATH, OPTIMIZATION.OUTPUT)),
    error = function(e) {}
  )
  
  if(!exists("optim.result")) {
    
    # Get all the item codes in a row:
    item.codes <- all.item.codes.from.values(loading.estimates)
    n.params <- length(item.codes)
    
    pos.loadings <- loadings.by.polarity(loading.estimates, "+")
    
    objective <- function(decision.vars) {
      
      decision.vars <- decision.vars > OPTIM.THRESHOLD
      
      assert_are_same_length(decision.vars, item.codes)
      
      selected <- item.codes[as.logical(decision.vars)]
      selected.pos.loadings <- vars.in.codes(pos.loadings, selected)
      selected.loadings <- vars.in.codes(loading.estimates, selected)
      selected.angles <- vars.in.codes(angles, selected)
      
      n.ES.pos.items <- nrow(selected.pos.loadings$Neuroticismo)
      pos.loading.vars <- get.general.factor.variances(selected.pos.loadings)
      abs.loading.mins <- get.abs.loading.min(selected.loadings)
      # selected.angle.vars <- get.total.angle.vars(selected.angles)
      selected.angle.vars <- get.gen.angle.vars(selected.angles)
      
      criteria <- c(selected.angle.vars, -abs.loading.mins, -pos.loading.vars, -n.ES.pos.items)
      names(criteria) <- OPTIM.CRITERIA.NAMES
      
      return(criteria)
    }
    
    constraints <- function(decision.vars) {
      
      assert_are_same_length(decision.vars, item.codes)
      
      decision.vars  <-  decision.vars > OPTIM.THRESHOLD
      selected <- item.codes[decision.vars]
      
      selected.per.trait <- sapply(vars.in.codes(loading.estimates, selected), nrow)

      selected.pos.loadings <- vars.in.codes(pos.loadings, selected)
      direct.selected <- sapply(selected.pos.loadings, nrow)
      inverse.selected <- selected.per.trait - direct.selected
      
      total.inverse.selected <- sum(inverse.selected)
      
      constraints <- c(
        -abs(selected.per.trait - ITEMS.BY.TRAIT) + 1,
        direct.selected[-1] - MIN.ITEMS.PER.TRAIT.AND.POL["DIRECT"] + 1,
        inverse.selected - MIN.ITEMS.PER.TRAIT.AND.POL["INVERSE"] + 1,
        -abs(sum(direct.selected) - ITEMS.BY.POLARITY["DIRECT"]) + 1,
        -abs(sum(inverse.selected) - ITEMS.BY.POLARITY["INVERSE"]) + 1
      )
      names(constraints) <- OPTIM.CONSTRAINT.NAMES
      
      return(constraints)
    }
    opt.time <- system.time(
      optim.result <- nsga2(
        objective, idim = n.params, odim = length(OPTIM.CRITERIA.NAMES),
        constraints = constraints, cdim = length(OPTIM.CONSTRAINT.NAMES),
        generations = N.GENERATIONS, popsize = POP.SIZE,
        lower.bounds = rep(0, n.params), upper.bounds = rep(1, n.params)
      )
    )
    
    colnames(optim.result$par) <- all.item.codes.from.values(loading.estimates)
    rownames(optim.result$par) <- seq_len(POP.SIZE)
    
    colnames(optim.result$value) <- OPTIM.CRITERIA.NAMES
    rownames(optim.result$value) <- seq_len(POP.SIZE)
    
    optim.result$constraints <- t(apply(optim.result$par, 1, constraints))
    colnames(optim.result$constraints) <- OPTIM.CONSTRAINT.NAMES
    rownames(optim.result$constraints) <- seq_len(POP.SIZE)
    
    optim.result$time = opt.time
    
    optim.result$indexes <- optim.result$par > OPTIM.THRESHOLD
    colnames(optim.result$indexes) <- all.item.codes.from.values(loading.estimates)
    rownames(optim.result$indexes) <- seq_len(POP.SIZE)
    
    save(optim.result, file = paste0(OUTPUT.PATH, OPTIMIZATION.OUTPUT))
  }
  
  return(optim.result)
}

items.in.solution <- function(index) {
  
  return(sum(index))
}

codes.from.vars <- function(vars, item.codes) {
  
  item.vars <- vars.in.codes(vars, item.codes)
  
  codes <- lapply(item.vars, rownames)
  
  return(codes)
}

summarize.item.selection.result <- function(optim.result, loading.estimates) {
  
  item.codes <- all.item.codes.from.values(loading.estimates)
  n.params <- length(item.codes)
  
  pos.loadings <- loadings.by.polarity(loading.estimates, "+")
  neg.loadings <- loadings.by.polarity(loading.estimates, "-")
  
  
  indexes <- optim.result$indexes
  
  items.per.sol <- apply(indexes, 1, items.in.solution)
  
  selected <- t(
    sapply(
      seq_len(POP.SIZE),
      function(solution) return(item.codes[indexes[solution, ]]),
      simplify = all(items.per.sol == ITEMS.BY.TRAIT * length(BIG.FIVE.TRAITS))
    )
  )
  
  n.selected.pos <- t(
    apply(
      selected, 1,
      function(items) {
        
        loadings <- vars.in.codes(pos.loadings, items)
        n.loadings <- sapply(loadings, nrow)
        
        return(n.loadings)
      }
    )
  )
  
  n.selected.neg <- t(
    apply(
      selected, 1,
      function(items) {
        
        loadings <- vars.in.codes(neg.loadings, items)
        n.loadings <- sapply(loadings, nrow)
        
        return(n.loadings)
      }
    )
  )
  
  # Flag the solutions with the items more unbalanced
  flags <- apply(
    n.selected.neg[, BIG.FIVE.TRAITS != "Responsabilidad"] < 8 |
      n.selected.neg[, BIG.FIVE.TRAITS["Responsabilidad"]] < 7,
    1,
    any
  )
  valid.sols <- seq_len(POP.SIZE)[!flags]
  
  total.ang.var <- rowSums(optim.result$value[!flags, ANG.CRITERIA])
  total.dir.var <- -rowSums(optim.result$value[!flags, VAR.CRITERIA])
  min.abs.loading <- apply(-optim.result$value[!flags, MIN.LOAD.CRITERIA], 1, min)
  
  sol.order <- order(
    -optim.result$value[!flags, ES.DIR.ITEMS.CRITERION],
    -total.ang.var,
    min.abs.loading,
    total.dir.var,
    decreasing = TRUE
  )
  best.sol <- valid.sols[sol.order[1]]
  optim.result$solution.chosen <- best.sol
  
  chosen.items <- item.codes[indexes[best.sol, ]]
  
  all.items <- get.instrument.items(instrument = "APRA")$code
  chosen.items <- all.items[all.items %in% chosen.items]
  chosen.items.num <- which(all.items %in% chosen.items)
  
  pos.items.by.trait <- codes.from.vars(pos.loadings, chosen.items)
  neg.items.by.trait <- codes.from.vars(neg.loadings, chosen.items)
  all.items.by.trait <- codes.from.vars(loading.estimates, chosen.items)
  
  ## PARALLEL ANALYSIS
#   pa.data <- sapply(
#     BIG.FIVE.TRAITS,
#     function(trait) {
#       
#       pa.items <- all.items.by.trait[[trait]]
#       
#       responses <- RESPONSE.DATA[, pa.items]
#       
#       parallel.analysis <- fa.parallel(responses, fa = "fa", cor = 'poly')
#       
#       return(parallel.analysis)
#     },
#     USE.NAMES = TRUE, simplify = FALSE
#   )
#   save(pa.data, file = paste0(OUTPUT.PATH, "optim_pa.Rdata"))
  
  return(chosen.items)
}


create.block.set <- function(loading.estimates, angles) {
  
  tryCatch(
    load(paste0(OUTPUT.PATH, OPTIMIZATION.BLOCKS.OUTPUT)),
    error = function(e) {}
  )
  
  if(!exists("blockset.result")) {
    
    # Get all the item codes in a row:
    item.codes <- all.item.codes.from.values(loading.estimates)
    n.params <- length(item.codes) + N.BLOCKS * ITEMS.PER.BLOCK
    
    pos.loadings <- loadings.by.polarity(loading.estimates, "+")
    
    
    items <- get.instrument.items(instrument = "APRA")
    item.traits <- as.character(items$trait)
    item.pols <- as.character(items$polarity)
    names(item.traits) <- names(item.pols) <- items$code
    
    trait.pairs <- character()
    for(trait in seq_along(BIG.FIVE.TRAITS[-1]))
      trait.pairs <- c(trait.pairs, paste(BIG.FIVE.TRAITS[trait], BIG.FIVE.TRAITS[-(1:trait)], sep = "-"))
    
    item.block.constraints <- paste("item", seq_len(N.BLOCKS * ITEMS.PER.BLOCK))
    optim.constraints <- c(item.block.constraints, OPTIM.CONSTRAINT.NAMES)
    
    
    format.decision.vars <- function(decision.vars) {
      
      selected.items <- decision.vars[seq_along(item.codes)] > OPTIM.THRESHOLD
      
      item.indices <- round(decision.vars[seq_len(N.BLOCKS * ITEMS.PER.BLOCK) + length(item.codes)])
      dim(item.indices) <- c(ITEMS.PER.BLOCK, N.BLOCKS)
      item.indices <- apply(item.indices, 2, function(block) return(block[order(block)]))
      
      selected.item.codes <- item.codes[selected.items]
      
      block.item.codes <- item.codes[item.indices]
      
      block.traits <- item.traits[block.item.codes]
      block.pols <- item.pols[block.item.codes]
      
      dim(block.item.codes) <- dim(block.traits) <- dim(block.pols) <- dim(item.indices)    
      
      times.item <- sapply(seq_len(N.BLOCKS * ITEMS.PER.BLOCK), function(item) sum(item == item.indices))

      return(
        list(
          selected.item.codes = selected.item.codes,
          block.item.codes = block.item.codes, block.traits = block.traits, block.pols = block.pols,
          times.item = times.item
        )
      )
    }
    
    get.loading.matrix <- function(blocks, selected.loadings) {
      
      loading.matrix <- array(0, dim = c(ncol(blocks), length(BIG.FIVE.TRAITS)))
      colnames(loading.matrix) <- BIG.FIVE.TRAITS
      
      if(ncol(blocks) > 0) {
        
        block.traits <- as.character(item.traits[blocks])
        dim(block.traits) <- dim(blocks)
        
        for(block in seq_len(ncol(blocks))) {
          
          trait.1 <- selected.loadings[[block.traits[1, block]]]
          trait.2 <- selected.loadings[[block.traits[2, block]]]
          loading.1 <- loading.2 <- 0
          
          if(!is.null(trait.1))
            if(blocks[1, block] %in% rownames(trait.1)) loading.1 <- trait.1[blocks[1, block], 1]
          if(!is.null(trait.2))
            if(blocks[2, block] %in% rownames(trait.2)) loading.2 <- trait.2[blocks[2, block], 1]
          
          loading.matrix[block, block.traits[, block]] <- c(loading.1, -loading.2)
        }
        
        loading.matrix <- loading.matrix[rowSums(loading.matrix != 0) == ITEMS.PER.BLOCK, , drop = FALSE]
      }
      
      return(loading.matrix)
    }
    
    blocks.by.trait.pairs <- function(block.traits) {
      
      block.traits <- apply(block.traits, 2, paste, collapse = "-")
      
      n.blocks <- sapply(trait.pairs, function(pair) sum(block.traits == pair))
      
      return(n.blocks)
    }
    
    
    objective <- function(decision.vars) {
      
      dec.vars <- format.decision.vars(decision.vars)
      
      sel.item.codes <- dec.vars$selected.item.codes
      
      blocks <- dec.vars$block.item.codes
      traits.per.block <- dec.vars$block.traits
      pol.per.block <- apply(dec.vars$block.pols, 2, paste0, collapse = "")
      
      dd.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "++", drop = FALSE])
      id.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "-+", drop = FALSE])
      di.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "+-", drop = FALSE])
      heteropolar.by.trait <- c(id.by.trait.pair, di.by.trait.pair)
      
      # Now we are not going to maximize the variance of the direct item loadings
      selected.pos.loadings <- vars.in.codes(pos.loadings, sel.item.codes)
      selected.loadings <- vars.in.codes(loading.estimates, sel.item.codes)
      selected.angles <- vars.in.codes(angles, sel.item.codes)
      
      ## Item selection criteria:
      # n.ES.pos.items <- nrow(selected.pos.loadings$Neuroticismo)
      n.ES.pos.items <- nrow(vars.in.codes(pos.loadings, sel.item.codes)$Neuroticismo)
      # pos.loading.vars <- get.general.factor.variances(selected.pos.loadings)
      abs.loading.mins <- get.abs.loading.min(selected.loadings)
      # selected.angle.vars <- get.total.angle.vars(selected.angles)
      selected.angle.vars <- get.gen.angle.vars(selected.angles)
      
      pos.loading.matrix <- get.loading.matrix(blocks, selected.pos.loadings)
      pos.loadings.LSV <- if(nrow(pos.loading.matrix) > 0) svd(pos.loading.matrix)$d[ncol(pos.loading.matrix)]
      else 0
      
      # criteria <- c(selected.angle.vars, -abs.loading.mins, -pos.loading.vars, -n.ES.pos.items)
      criteria <- c(
        selected.angle.vars,
        -abs.loading.mins,
        -n.ES.pos.items,
        -pos.loadings.LSV,
        max(dd.by.trait.pair) - min(dd.by.trait.pair),
        max(heteropolar.by.trait) - min(heteropolar.by.trait)
      )
      names(criteria) <- OPTIM.CRITERIA.BLOCKS
      
#       n.iters <<- n.iters + 1
#       cat("Iteration ", n.iters, "\n")
      
      return(criteria)
    }
    
    constraints <- function(decision.vars) {
      
      dec.vars <- format.decision.vars(decision.vars)
      
      selected <- dec.vars$selected.item.codes
      times.item <- dec.vars$times.item
      
      blocks <- dec.vars$block.item.codes
      traits.per.block <- dec.vars$block.traits
      pol.per.block <- apply(dec.vars$block.pols, 2, paste0, collapse = "")
      
      n.valid.blocks <- N.BLOCKS - sum(traits.per.block[1, ] == traits.per.block[2, ])
      dd.blocks <- sum(pol.per.block == "++")
      ii.blocks <- sum(pol.per.block == "--")
      
      # traits.per.block <- dec.vars$traits.per.block[, colnames(dec.vars$valid.block.codes)]
      
      
      selected.per.trait <- sapply(vars.in.codes(loading.estimates, selected), nrow)
      
      selected.pos.loadings <- vars.in.codes(pos.loadings, selected)
      direct.selected <- sapply(selected.pos.loadings, nrow)
      inverse.selected <- selected.per.trait - direct.selected
      
      total.inverse.selected <- sum(inverse.selected)
      
      constraints <- c(
        -abs(times.item - 1) + 1,
        -abs(selected.per.trait - ITEMS.BY.TRAIT) + 1,
        direct.selected[-1] - MIN.ITEMS.PER.TRAIT.AND.POL["DIRECT"] + 1,
        inverse.selected - MIN.ITEMS.PER.TRAIT.AND.POL["INVERSE"] + 1,
        -abs(sum(direct.selected) - ITEMS.BY.POLARITY["DIRECT"] + 1),
        -abs(sum(inverse.selected) - ITEMS.BY.POLARITY["INVERSE"] + 1),
        -abs(dd.blocks - DD.BLOCKS) + 1,
        -abs(n.valid.blocks - N.BLOCKS) + 1,
        -ii.blocks + 1
      )
      names(constraints) <- optim.constraints
      
      return(constraints)
    }
    
    optim.time <- system.time(
      blockset.result <- nsga2(
        objective, idim = n.params, odim = length(OPTIM.CRITERIA.BLOCKS),
        constraints = constraints, cdim = length(optim.constraints),
        generations = N.GENERATIONS, popsize = POP.SIZE,
        lower.bounds = c(rep(0, length(item.codes)), rep(.5, N.BLOCKS * ITEMS.PER.BLOCK)),
        upper.bounds = c(rep(1, length(item.codes)), rep(N.BLOCKS * ITEMS.PER.BLOCK + .5, N.BLOCKS * ITEMS.PER.BLOCK))
      )
    )
    
    colnames(blockset.result$par) <- c(
      all.item.codes.from.values(loading.estimates),
      paste0("I_", seq_len(N.BLOCKS * ITEMS.PER.BLOCK))
    )
    rownames(blockset.result$par) <- seq_len(POP.SIZE)
    
    colnames(blockset.result$value) <- OPTIM.CRITERIA.BLOCKS
    rownames(blockset.result$value) <- seq_len(POP.SIZE)
    
    blockset.result$constraints <- apply(blockset.result$par, 1, constraints)
    blockset.result$optim.time <- optim.time
    
    save(blockset.result, file = paste0(OUTPUT.PATH, OPTIMIZATION.BLOCKS.OUTPUT, "_", N.GENERATIONS))
  }
  
  summarize.blockset.result <- function(optim.result, loading.estimates) {
    
    neg.loadings <- loadings.by.polarity(loading.estimates, "-")
    
    get.solutions <- function(parameters) {
      
      selected.item.codes <- list()
      valid.block.codes <- list()
      traits.per.block <- array(character(), dim = c(POP.SIZE, N.BLOCKS))
      pol.per.block <- array(character(), dim = c(POP.SIZE, N.BLOCKS))
      items.per.block <- array(numeric(), dim = c(POP.SIZE, N.BLOCKS))
      
      for(sol in seq_len(POP.SIZE)) {
        
        vars <- format.decision.vars(parameters[sol, ])
        
        selected.item.codes <- append(selected.item.codes, list(vars$selected.item.codes))
        valid.block.codes <- append(valid.block.codes, list(vars$valid.block.codes))
        traits.per.block[sol, ] <- apply(vars$traits.per.block, 2, paste0, collapse = "-")
        pol.per.block[sol, ] <- vars$pol.per.block
        items.per.block[sol, ] <- vars$items.per.block
      }
      
      return(
        list(
          selected.items = selected.item.codes,
          valid.blocks = valid.block.codes,
          block.traits = traits.per.block,
          block.polarities = pol.per.block,
          block.items = items.per.block
        )
      )
    }
    
    solution <- get.solutions(optim.result$par)
    
#     selected <- t(
#       sapply(
#         seq_len(POP.SIZE),
#         function(solution) return(item.codes[indexes[solution, ]]),
#         simplify = all(items.per.sol == ITEMS.BY.TRAIT * length(BIG.FIVE.TRAITS))
#       )
#     )
    
#     n.selected.pos <- t(
#       apply(
#         selected, 1,
#         function(items) {
#           
#           loadings <- vars.in.codes(pos.loadings, items)
#           n.loadings <- sapply(loadings, nrow)
#           
#           return(n.loadings)
#         }
#       )
#     )
    
#     n.selected.neg <- t(
#       apply(
#         selected, 1,
#         function(items) {
#           
#           loadings <- vars.in.codes(neg.loadings, items)
#           n.loadings <- sapply(loadings, nrow)
#           
#           return(n.loadings)
#         }
#       )
#     )
    
    # Flag the solutions with wrong number of items
    flags <- sapply(
      solution$selected.items,
      function(sol) length(solution$selected.items[[1]]) != N.BLOCKS * ITEMS.PER.BLOCK
    )
    
    # Flag the solutions with wrong number of items per block
    flags <- flags | apply(solution$block.items, 1, function(sol) any(sol != ITEMS.PER.BLOCK))
    
    valid.sols <- seq_len(POP.SIZE)[!flags]
    
    total.ang.var <- rowSums(optim.result$value[!flags, ANG.CRITERIA, drop = FALSE])
    min.abs.loading <- apply(-optim.result$value[!flags, MIN.LOAD.CRITERIA, drop = FALSE], 1, min)
    
    sol.order <- order(
      -optim.result$value[!flags, ES.DIR.ITEMS.CRITERION],
      -total.ang.var,
      min.abs.loading,
      decreasing = TRUE
    )
    best.sol <- valid.sols[sol.order[1]]
    optim.result$solution.chosen <- best.sol
    
    chosen.items <- item.codes[item.codes %in% solution$selected.items[[best.sol]]]
    
    all.items <- get.instrument.items(instrument = "APRA")$code
    chosen.items <- all.items[all.items %in% chosen.items]
    chosen.items.num <- which(all.items %in% chosen.items)
    
    pos.items.by.trait <- codes.from.vars(pos.loadings, chosen.items)
    neg.items.by.trait <- codes.from.vars(neg.loadings, chosen.items)
    all.items.by.trait <- codes.from.vars(loading.estimates, chosen.items)
    
    ## PARALLEL ANALYSIS
#     pa.data <- sapply(
#       BIG.FIVE.TRAITS,
#       function(trait) {
#         
#         pa.items <- all.items.by.trait[[trait]]
#         
#         responses <- RESPONSE.DATA[, pa.items]
#         
#         parallel.analysis <- fa.parallel(responses, fa = "fa", cor = 'poly')
#         
#         return(parallel.analysis)
#       },
#       USE.NAMES = TRUE, simplify = FALSE
#     )
#     save(pa.data, file = paste0(OUTPUT.PATH, "optim_pa.Rdata"))
    
    blocks <- t(solution$valid.blocks[[best.sol]])
    
    traits <- items$trait[which(items$code %in% blocks)]
    pols <- items$polarity[which(items$code %in% blocks)]
    dim(traits) <- dim(pols) <- dim(blocks)
    
    return(blocks)
  }
  
  return(blockset.result)
}


block.set.from.items <- function(loading.estimates, item.codes) {
  
  n.params <- N.BLOCKS * ITEMS.PER.BLOCK
  
  loading.estimates <- vars.in.codes(loading.estimates, item.codes)
  pos.loadings <- loadings.by.polarity(loading.estimates, "+")
  neg.loadings <- loadings.by.polarity(loading.estimates, "-")
  
  items <- get.instrument.items(instrument = "APRA")
  item.traits <- as.character(items$trait)
  item.pols <- as.character(items$polarity)
  names(item.traits) <- names(item.pols) <- items$code
  
  trait.pairs <- character()
  for(trait in seq_along(BIG.FIVE.TRAITS[-1]))
    trait.pairs <- c(trait.pairs, paste(BIG.FIVE.TRAITS[trait], BIG.FIVE.TRAITS[-(1:trait)], sep = "-"))
  
  item.block.constraints <- paste("item", seq_along(item.codes))
  optim.constraints <- c(item.block.constraints, OPTIM.CONSTRAINT.NAMES[17:19])

  format.decision.vars <- function(decision.vars) {
    
    item.indices <- round(decision.vars)
    dim(item.indices) <- c(ITEMS.PER.BLOCK, N.BLOCKS)
    
    item.indices <- apply(item.indices, 2, function(block) return(block[order(block)]))
    
    block.item.codes <- item.codes[item.indices]
    
    block.traits <- item.traits[block.item.codes]
    block.pols <- item.pols[block.item.codes]
    
    dim(block.item.codes) <- dim(block.traits) <- dim(block.pols) <- dim(item.indices)    
    
    times.item <- sapply(seq_along(item.codes), function(item) sum(item == item.indices))
    names(times.item) <- item.codes
    
    return(
      list(
        block.item.codes = block.item.codes, block.traits = block.traits, block.pols = block.pols,
        times.item = times.item
      )
    )
  }
  
  get.loading.matrix <- function(blocks, selected.loadings) {
    
    loading.matrix <- array(0, dim = c(ncol(blocks), length(BIG.FIVE.TRAITS)))
    colnames(loading.matrix) <- BIG.FIVE.TRAITS
    
    if(ncol(blocks) > 0) {
      
      block.traits <- as.character(item.traits[blocks])
      dim(block.traits) <- dim(blocks)
      
      for(block in seq_len(ncol(blocks))) {
        
        trait.1 <- selected.loadings[[block.traits[1, block]]]
        trait.2 <- selected.loadings[[block.traits[2, block]]]
        loading.1 <- loading.2 <- 0
        
        if(!is.null(trait.1))
          if(blocks[1, block] %in% rownames(trait.1)) loading.1 <- trait.1[blocks[1, block], 1]
        if(!is.null(trait.2))
          if(blocks[2, block] %in% rownames(trait.2)) loading.2 <- trait.2[blocks[2, block], 1]
        
        loading.matrix[block, block.traits[, block]] <- c(loading.1, -loading.2)
      }
      
      loading.matrix <- loading.matrix[rowSums(loading.matrix != 0) == ITEMS.PER.BLOCK, , drop = FALSE]
    }
    
    return(loading.matrix)
  }
  
  blocks.by.trait.pairs <- function(block.traits) {
    
    block.traits <- apply(block.traits, 2, paste, collapse = "-")
    
    n.blocks <- sapply(trait.pairs, function(pair) sum(block.traits == pair))
    
    return(n.blocks)
  }
  
  objective <- function(decision.vars) {
    
    dec.vars <- format.decision.vars(decision.vars)
    blocks <- dec.vars$block.item.codes
    traits.per.block <- dec.vars$block.traits
    pol.per.block <- apply(dec.vars$block.pols, 2, paste0, collapse = "")
    
    dd.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "++", drop = FALSE])
    id.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "-+", drop = FALSE])
    di.by.trait.pair <- blocks.by.trait.pairs(traits.per.block[, pol.per.block == "+-", drop = FALSE])
    heteropolar.by.trait <- c(id.by.trait.pair, di.by.trait.pair)
    
    pos.loading.matrix <- get.loading.matrix(blocks, pos.loadings)
    pos.loadings.LSV <- if(nrow(pos.loading.matrix) > 0) {
      svd(pos.loading.matrix)$d[ncol(pos.loading.matrix)]
    } else 0
    
    # criteria <- c(selected.angle.vars, -abs.loading.mins, -pos.loading.vars, -n.ES.pos.items)
    criteria <- c(
      -pos.loadings.LSV,
      max(dd.by.trait.pair) - min(dd.by.trait.pair),
      max(heteropolar.by.trait) - min(heteropolar.by.trait)
    )
    names(criteria) <- OPTIM.CRITERIA.BLOCKS[12:14]
    
    return(criteria)
  }
  
  constraints <- function(decision.vars) {
    
    dec.vars <- format.decision.vars(decision.vars)
    blocks <- dec.vars$block.item.codes
    traits.per.block <- dec.vars$block.traits
    pol.per.block <- apply(dec.vars$block.pols, 2, paste0, collapse = "")
    times.item <- dec.vars$times.item

    n.valid.blocks <- N.BLOCKS - sum(traits.per.block[1, ] == traits.per.block[2, ])
    dd.blocks <- sum(pol.per.block == "++")
    ii.blocks <- sum(pol.per.block == "--")

#     traits.per.block <- dec.vars$traits.per.block[, colnames(dec.vars$valid.block.codes)]
#     
#     
#     selected.per.trait <- sapply(vars.in.codes(loading.estimates, item.codes), nrow)
#     
#     selected.pos.loadings <- vars.in.codes(pos.loadings, item.codes)
#     direct.selected <- sapply(selected.pos.loadings, nrow)
#     inverse.selected <- selected.per.trait - direct.selected
#     
#     total.inverse.selected <- sum(inverse.selected)
    
    constraints <- c(
      -abs(times.item - 1) + 1,
      -abs(dd.blocks - DD.BLOCKS) + 1,
      -abs(n.valid.blocks - N.BLOCKS) + 1,
      -ii.blocks + 1
    )
    names(constraints) <- optim.constraints
    
    return(constraints)
  }

  optim.time <- system.time(
    blockset.result <- nsga2(
      objective, idim = n.params, odim = length(OPTIM.CRITERIA.BLOCKS[12:14]),
      constraints = constraints, cdim = length(optim.constraints),
      generations = N.GENERATIONS, popsize = POP.SIZE,
      lower.bounds = rep(.5, length(item.codes)),
      upper.bounds = rep(N.BLOCKS * ITEMS.PER.BLOCK + .5, length(item.codes))
    )
  )
  
  blockset.result$constraints <- apply(blockset.result$par, 1, constraints)
  blockset.result$time <- optim.time
  
  return(blockset.result)
}

ECV <- function(loadings, factors = 1, percentage = FALSE, absolute = FALSE) {
  
  if(!(is_numeric(loadings) & is_matrix(loadings)))
  	if(is_data.frame(loadings)) sapply(loadings, assert_is_numeric)
		else stop("loadings must be a numeric matrix or an all-numeric data frame")
  assert_is_numeric(factors)
  assert_all_are_less_than_or_equal_to(factors, ncol(loadings))
  
  result <- colSums(loadings[, factors, drop = FALSE]^2)
  
  if(!absolute) {
  	
  	result <- result / sum(loadings^2)
  	if(percentage) result <- result * 100
  }
  
  return(result)
}

omegaH <- function(loadings, factors = 1) {
  
  assert_is_numeric(loadings)
  assert_is_matrix(loadings)
  assert_is_numeric(factors)
  assert_all_are_less_than_or_equal_to(factors, ncol(loadings))
  
  uniqueness <- 1 - rowSums(loadings^2)
  loading.sums <- colSums(loadings)
  
  result <- loading.sums[factors]^2 / sum(loading.sums^2, uniqueness)
  
  return(result)
}

PUC <- function(loadings, general = 1, cutoff = .3, percentage = FALSE, abs = TRUE) {
  
  assert_is_numeric(loadings)
  assert_is_matrix(loadings)
  assert_is_numeric(general)
  assert_is_of_length(general, 1)
  assert_all_are_less_than_or_equal_to(general, ncol(loadings))
  
  item.names <- rownames(loadings)
  n.items <- nrow(loadings)
  relevant <- if(abs) abs(loadings[, -general]) >= cutoff else loadings[, -general] >= cutoff
  
  cont.corrs <- matrix(
    FALSE,
    nrow = n.items, ncol = n.items,
    dimnames = list(item.names, item.names)
  )
  n.corrs <- n.items * (n.items - 1) / 2
  
  for(item in item.names) {
    for(item2 in item.names) {
      
      shared.group.factor <- apply(relevant[c(item, item2), ], 2, all)
      
      if(any(shared.group.factor)) cont.corrs[item, item2] <- TRUE
    }
  }
  n.cont.corrs <- sum(cont.corrs[lower.tri(cont.corrs)])
  
  
  result <- (n.corrs - n.cont.corrs) / n.corrs
  
  if(percentage) result <- result * 100
  
  return(result)
}

item.common.var <- function(loadings, factors = colnames(loadings)) {
  
  common.var <- loadings^2
  
  result <- common.var[, factors] / rowSums(common.var)
  
  return(result)
}

improve.dropping <- function(loadings, items = rownames(loadings), general = 1, PUC.cutoff = .3) {
  
  assert_is_numeric(loadings)
  assert_is_matrix(loadings)
  assert_is_numeric(general)
  assert_is_of_length(general, 1)
  assert_all_are_less_than_or_equal_to(general, ncol(loadings))
  
  base <- c(ECV(loadings, general), omegaH(loadings, general), PUC(loadings, general, PUC.cutoff))
  
  result <- sapply(
    items,
    function(item) {
      
      remaining <- loadings[rownames(loadings) != item, ]
      
      result <- c(
        ECV(remaining, general),
        omegaH(remaining, general),
        PUC(remaining, general, PUC.cutoff)
      )
      
      return(result)
    }
  )
  
  result <- (result - base) * 100
  result <- t(result)
  
  colnames(result) <- c("ECV", "omega.h", "PUC")
  
  return(result)
}

get.Mplus.output.filename <- function(filename, dir = RESOURCE.PATH) {
	
	filename <- file(paste0(dir, filename))
	
	if(!grepl(MPLUS.MODEL.FILE.SUFFIX.REGEXP, filename)) filename <- paste0(filename, MPLUS.MODEL.SUFFIX)
	
	return(filename)
}

read.Mplus.model <- function(filename, dir = RESOURCE.PATH) {
	
	model <- readModels(target = get.Mplus.output.filename(filename, dir))
	if("mplus.model.list" %in% class(model)) model <- model[[as.mplus.model.filename(filename)]]
	
	return(model)
}

estimate.Mplus.model <- function(model.name, flush.model = FALSE, del.input = TRUE) {
  
  setwd(RESOURCE.PATH)

  if(!file.exists(paste0(model.name, MPLUS.MODEL.SUFFIX)) | flush.model) {
    
    runModels(".", filefilter = model.name, logFile = NULL)
  }
	if(del.input & file.exists(paste0(model.name, MPLUS.INPUT.SUFFIX)))
		file.remove(paste0(model.name, MPLUS.INPUT.SUFFIX))
	
	setwd("..")
	
  model <- read.Mplus.model(model.name)

  return(model)
}

as.mplus.model.filename <- function(filename, lowercase = TRUE) {
  
  if(!grepl(paste0(".+", MPLUS.MODEL.SUFFIX.REGEXP), filename)) filename <- paste0(filename, MPLUS.MODEL.SUFFIX)
  
  if(lowercase) filename <- tolower(filename)
  
  return(filename)
}

get.CFA.parameters <- function(
  model,
  type = c("unstandardized", "std.standardized", "stdyx.standardized", "stdy.standardized"),
  loadings.in.matrix = TRUE,
  values = ESTIMATE.HEADERS,
  traits = c(BIG.FIVE.TRAITS, BIG.FIVE.FACETS), use.facets = TRUE, matrix.order = c("per.trait", "per.block")
) {
  
  traits <- if(use.facets) match.arg(traits, several.ok = TRUE) else match.arg(traits, BIG.FIVE.TRAITS)
  type <- match.arg(type)
  values <- match.arg(values, several.ok = TRUE)
  
  if(identical(class(model), "character")) {
  	
  	model <- read.Mplus.model(model)
  }
  
  if(MPLUS.MODEL.CLASS %in% class(model)) {
  	
  	parameters <- model$parameters[[type]]
  	parameters$pval[parameters$pval == MPLUS.NA] <- NA
  } else
  	stop("Model must be of class Character (if it is a filename) or ", MPLUS.MODEL.CLASS, " if it is an Mplus output.")
  
  # Read and format loadings:
  loadings <- paramExtract(parameters, params = "loading")
  
  if(loadings.in.matrix) {
  	
  	matrix.order <- match.arg(matrix.order)
    
    loading.names <- unique(loadings$param)
    
    if(ESTIMATE.HEADERS["estimate"] %in% values)
      loading.estimates <- matrix(
        0,
        nrow = length(loading.names), ncol = length(traits),
        dimnames = list(variable = loading.names, factor = traits)
      )
    
    if(ESTIMATE.HEADERS["std.err"] %in% values)
      loading.SEs <- matrix(
        NA,
        nrow = length(loading.names), ncol = length(traits),
        dimnames = list(variable = loading.names, factor = traits)
      )
    
    if(ESTIMATE.HEADERS["p_value"] %in% values)
      loading.pvalues <- matrix(
        NA,
        nrow = length(loading.names), ncol = length(traits),
        dimnames = list(variable = loading.names, factor = traits)
      )
    
    for(trait in traits) {
      
      header <- get.loading.headers(trait, facets = FALSE)
      
      factor.loadings <- loadings[loadings$paramHeader == header, ]
      
      if(ESTIMATE.HEADERS["estimate"] %in% values)
        loading.estimates[factor.loadings$param, trait] <- factor.loadings[, MPLUS.COLUMN.HEADERS["est"]]
      
      if(ESTIMATE.HEADERS["std.err"] %in% values)
        loading.SEs[factor.loadings$param, trait] <- factor.loadings[, MPLUS.COLUMN.HEADERS["se"]]
      
      if(ESTIMATE.HEADERS["p_value"] %in% values)
        loading.pvalues[factor.loadings$param, trait] <- factor.loadings[, MPLUS.COLUMN.HEADERS["pval"]]
    }
    
    if(matrix.order == "per.block") {
    	
    	if(ESTIMATE.HEADERS["estimate"] %in% values) {
    		
    		block.order <- order(nchar(rownames(loading.estimates)), rownames(loading.estimates))
    		loading.estimates <- loading.estimates[block.order, ]
    	}
    	
    	if(ESTIMATE.HEADERS["std.err"] %in% values) {
    		
    		block.order <- order(nchar(rownames(loading.SEs)), rownames(loading.SEs))
    		loading.SEs <- loading.SEs[block.order, ]
    	}
    	
    	if(ESTIMATE.HEADERS["p_value"] %in% values) {
    		
    		block.order <- order(nchar(rownames(loading.pvalues)), rownames(loading.pvalues))
    		loading.pvalues <- loading.pvalues[block.order, ]
    	}
    }
    
    loadings.result <- list()
    if(ESTIMATE.HEADERS["estimate"] %in% values) {
      
      loadings.result <- append(loadings.result, list(loading.estimates))
      names(loadings.result)[length(loadings.result)] <- ESTIMATE.HEADERS["estimate"]
    }
    if(ESTIMATE.HEADERS["std.err"] %in% values) {
      
      loadings.result <- append(loadings.result, list(loading.SEs))
      names(loadings.result)[length(loadings.result)] <- ESTIMATE.HEADERS["std.err"]
    }
    if(ESTIMATE.HEADERS["p_value"] %in% values) {
      
      loadings.result <- append(loadings.result, list(loading.pvalues))
      names(loadings.result)[length(loadings.result)] <- ESTIMATE.HEADERS["p_value"]
    }
  } else {
    
    loadings.result <- list()
    
    for(trait in traits) {
      
      header <- get.loading.headers(trait, facets = FALSE)
      
      select.loadings <- loadings[loadings$paramHeader == header, ]
      
      factor.loadings <- as.matrix(select.loadings[, MPLUS.COLUMN.HEADERS[ESTIMATE.HEADERS %in% values], drop = FALSE])
      dimnames(factor.loadings) <- list(variable = select.loadings$param, values)
      
      loadings.result <- append(loadings.result, list(factor.loadings))
      names(loadings.result)[length(loadings.result)] <- trait
    }
  }
  
  if(length(traits) > 1) {
  	
  	# Read and format correlations:
  	corrs <- paramExtract(parameters, params = "undirected")
  	
  	if(ESTIMATE.HEADERS["estimate"] %in% values)
  		corr.matrix.est <- matrix(1, nrow = length(traits), ncol = length(traits), dimnames = list(traits, traits))
  	
  	if(ESTIMATE.HEADERS["std.err"] %in% values)
  		corr.matrix.SEs <- matrix(NA, nrow = length(traits), ncol = length(traits), dimnames = list(traits, traits))
  	
  	if(ESTIMATE.HEADERS["p_value"] %in% values)
  		corr.matrix.pval <- matrix(NA, nrow = length(traits), ncol = length(traits), dimnames = list(traits, traits))
  	
  	for(trait in traits) {
  		
  		header <- get.corr.headers(trait, facets = FALSE)
  		corrs.results <- corrs[corrs$paramHeader == header, ]
  		
  		if(nrow(corrs.results) > 0) {
  			
  			corr.1 <- get.trait.name.from.dimension(header, MPLUS.CORR.SUFFIX)
  			corrs.2 <- get.trait.name.from.dimension(corrs.results$param)
  			
  			if(ESTIMATE.HEADERS["estimate"] %in% values)
  				corr.matrix.est[corrs.2, corr.1] <- corr.matrix.est[corr.1, corrs.2] <- corrs.results$est
  			
  			if(ESTIMATE.HEADERS["std.err"] %in% values)
  				corr.matrix.SEs[corrs.2, corr.1] <- corr.matrix.SEs[corr.1, corrs.2] <- corrs.results$se
  			
  			if(ESTIMATE.HEADERS["p_value"] %in% values)
  				corr.matrix.pval[corrs.2, corr.1] <- corr.matrix.pval[corr.1, corrs.2] <- corrs.results$pval
  		}
  	}
  	
  	corrs.result <- list()
  	if(ESTIMATE.HEADERS["estimate"] %in% values) {
  		
  		corrs.result <- append(corrs.result, list(corr.matrix.est))
  		names(corrs.result)[length(corrs.result)] <- ESTIMATE.HEADERS["estimate"]
  	}
  	if(ESTIMATE.HEADERS["std.err"] %in% values) {
  		
  		corrs.result <- append(corrs.result, list(corr.matrix.SEs))
  		names(corrs.result)[length(corrs.result)] <- ESTIMATE.HEADERS["std.err"]
  	}
  	if(ESTIMATE.HEADERS["p_value"] %in% values) {
  		
  		corrs.result <- append(corrs.result, list(corr.matrix.pval))
  		names(corrs.result)[length(corrs.result)] <- ESTIMATE.HEADERS["p_value"]
  	}
  } else corrs.result <- NULL
  
  
  means <- paramExtract(parameters, "expectation")
  
  # Read and format thresholds:
  thresholds <- means[means$paramHeader == MPLUS.THRESHOLD.HEADER, ]
  rownames(thresholds) <- thresholds$param
  thresholds <- thresholds[, MPLUS.COLUMN.HEADERS[ESTIMATE.HEADERS %in% values], drop = FALSE]
  colnames(thresholds) <- values
  
  # Read and format latent variable means:
  factor.means <- means[means$paramHeader == MPLUS.LV.MEAN.HEADER, ]
  if(nrow(factor.means) > 0) {
    
  	factor.means <- factor.means[factor.means$param %in% get.model.name.from.trait(traits, abbreviation = TRUE), ]
  	rownames(factor.means) <- traits
    factor.means <- factor.means[, MPLUS.COLUMN.HEADERS[ESTIMATE.HEADERS %in% values], drop = FALSE]
    colnames(factor.means) <- values
  }
  
  
  variances <- paramExtract(parameters, "variability")
  
  # Read and format variable unicities:
  unicities <- variances[variances$paramHeader == MPLUS.UNICITY.HEADER, ]
  if(nrow(unicities) > 0) {
  	
  	rownames(unicities) <- unicities$param
  	unicities <- unicities[, MPLUS.COLUMN.HEADERS[ESTIMATE.HEADERS %in% values], drop = FALSE]
  	colnames(unicities) <- values
  }
  
  # Read and format variable latent variable variances:
  factor.vars <- variances[variances$paramHeader == MPLUS.LV.VAR.HEADER, ]
  factor.vars <- factor.vars[factor.vars$param %in% get.model.name.from.trait(traits, abbreviation = TRUE), ]
  rownames(factor.vars) <- traits
  factor.vars <- factor.vars[, MPLUS.COLUMN.HEADERS[ESTIMATE.HEADERS %in% values], drop = FALSE]
  colnames(factor.vars) <- values
  
  result <- list(
    loadings = loadings.result,
    corrs = corrs.result,
    thresholds = thresholds,
    latent.means = factor.means,
    unicities = unicities,
    latent.vars = factor.vars
  )
  
  return(result)
}

get.TIRT.parameters <- function(
  model, specification, invert.dims = c("none", BIG.FIVE.TRAITS),
  type = c("unstandardized", "std.standardized", "stdyx.standardized", "stdy.standardized"), compare.polarity = FALSE,
  traits = BIG.FIVE.TRAITS, arrange = c("per.trait", "per.block"), values = ESTIMATE.HEADERS
) {
  
  traits <- match.arg(traits, several.ok = TRUE)
  type <- match.arg(type)
  arrange <- match.arg(arrange)
  values <- match.arg(values, several.ok = TRUE)
  invert.dims <- if(invert.dims[1] == "none") NA else match.arg(invert.dims, BIG.FIVE.TRAITS, several.ok = TRUE)
  
  loadings.result <- list()
  if(arrange == "per.trait") {
  	
  	par.estimates <- get.CFA.parameters(
  		model,
  		type = type, values = values, loadings.in.matrix = FALSE, traits = traits
  	)
  	
  	specification <- get.trait.blocks(specification, trait = traits, discard.traits = TRUE)
  	
  	for(trait in traits) {
  		
  		item.1.blocks <- get.trait.blocks(specification, trait = trait, in.pos = "first")
  		item.2.blocks <- get.trait.blocks(specification, trait = trait, in.pos = "second")
  		
  		trait.loadings <- par.estimates$loadings[[trait]]
  		
  		if(ESTIMATE.HEADERS["estimate"] %in% values) {
  			
  			trait.loadings[rownames(item.2.blocks), ESTIMATE.HEADERS["estimate"]] <-
  				-trait.loadings[rownames(item.2.blocks), ESTIMATE.HEADERS["estimate"]]
  			
  			if(trait %in% invert.dims) {
  				
  				if(ESTIMATE.HEADERS["estimate"] %in% values)
  					trait.loadings[, ESTIMATE.HEADERS["estimate"]] <- -trait.loadings[, ESTIMATE.HEADERS["estimate"]]
  				
  				par.estimates$corrs$estimate[trait, ] <- -par.estimates$corrs$estimate[trait, ]
  				par.estimates$corrs$estimate[, trait] <- -par.estimates$corrs$estimate[, trait]
  			}
  		}
  		
  		if(compare.polarity) {
  			
  			polarities <- c(item.1.blocks$polarity.item.1, item.2.blocks$polarity.item.2)
  			polarities <- factor(polarities, labels = POLARITY[c(1 %in% polarities, 2 %in% polarities)])
  			
  			trait.loadings <- data.frame(trait.loadings, polarity = polarities)
  		}
  		
  		loadings.result <- append(loadings.result, list(trait.loadings))
  		names(loadings.result)[length(loadings.result)] <- trait
  	}
  } else {
  	
  	par.estimates <- get.CFA.parameters(
  		model,
  		type = type, values = values, loadings.in.matrix = TRUE, traits = traits, matrix.order = "per.block"
  	)
  	loading.pars <- par.estimates$loadings
  	for(trait in BIG.FIVE.TRAITS)
  		if(trait %in% invert.dims) loading.pars[, trait] <- -loading.pars[, trait]
  	
  	if(ESTIMATE.HEADERS["estimate"] %in% values) {
  		
  		loading.estimates <- matrix(
  			0,
  			nrow = nrow(specification), ncol = 2,
  			dimnames = list(block = rownames(specification), item = c("item.1", "item.2"))
  		)
  	}
  	
  	if(ESTIMATE.HEADERS["std.err"] %in% values)
  		loading.SEs <- matrix(
  			NA,
  			nrow = nrow(specification), ncol = 2,
  			dimnames = list(block = rownames(specification), item = c("item.1", "item.2"))
  		)
  	
  	if(ESTIMATE.HEADERS["p_value"] %in% values)
  		loading.pvalues <- matrix(
  			NA,
  			nrow = nrow(specification), ncol = 2,
  			dimnames = list(block = rownames(specification), item = c("item.1", "item.2"))
  		)
  	
  	for(trait in traits) {
  		
  		blocks.item.1 <- rownames(get.trait.blocks(specification, trait, in.pos = "first"))
  		blocks.item.2 <- rownames(get.trait.blocks(specification, trait, in.pos = "second"))
  		
  		if(ESTIMATE.HEADERS["estimate"] %in% values) {
  			
  			loading.estimates[blocks.item.1, "item.1"] <- loading.pars$estimate[blocks.item.1, trait]
  			loading.estimates[blocks.item.2, "item.2"] <- -loading.pars$estimate[blocks.item.2, trait]
  		}
  		
  		if(ESTIMATE.HEADERS["std.err"] %in% values) {
  			
  			loading.SEs[blocks.item.1, "item.1"] <- loading.pars$std.err[blocks.item.1, trait]
  			loading.SEs[blocks.item.2, "item.2"] <- loading.pars$std.err[blocks.item.2, trait]
  		}
  		
  		if(ESTIMATE.HEADERS["p_value"] %in% values) {
  			
  			loading.pvalues[blocks.item.1, "item.1"] <- loading.pars$p_values[blocks.item.1, trait]
  			loading.pvalues[blocks.item.2, "item.2"] <- loading.pars$p_values[blocks.item.2, trait]
  		}
  	}
  	
  	if(ESTIMATE.HEADERS["estimate"] %in% values)
  		loadings.result <- append(loadings.result, list(loading.estimates))
  	
  	if(ESTIMATE.HEADERS["std.err"] %in% values)
  		loadings.result <- append(loadings.result, list(loading.SEs))
  	
  	if(ESTIMATE.HEADERS["p_value"] %in% values)
  		loadings.result <- append(loadings.result, list(loading.pvalues))
  	
  	names(loadings.result) <- values
  }
  
  par.estimates$loadings <- loadings.result
  
  threshold.index <- rownames(par.estimates$thresholds) %in% get.threshold.names(specification, ITEM.TYPES["block"])
  par.estimates$thresholds <- par.estimates$thresholds[threshold.index, ]
  rownames(par.estimates$thresholds) <- rownames(specification)
  
  return(par.estimates)
}

get.threshold.names <- function(variables, type = ITEM.TYPES) {
	
	type = match.arg(type)
	
	thresholds <- if(type == ITEM.TYPES["item"]) GSQ.THRESHOLDS else FCQ.THRESHOLDS
	
	result <- sapply(rownames(variables), paste, thresholds, sep = "$")
	dim(result) <- NULL
	
	return(result)
}

explore.polarities <- function(model.loadings) {
  
  lapply(model.loadings, function(model.loadings) ftable(sign(model.loadings$estimate), model.loadings$polarity))
}

get.unmatching.polarities <- function(loadings, traits = c(BIG.FIVE.TRAITS, BIG.FIVE.FACETS)) {
  
  traits <- match.arg(traits, several.ok = TRUE)
  
  unmatching.items <- data.frame(stringsAsFactors = FALSE)
  
  for(trait in traits) {
    
    loading.index <- xor(loadings[[trait]]$estimate > 0, loadings[[trait]]$polarity == POLARITY["+"])
    
    new.loadings <- loadings[[trait]][loading.index, ]
    
    if(nrow(new.loadings) > 0) {
      
      new.loadings <- cbind(variable = rownames(new.loadings), new.loadings, trait = trait, stringsAsFactors = FALSE)
      
      unmatching.items <- rbind(unmatching.items, new.loadings, stringsAsFactors = FALSE)
    }
  }
  
  rownames(unmatching.items) <- seq_len(nrow(unmatching.items))
  
  return(unmatching.items)
}

get.bifactor.EFA.parameters <- function(
  model, specification, invert.dims = c("none", BIG.FIVE.TRAITS, BIG.FIVE.FACETS),
  type = c("unstandardized", "std.standardized", "stdyx.standardized", "stdy.standardized"), compare.polarity = FALSE,
  values = ESTIMATE.HEADERS,
  trait = BIG.FIVE.TRAITS, use.facets = TRUE
) {
  
  trait <- match.arg(trait, several.ok = TRUE)
  type <- match.arg(type)
  invert.dims <- if(invert.dims[1] == "none") NA
    else match.arg(invert.dims, c(BIG.FIVE.TRAITS, BIG.FIVE.FACETS), several.ok = TRUE)
  
  traits <- if(use.facets) c(trait, get.facets(trait)) else trait
  
  par.estimates <- get.CFA.parameters(
    model,
    type = type, values = values, loadings.in.matrix = FALSE, traits = traits, use.facets = use.facets
  )
  
  specification <- get.trait.items(specification, trait = trait)
  
  loadings <- list()
  for(trait in traits) {
    
    trait.loadings <- par.estimates$loadings[[trait]]
    
    if(trait %in% invert.dims) {
      
      trait.loadings[, ESTIMATE.HEADERS["estimate"]] <- -trait.loadings[, ESTIMATE.HEADERS["estimate"]]
      
      if(!is.null(par.estimates$corrs)) {
      	
      	par.estimates$corrs$estimate[trait, ] <- -par.estimates$corrs$estimate[trait, ]
      	par.estimates$corrs$estimate[, trait] <- -par.estimates$corrs$estimate[, trait]
      }
    }
    
    if(compare.polarity) {
      
    	loadings.with.pol <- as.data.frame(
    		matrix(
    			c(0, NA, NA),
    			nrow = nrow(specification), ncol = 3, byrow = TRUE,
    			dimnames = list(vars = rownames(specification), ESTIMATE.HEADERS)
    		)
    	)
    	loadings.with.pol <- loadings.with.pol[ESTIMATE.HEADERS %in% values]
    	loadings.with.pol[rownames(trait.loadings), ] <- trait.loadings
    	
      trait.loadings <- data.frame(loadings.with.pol, polarity = specification$polarity)
    }
    
    loadings <- append(loadings, list(trait.loadings))
    names(loadings)[length(loadings)] <- trait
  }
  
  par.estimates$loadings <- loadings
  
  return(par.estimates)
}

get.psych.model.summary <- function(model) {
	
	model.summary <- with(
		model,
		data.frame(
			`Fit index` = fit, `Off-diagonal fit` = fit.off, `Degrees of freedom` = dof,
			`Objective function value` = objective, `Chi square` = STATISTIC, `Chi square p-value` = PVAL,
			`Empirial Chi square` = chi, `RMSR` = rms, `Df-corrected RMSR` = crms,
			RMSEA = as.data.frame(t(RMSEA)), TLI = TLI, BIC = BIC
		)
	)
}

assess.unidimensional.model <- function(bifactor.loadings, responses) {
	
  ## Comparison with the unidimensional model
  response.cors <- polychoric(responses, smooth = FALSE, global = FALSE, correct = FALSE)
  fit.unidimensional <- fa(
  	response.cors$rho,
  	nfactors = 1, n.obs = nrow(responses), cor = "poly", fm = "wls",
  	rotate = "none"
  )
  
  bifactor.loadings <- bifactor.loadings[names(bifactor.loadings) != "polarity"]
  ff.bifactor.loadings <- bifactor.loadings$estimate
  unidim.loadings <- unclass(fit.unidimensional$loadings)
  
  result <- list(
  	ff.ECV = ECV(bifactor.loadings),
  	loadings.corr = cor(ff.bifactor.loadings, unidim.loadings),
  	loadings.mean.bias = mean(unidim.loadings - ff.bifactor.loadings),
  	loadings.rmse = sqrt(mean((unidim.loadings - ff.bifactor.loadings)^2)),
  	unidim.summary = get.psych.model.summary(fit.unidimensional)
  )
  
  return(result)
}

assess.Mplus.unidimensional.model <- function(
	unidim.model, trait, items, invert = FALSE, estimator = c("ULSMV", "ML"), estimate.bifactor = TRUE
) {
	
	estimator <- match.arg(estimator)
	
	unidim.loadings <- get.CFA.parameters(
		unidim.model, traits = trait, values = ESTIMATE.HEADERS[c("estimate", "p_value")]
	)$loadings$estimate
	
	## Estimate the bi-factor model to compare:
	file.name <- paste(
		MPLUS.INPUT.BIFACTOR.FILE.PREFIX, "final", get.model.name.from.trait(trait, abbreviation = TRUE), ESTIMATOR,
		sep = "_"
	)
	if(estimate.bifactor) {
		
		write.Mplus.bifactor.CFA.model(
			paste0(file.name, MPLUS.INPUT.SUFFIX),
			MPLUS.RESPONSE.TEMP.FILE,
			items, iteration = "final", trait = trait,
			template.file = MPLUS.TEMPLATE.FILE,
			specific.factors = TRUE, estimator = estimator
		)
	}

	model.result <- estimate.Mplus.model(file.name)
	
	## Compute explained common variance
	bifactor.loadings <- get.CFA.parameters(
		model.result,
		loadings.in.matrix = TRUE, traits = c(trait, get.facets(trait)), values = ESTIMATE.HEADERS["estimate"]
	)$loadings$estimate
	
	bifactor.loadings <- SLi.rotation(bifactor.loadings, create.target(items, trait))
	
	if(invert) bifactor.loadings <- -bifactor.loadings
	
	common.vars <- item.common.var(bifactor.loadings, 1)
	
	# Reorder the matrix so the first factor has the largest explained common variance
	factor.ECVs <- ECV(bifactor.loadings, 1:ncol(bifactor.loadings))
	factor.names <- colnames(bifactor.loadings)
	bifactor.loadings <- bifactor.loadings[, order(factor.ECVs, decreasing = TRUE)]
	colnames(bifactor.loadings) <- factor.names
	
	ff.bifactor.loadings <- bifactor.loadings[, 1]
	
	summary <- model.result$summaries
	summary <- summary[
		!(names(summary) %in% c("Mplus.version", "Title", "AnalysisType", "DataType", "Observations", "Filename"))
	]
	
	bias <- unidim.loadings - ff.bifactor.loadings
	rel.bias <- bias / ff.bifactor.loadings
	
	result <- data.frame(
		ECV = ECV(bifactor.loadings),
		loadings.corr = cor(ff.bifactor.loadings, unidim.loadings[, 1]),
		loadings.mean.bias = mean(bias, na.rm = TRUE),
		loadings.bias.sd = sd(bias, na.rm = TRUE),
		loadings.rel.bias = mean((bias) / ff.bifactor.loadings),
		loadings.rel.bias.min = min(bias, na.rm = TRUE) * 100,
		loadings.rel.bias.max = max(bias, na.rm = TRUE) * 100,
		loadings.rmse = sqrt(mean((unidim.loadings - ff.bifactor.loadings)^2))
	)
	result <- cbind(result, summary)
	
	return(
		list(
			fit = result,
			common.vars = common.vars,
			bf.general = ff.bifactor.loadings
		)
	)
}

vgQ.unimax <- function(L) {
	nfactors <- ncol(L)
	nvars <- nrow(L)
	
	eigenvalues <- eigen(t(L) %*% L)$values
	ECVs <- ECV(L, factors = 1:ncol(L), absolute = TRUE)
	
	gradient <- -t(t(L) * (sqrt(eigenvalues) - sqrt(ECVs)) / sqrt(eigenvalues)) * sqrt(.5)
	
	return(list(f = eigenvalues[1] - ECVs[1], Gq = gradient, Method = "unimax"))
}

select.cutoff.threshold <- function(thresholds, items, threshold = GSQ.THRESHOLDS, threshold.inv = threshold) {
	
	threshold <- match.arg(threshold)
	threshold.inv <- match.arg(threshold.inv, GSQ.THRESHOLDS)
	
	item.name.regexp <- paste0("P[45]_[0-9]{1,3}\\$", threshold)
	item.name.regexp.inv <- paste0("P[45]_[0-9]{1,3}\\$", threshold.inv)
	
	
	result <- thresholds[grep(item.name.regexp, rownames(thresholds)), , drop = FALSE]
	rownames(result) <- sub(paste0("\\$", threshold), "", rownames(result))
	
	result.inv <- thresholds[grep(item.name.regexp.inv, rownames(thresholds)), , drop = FALSE]
	rownames(result.inv) <- sub(paste0("\\$", threshold.inv), "", rownames(result.inv))
	
	items <- items[rownames(items) %in% rownames(result), ]
	result <- result[rownames(items), , drop = FALSE]
	result.inv <- result.inv[rownames(items), , drop = FALSE]
	
	for(result.col in ncol(result))
		result[, result.col] <- ifelse(items$polarity == POLARITY["+"], result[, result.col], result.inv[, result.col])
	
	return(result)
}

grm <- function(discrimination, thresholds, thetas) {
	
	logistic <- function(logit) return(1/(1 + exp(-logit)))
	
	categories <- as.character(0:length(thresholds))
	
	probs <- matrix(
		nrow = length(thetas), ncol = length(categories),
		dimnames = list(format(round(thetas, digits = 3)), categories))
	
	probs[, 1] <- 1 - logistic(discrimination * thetas + thresholds[1])
	
	for(category in 2:length(thresholds))
		probs[, category] <- logistic(discrimination * thetas + thresholds[category - 1]) -
			logistic(discrimination * thetas + thresholds[category])
	
	probs[, ncol(probs)] <- logistic(discrimination * thetas + thresholds[ncol(probs) - 1])
	
	return(probs)
}

corrected.LR.test <- function(unrestricted.LL, unrestricted.cf, unrestricted.pars,
																	 restricted.LL, restricted.cf, restricted.pars) {
	
	# TSC = Test scaling correction
	diff.TSC <- (restricted.pars * restricted.cf - unrestricted.pars * unrestricted.cf) /
		(restricted.pars - unrestricted.pars)
	
	Chi.Sq <- -2 * (restricted.LL - unrestricted.LL) / diff.TSC
	df = unrestricted.pars - restricted.pars
	p.value <- pchisq(Chi.Sq, df, lower.tail = FALSE)
	
	return(c(value = Chi.Sq, df = df, p = p.value))
}

compute.model10.cf <- function(restricted.file.name) {
	
	model.file.name <- paste(restricted.file.name, "Model_10", sep = "_")
	
	restricted.model <- readLines(paste0(RESOURCE.PATH, restricted.file.name, ".out"))
	
	model10.spec.range <- restricted.model %>% {
		c(
			grep(pattern = "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES", x = .) + 1,
			grep(pattern = "RESIDUAL OUTPUT", x = .) - 1
		)
	}
	model10.spec <- restricted.model[ model10.spec.range %>% { seq(.[1], .[2]) } ]
	
	model10.template <- readLines(paste0(RESOURCE.PATH, "Model_10.inp.Template"))
	model10.template <- model10.template %>% append(model10.spec, after = 155)
	model10.template %>% writeLines(con = paste0(RESOURCE.PATH, model.file.name, ".inp"))
	
	model10 <- estimate.Mplus.model(model.file.name)
	
	return(model10$summaries$LLCorrectionFactor)
}
