
### INCLUDES:

### CONSTANTS:

## MPLUS FILES RELEVANT CONSTANTS:
MPLUS.NA <- "99"

MPLUS.TEMPLATE.FILE <- "Mplus_exploratory_bifactor.inp.Template"
MPLUS.TIRT.TEMPLATE.FILE <- "Mplus_FCQ_TIRT.inp.Template"
MPLUS.TIRT.ML.TEMPLATE.FILE <- "Mplus_FCQ_TIRT_ML.inp.Template"
MPLUS.BIFACTOR.CFA.TEMPLATE.FILE <- "Mplus_bifactor_CFA.inp.Template"
MPLUS.UNIDIMENSIONAL.CFA.TEMPLATE.FILE <- "Mplus_unidimensional_CFA.inp.Template"
MPLUS.JOINT.CFA.TEMPLATE.FILE <- "Mplus_FCQ_and_GSQ_CFA.inp.Template"
MPLUS.CFA.TEMPLATE.FILE <- "Mplus_GSQ_CFA.inp.Template"

MPLUS.INPUT.FILE.PREFIX <- "APRA2_FCB_TIRT_iteration"
MPLUS.INPUT.BIFACTOR.FILE.PREFIX <- "APRA2_Bifactor_iteration"
MPLUS.INPUT.UNIDIMENSIONAL.PREFIX <- "APRA2_Unidimensional_iteration"
MPLUS.INPUT.DICHOTOMIZED.PREFIX <- "APRA2_Dichotomized_unidim_"
MPLUS.INPUT.JOINT.PREFIX <- "APRA2_FCQ_GSQ_joint"
MPLUS.INPUT.JOINT.WALDTEST.PREFIX <- "Joint_FCQ_GSQ_Wald_test"
MPLUS.MODEL.FILE.SUFFIX <- ".inp"
MPLUS.MODEL.FILE.SUFFIX.REGEXP <- paste0("\\", MPLUS.MODEL.FILE.SUFFIX, "$")

OPEN.SQ.BRACKET.REGEXP <- "\\["
CLOSE.SQ.BRACKET.REGEXP <- "\\]"
OPEN.REPEATABLE.ENCLOSING <- "<.*"
CLOSE.REPEATABLE.ENCLOSING <- ".*>"

MPLUS.OPTION.TRAIT <- "TRAIT"
MPLUS.OPTION.ITERATION <- "it"
MPLUS.OPTION.RESPONSE.FILE <- "MPLUS_RESPONSE_FILE_PATH"
MPLUS.OPTION.ITEM.NAMES <- "ITEM_NAMES"
MPLUS.OPTION.BLOCK.NAMES <- "BLOCK_NAMES"
MPLUS.OPTION.SELECTED.ITEMS <- "SEL_ITEM_NAMES"
MPLUS.OPTION.TRAIT.ITEMS <- "TRAIT_ITEMS"
MPLUS.OPTION.MISSING.VALUE <- "MPLUS_MISSING_VALUE"
MPLUS.OPTION.FACET <- "Facet"
MPLUS.OPTION.FACET.INDICATORS <- "facet_indicators"
MPLUS.OPTION.FACET.NON.INDICATORS <- "non_indicators"
MPLUS.OPTION.ITEM.RANGE <- "ITEM_RANGE"
MPLUS.OPTION.BLOCK.RANGE <- "BLOCK_RANGE"
MPLUS.OPTION.TRAIT.RANGE <- "TRAIT_RANGE"
MPLUS.OPTION.DIM <- "DIM"
MPLUS.OPTION.ESTIMATOR <- "estimator"
MPLUS.OPTION.MODINDICES <- "MODINDICES"
MPLUS.OPTION.PARAMETERIZATION <- "PARAMETERIZATION"
MPLUS.OPTION.UNIQUENESSESS <- "UNIQUENESSES"
MPLUS.OPTION.JOINT.UNIQUENESSESS <- "JOINT_UNIQUENESSES"
MPLUS.OPTION.INTEGRATION <- "INTEGRATION"
MPLUS.OPTION.FREE.LOADING <- "FREE_LOADING"
MPLUS.OPTION.BLOCK <- "BLOCK"
MPLUS.OPTION.ITEM <- "ITEM"
MPLUS.OPTION.MODEL.CONSTRAINT <- "MODEL_CONSTRAINT"
MPLUS.OPTION.MODEL.TEST <- "MODEL_TEST"
MPLUS.OPTION.UNIQUENESSES.CORRS <- "UNIQUENESSES_CORRS"
MPLUS.OPTION.THRESHOLD.CONSTRAINTS <- "THRESHOLD_CONSTRAINTS"
MPLUS.OPTION.DIFFTEST.RUN <- "DIFFTEST_RUN"
MPLUS.OPTION.DIFFTEST.SAVE <- "DIFFTEST_SAVE"
MPLUS.OPTION.LISTWISE.DELETION <- "LISTWISE_DELETION"


MPLUS.OPTION.SUFFIX.BLOCKS <- "_BLOCKS"
MPLUS.OPTION.SUFFIX.ITEMS <- "_ITEMS"
MPLUS.OPTION.SUFFIX.ITEM.PAIR <- "_ITEM_PAIR"
MPLUS.OPTION.SUFFIX.BLOCK.ITEM.PAIR <- "_BLOCK_AND_ITEM_PAIR"
MPLUS.OPTION.SUFFIX.BLOCK.ITEM <- "_BLOCK_AND_ITEM"
MPLUS.OPTION.SUFFIX.BLOCK.IN.ITEM <- "ITEM_BLOCK"


MPLUS.LOADING.INIT.POSITIVE <- "1"
MPLUS.LOADING.INIT.NEGATIVE <- "-1"
MPLUS.INITIALIZE.AT <- "*"
MPLUS.ALL.VARIABLES <- "ALL"
MPLUS.NEW.LINE <- "\n"
MPLUS.TAB <- "\t"
MPLUS.BLOCKS.LINE.FEED <- paste0(MPLUS.NEW.LINE, paste0(rep(MPLUS.TAB, 3), collapse = ""))
MPLUS.OPTION.LINE.FEED <- paste0(MPLUS.NEW.LINE, paste0(rep(MPLUS.TAB, 2), collapse = ""))
MPLUS.COMMAND.END <- ";"
MPLUS.FREE.LOADING.CHAR <- "*"
MPLUS.JOINT.UNIQUENESSES.VALUE <- paste0("[ITEM_RANGE]@1;", MPLUS.NEW.LINE, MPLUS.TAB, MPLUS.TAB, "[BLOCK_RANGE]@2;")
MPLUS.MODEL.CONSTRAINT.HEADER <- "MODEL CONSTRAINT:"


RESOURCE.PATH <- "res/"

MPLUS.RESPONSE.FILE <- "Mplus_responses.dat"
MPLUS.RESPONSE.FILE.APRA2 <- "Mplus_responses_APRA2.dat"
MPLUS.RESPONSE.FILE.APRA2.BLOCKS <- "Mplus_block_responses_APRA2.dat"
MPLUS.RESPONSE.TEMP.FILE <- "Mplus_responses_temp.dat"

LIKERT.RESULTS.FILE <- "Filter_out_likert_results"
FCQ.RESULTS.FILE <- "Filter_out_FCQ_results.Rdata"
FCQ.RESULTS.FILE.ML <- "Filter_out_FCQ_results_ML.Rdata"
FINAL.FIT.RESULTS.PREFFIX <- "Final_fit_results_"
RDATA.SUFFIX <- ".Rdata"


### FUNCTIONS:

print.fa.loadings <- function(loadings, items, trait = BIG.FIVE.TRAITS, cutoff = .3, abs = TRUE) {

  trait <- match.arg(trait)

  items <- select.items.by.trait(items, trait)

  n.original.loadings <- nrow(loadings)
  loadings <- as.data.frame(unclass(loadings))
  loadings <- loadings[rownames(loadings) %in% rownames(items), ]
  if(nrow(loadings) < n.original.loadings) warning("Loadings not corresponding to trait ", trait, " dropped.")

  names(loadings) <- c(GENERAL.FACTOR.NAME, get.facets(trait, ordered = TRUE))

  if(abs) {

    loadings[abs(loadings) < cutoff] <- NA
  } else {

    loadings[loadings < cutoff] <- NA
  }

  output.loadings <- format(loadings, digits = 3)
  output.loadings[sapply(output.loadings, function(f) grepl("NA", f))] <- " "

  output <- cbind(output.loadings, Facet = items[rownames(loadings), "facet"])

  print(output, digits = 3)
}

print.fa.block.loadings <- function(loadings, blocks, cutoff = .3, abs = TRUE) {

  n.original.loadings <- nrow(loadings)
  loadings <- as.data.frame(unclass(loadings))
  loadings <- loadings[rownames(loadings) %in% rownames(blocks), ]
  if(nrow(loadings) < n.original.loadings) warning("Loadings not corresponding to forced choice blocks dropped.")

  names(loadings) <- BIG.FIVE.TRAITS

  if(abs) {

    loadings[abs(loadings) < cutoff] <- NA
  } else {

    loadings[loadings < cutoff] <- NA
  }

  output.loadings <- format(loadings, digits = 3)
  output.loadings[sapply(output.loadings, function(f) grepl("NA", f))] <- " "

  print(output.loadings, digits = 3)
}

write.Mplus.data <- function(table = RESPONSE.DATA, file = MPLUS.RESPONSE.FILE) {

  write.table(table, file = paste0(DATA.PATH, file), sep = "\t", na = MPLUS.NA, col.names = FALSE, row.names = FALSE)
}

mplus.option <- function(option, repeatable = FALSE) {

  result <- paste0(OPEN.SQ.BRACKET.REGEXP, option, CLOSE.SQ.BRACKET.REGEXP)

  if(repeatable) {

    result <- paste0("^", OPEN.REPEATABLE.ENCLOSING, result, CLOSE.REPEATABLE.ENCLOSING, MPLUS.COMMAND.END, "$")
  }

  return(result)
}

set.mplus.option <- function(file.content, option, value) {

  if(length(value) > 1) value <- paste0(value, collapse = MPLUS.OPTION.LINE.FEED)

  is.repeatable <- grep(mplus.option(option, repeatable = TRUE), file.content)

  result <- gsub(mplus.option(option), value, file.content)

  if(!is_empty(is.repeatable)) {

    for(line in rev(is.repeatable)) {

      result[line] <- gsub("[<>]", "", result[line])
      result <- append(result, file.content[line], after = line)
    }
  }

  return(result)
}

clean.mplus.output <- function(file.content) {

  repeatables <- grepl(paste0(OPEN.REPEATABLE.ENCLOSING, CLOSE.REPEATABLE.ENCLOSING), file.content)

  file.content <- file.content[!repeatables]

  return(file.content)
}

get.Mplus.input.file <- function(filename, dir = RESOURCE.PATH) {
	
	filename <- paste(dir, filename, sep = "/")
	
	if(!grepl(MPLUS.MODEL.FILE.SUFFIX.REGEXP, filename)) filename <- paste0(filename, MPLUS.MODEL.FILE.SUFFIX)

	return(file(filename))
}

write.Mplus.bifactor.CFA.model <- function(
  file.name, data.file,
  items = ITEM.DATA, trait = BIG.FIVE.TRAITS, init.params = FALSE, estimate.corrs = FALSE,
  iteration = 1, template.file = MPLUS.BIFACTOR.CFA.TEMPLATE.FILE, specific.factors = TRUE, estimator = c("ML", "ULSMV")
) {

  trait <- match.arg(trait)
  estimator <- match.arg(estimator)
  assert_is_logical(estimate.corrs)

  trait.items <- get.trait.items(trait = trait, items, instrument = APRA2.INSTRUMENT, ordered = "as.is")

  template.file <- file(paste0(RESOURCE.PATH, template.file))
  model.template <- readLines(template.file)
  close(template.file)

  result <- set.mplus.option(model.template, MPLUS.OPTION.TRAIT, trait)
  result <- set.mplus.option(result, MPLUS.OPTION.ITERATION, iteration)
  result <- set.mplus.option(result, MPLUS.OPTION.RESPONSE.FILE, paste0(DATA.PATH, data.file))
  result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.ITEMS, rownames(trait.items))
  result <- set.mplus.option(result, MPLUS.OPTION.MISSING.VALUE, MPLUS.NA)
  result <- set.mplus.option(result, MPLUS.OPTION.ESTIMATOR, estimator)
  result <- set.mplus.option(
  	result,
  	MPLUS.OPTION.INTEGRATION,
  	if(estimator == "ML") "INTEGRATION = MONTECARLO;" else ""
  )
  result <- set.mplus.option(
  	result,
  	MPLUS.OPTION.PARAMETERIZATION,
  	if(estimator == "ULSMV") "PARAMETERIZATION = THETA;" else ""
  )
  result <- set.mplus.option(
    result, MPLUS.OPTION.DIM, get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)
  )
  
  result <- set.mplus.option(result, MPLUS.OPTION.UNIQUENESSESS, if(estimator == "ULSMV") "[ITEM_RANGE]@1;" else "")
  result <- set.mplus.option(result, MPLUS.OPTION.ITEM.RANGE, get.mplus.option.var.range(trait.items))
  
  if(specific.factors) {
  	
  	facets <- get.model.facets(trait)
  	
  	for(facet in seq_along(facets)) {
  		
  		result <- set.mplus.option(result, MPLUS.OPTION.FACET, facets[facet])
  		
  		result <- set.mplus.option(
  			result,
  			paste(MPLUS.OPTION.ITEM.RANGE, facets[facet], sep = "_"),
  			get.mplus.option.var.range(trait.items[-(1:facet), ]))
  	}
  	result <- clean.mplus.output(result)
  }
	
  result <- set.mplus.option(
  	result,
  	MPLUS.OPTION.ITEM.RANGE,
  	if(init.params) get.init.vars(items, trait, var.type = ITEM.TYPES["item"])
  	else get.mplus.option.var.range(trait.items)
  )
  result <- set.mplus.option(result, MPLUS.OPTION.FREE.LOADING, if(init.params) "" else MPLUS.FREE.LOADING.CHAR)
  
  result <- set.mplus.option(
  	result,
  	MPLUS.OPTION.TRAIT.RANGE,
  	get.mplus.option.trait.range(if(specific.factors) c(trait, get.facets(trait)) else trait)
  )
  result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.RANGE, if(estimate.corrs) "*" else "@0")
  result <- set.mplus.option(result, MPLUS.OPTION.MODINDICES, if(estimator == "ULSMV") "MODINDICES;" else "")
	
  output.file <- get.Mplus.input.file(file.name)
  on.exit(close(output.file))
  
  writeLines(result, output.file)

  invisible(NULL)
}

write.Mplus.joint.CFA.model <- function(
	items, blocks, file.name, data.file = MPLUS.RESPONSE.TEMP.FILE,
	traits = BIG.FIVE.TRAITS, specificities = "none",
	iteration = 1, template.file = MPLUS.JOINT.CFA.TEMPLATE.FILE, estimator = c("ML", "ULSMV", "MLR", "MLMV"),
	fix = "none", mc.iterations = 500, constrain.intercepts = "none", constrain.scales = TRUE,
	difftest.run = NULL, difftest.save = NULL, Wald.test = FALSE
) {
	
	traits <- match.arg(traits, several.ok = TRUE)
	estimator <- match.arg(estimator)
	
	assert_is_a_bool(constrain.scales)
	assert_is_a_string(constrain.intercepts)
	
	assert_is_numeric(mc.iterations)
	assert_all_are_whole_numbers(mc.iterations)
	assert_all_are_greater_than_or_equal_to(mc.iterations, 0)
	
	contrain.intercepts <- match.arg(constrain.intercepts, c("none", GSQ.THRESHOLDS))
	
	specificities <- match.arg(specificities, c("none", "all", rownames(items)), several.ok = TRUE)
	if(("none" %in% specificities) | ("all" %in% specificities))
		if(length(specificities) > 1)
			stop(
				"Parameter 'specificities' must be either 'none', 'all', or a character vector with any number of item parameter names"
			)
		else specificities <- NULL
	
	fix <- match.arg(fix, choices = c("none", rownames(items)), several.ok = TRUE)
	if("none" %in% fix)
		if(length(fix) > 1)
			stop("Parameter 'fix' must be either 'none' or a character vector with any number of item parameter names")
		else fix <- NULL
	if(any(!(fix %in% rownames(items)))) {
		
		warning("Some item codes in 'fix' reference unknown items and will be ignored")
		fix <- fix[fix %in% rownames(items)]
	}
	
	if(!is.null(difftest.save)) assert_is_a_string(difftest.save)
	if(!is.null(difftest.run)) assert_is_a_string(difftest.run)
	
	assert_is_a_bool(Wald.test)
	
	constraint.list <- character()
	
	template.file <- file(paste0(RESOURCE.PATH, template.file))
	model.template <- readLines(template.file)
	close(template.file)
	
	result <- set.mplus.option(model.template, MPLUS.OPTION.ITERATION, if(!is.null(iteration)) iteration else "NA")
	result <- set.mplus.option(result, MPLUS.OPTION.RESPONSE.FILE, paste0(DATA.PATH, data.file))
	result <- set.mplus.option(result, MPLUS.OPTION.LISTWISE.DELETION, if(estimator == "MLMV") "LISTWISE = ON;" else "")
	result <- set.mplus.option(result, MPLUS.OPTION.ITEM.NAMES, rownames(items))
	result <- set.mplus.option(result, MPLUS.OPTION.BLOCK.NAMES, rownames(blocks))
	result <- set.mplus.option(result, MPLUS.OPTION.MISSING.VALUE, MPLUS.NA)
	result <- set.mplus.option(result, MPLUS.OPTION.ESTIMATOR, estimator)
	result <- set.mplus.option(
		result,
		MPLUS.OPTION.INTEGRATION,
		if(estimator != "ULSMV") {
			paste0(
				"INTEGRATION = ",
				if(mc.iterations > 0) paste0("MONTECARLO(", mc.iterations, ");\n\tMCSEED = 290581;")
				else("10") # Puntos de cuadratura recomendados en el manual de Mplus (p. 472)
			)
		} else ""
	)
	result <- set.mplus.option(
		result,
		MPLUS.OPTION.DIFFTEST.RUN,
		if((!is.null(difftest.run)) & estimator == "MLMV") paste0("DIFFTEST = ", difftest.run, ";") else ""
	)
	result <- set.mplus.option(
		result, MPLUS.OPTION.PARAMETERIZATION, if(estimator == "ULSMV") "PARAMETERIZATION = THETA;" else ""
	)
	result <- set.mplus.option(
		result, MPLUS.OPTION.DIFFTEST.RUN,
		if(!is.null(difftest.run)) paste0("DIFFTEST = ", difftest.run, ";") else ""
	)
	
	for(trait in traits) {
		
		result <- set.mplus.option(
			result,
			MPLUS.OPTION.DIM,
			get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)
		)
		
		if(constrain.scales) {
			
			fixed.by.trait <- items[fix, ]
			fixed.by.trait <- fixed.by.trait[fixed.by.trait$trait %in% trait, ]
			
			if(nrow(fixed.by.trait) > 0) {
				
				returned.val <- get.blocks.with.items(fixed.by.trait, blocks, get.codes = TRUE, return.pos = TRUE)
				
				constraint.list <- c(
					constraint.list,
					if(Wald.test) {
						
						paste0(
							"TEST = ",
							get.constraints(rownames(fixed.by.trait), trait),
							ifelse(returned.val$pos == 1, " - ", " + "),
							get.constraints(returned.val$blocks, trait), ";"
						)
						
					} else {
						
						paste0(
							get.constraints(rownames(fixed.by.trait), trait),
							" = ",
							ifelse(returned.val$pos == 1, "", "-"),
							get.constraints(returned.val$blocks, trait), ";"
						)
					}
				)
				
				fixed.blocks <- returned.val$blocks
				
			} else {
				
				fixed.blocks <- fixed.by.trait <- NULL
			}
		} else {
			
			fixed.blocks <- fixed.by.trait <- NULL
		}
		
		result <- set.mplus.option(
			result,
			get.mplus.option.trait.items(trait),
			get.init.vars(blocks, trait, fixed = fixed.blocks)
		)
		
		result <- set.mplus.option(
			result,
			get.mplus.option.trait.items(trait, var.type = ITEM.TYPES["item"]),
			get.init.vars(items, trait, var.type = ITEM.TYPES["item"], fixed = rownames(fixed.by.trait))
		)
	}
	
	result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.RANGE, get.mplus.option.trait.range(traits))
	
	if(is.null(specificities))
		result <- set.mplus.option(result, MPLUS.OPTION.UNIQUENESSES.CORRS, "")
	else if(specificities == "all")
		result <- set.mplus.option(
			result,
			MPLUS.OPTION.UNIQUENESSES.CORRS, get.mplus.option.uniquenesses.corr(items, traits)
		)
	else {
		
		sel.items <- items[rownames(items) %in% specificities, ]
		
		for(item in rownames(sel.items)) {
			
			block.list <- get.blocks.with.items(items[item, ], blocks, get.codes = TRUE, return.pos = TRUE)
			block <- block.list$block
			item.pos <- block.list$pos
			
			result <- set.mplus.option(result, MPLUS.OPTION.ITEM, item)
			result <- set.mplus.option(
				result,
				get.mplus.option.block.and.item(items[item, ]),
				paste0(block, "@", ifelse(item.pos == "1", "1", "-1"), " ", item, "@1")
			)
		}
		
		result <- set.mplus.option(
			result,
			MPLUS.OPTION.UNIQUENESSES.CORRS, get.mplus.option.uniquenesses.corr(sel.items, traits)
		)
	}
	
	result <- clean.mplus.output(result)
	
	result <- set.mplus.option(
		result,
		MPLUS.OPTION.JOINT.UNIQUENESSESS,
		if(estimator == "ULSMV") MPLUS.JOINT.UNIQUENESSES.VALUE else ""
	)
	if(estimator == "ULSMV") {
		
		result <- set.mplus.option(result, MPLUS.OPTION.ITEM.RANGE, get.mplus.option.var.range(items))
		result <- set.mplus.option(result, MPLUS.OPTION.BLOCK.RANGE, get.mplus.option.var.range(blocks))
	}
	
	intercept.constraints <- if(!is.null(fix) & (constrain.intercepts != "none"))
		label.thresholds(items[fix, ], blocks, constrain.intercepts)
	else ""
	
	result <- set.mplus.option(result, MPLUS.OPTION.THRESHOLD.CONSTRAINTS, intercept.constraints)
	
	if(!is.null(fix) | !identical(intercept.constraints, "")) {
		
		constraint.value <- MPLUS.MODEL.CONSTRAINT.HEADER
		if(Wald.test) constraint.value <- c(constraint.value, "NEW(TEST);")
		constraint.value <- c(constraint.value, constraint.list)

		if(!identical(intercept.constraints, "")) {
			
			fix.blocks <- get.blocks.with.items(items[fix, ], blocks, get.codes = TRUE)
			use.blocks <- fix.blocks[duplicated(fix.blocks)]
			
			for(block in use.blocks) {
				
				constraint.labels <- get.mplus.option.threshold.labels(items, blocks[block, ], constrain.intercepts, FALSE)
				
				new.constraint <- if(Wald.test) {
					
					paste0(
						"TEST = ", constraint.labels[3], " - ", constraint.labels[1], " + ", constraint.labels[2], ";"
					)
					
				} else {
					
					new.constraint <- paste0(
						constraint.labels[3], " = ", constraint.labels[1], " - ", constraint.labels[2], ";"
					)
				}
				
				constraint.value <- c(constraint.value, new.constraint)
			}
		}
		
		constraint.value <- paste0(constraint.value, collapse = "\n\t\t\t")
		
		result <- set.mplus.option(result, MPLUS.OPTION.MODEL.CONSTRAINT, constraint.value)
	} else result <- set.mplus.option(result, MPLUS.OPTION.MODEL.CONSTRAINT, "")
	
	result <- set.mplus.option(result, MPLUS.OPTION.MODINDICES, if(estimator == "ULSMV") "MODINDICES;" else "")
	
	result <- set.mplus.option(result, MPLUS.OPTION.MODEL.TEST, if(Wald.test) "MODEL TEST:\n\t\t\tTEST = 0;" else "")
	
	result <- set.mplus.option(
		result, MPLUS.OPTION.DIFFTEST.SAVE,
		if((!is.null(difftest.save)) & estimator == "MLMV")  paste0("SAVEDATA: DIFFTEST = ", difftest.save, ";") else ""
	)
	
	output.file <- get.Mplus.input.file(file.name)
	on.exit(close(output.file))
	
	writeLines(result, output.file)
	
	invisible(NULL)
}

## TODO: incomplete (cannot write a Bifactor model with this function)
write.Mplus.bifactor.model <- function(
	file.name, data.file,
	items = ITEM.DATA, trait = BIG.FIVE.TRAITS, init.params = FALSE,
	iteration = 1, template.file = MPLUS.TEMPLATE.FILE, estimator = c("ML", "ULSMV")
) {
	
	trait <- match.arg(trait)
	estimator <- match.arg(estimator)
	
	trait.items <- get.trait.items(trait = trait, items, instrument = APRA2.INSTRUMENT, ordered = "as.is")
	
	template.file <- file(paste0(RESOURCE.PATH, template.file))
	model.template <- readLines(template.file)
	close(template.file)
	
	result <- set.mplus.option(model.template, MPLUS.OPTION.TRAIT, trait)
	result <- set.mplus.option(result, MPLUS.OPTION.ITERATION, iteration)
	result <- set.mplus.option(result, MPLUS.OPTION.RESPONSE.FILE, paste0(DATA.PATH, data.file))
	result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.ITEMS, rownames(trait.items))
	result <- set.mplus.option(result, MPLUS.OPTION.MISSING.VALUE, MPLUS.NA)
	result <- set.mplus.option(result, MPLUS.OPTION.ESTIMATOR, estimator)
	result <- set.mplus.option(
		result,
		MPLUS.OPTION.INTEGRATION,
		if(estimator == "ML") "INTEGRATION = MONTECARLO;" else ""
	)
	result <- set.mplus.option(
		result,
		MPLUS.OPTION.PARAMETERIZATION,
		if(estimator == "ULSMV") "PARAMETERIZATION = THETA;" else ""
	)
	result <- set.mplus.option(
		result, MPLUS.OPTION.DIM, get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)
	)
	
	for(facet in get.model.facets(trait)) result <- set.mplus.option(result, MPLUS.OPTION.FACET, facet)
	result <- clean.mplus.output(result)
	
	result <- set.mplus.option(result, MPLUS.OPTION.UNIQUENESSESS, if(estimator == "ULSMV") "[ITEM_RANGE]@1;" else "")

	result <- set.mplus.option(result, MPLUS.OPTION.ITEM.RANGE, get.mplus.option.var.range(trait.items))## TODO: init params
	
	on.exit(close(output.file))
	
	output.file <- file(
		paste0(
			RESOURCE.PATH,
			file.name,
			if(!grepl(MPLUS.MODEL.FILE.SUFFIX.REGEXP, file.name)) MPLUS.MODEL.FILE.SUFFIX
		)
	)
	
	writeLines(result, output.file)
	
	invisible(NULL)
}

get.mplus.option.trait.items <- function(trait, var.type = ITEM.TYPES["block"]) {
	
	var.type <- match.arg(var.type, ITEM.TYPES)
	
  model.abbr <- get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)

  result <- paste0(model.abbr, if(var.type == "block") MPLUS.OPTION.SUFFIX.BLOCKS else MPLUS.OPTION.SUFFIX.ITEMS)
  
  return(result)
}

get.mplus.option.block.items <- function(block, include.block = FALSE) {
	
	result <- paste0(
		rownames(block),
		if(include.block) MPLUS.OPTION.SUFFIX.BLOCK.ITEM.PAIR else MPLUS.OPTION.SUFFIX.ITEM.PAIR
	)
	
	return(result)
}

get.mplus.option.block.and.item <- function(item) {
	
	result <- paste0(rownames(item), MPLUS.OPTION.SUFFIX.BLOCK.ITEM)
	
	return(result)
}

get.init.vars <- function(variables, trait, var.type = ITEM.TYPES["block"], end.line = FALSE, fixed = NULL) {
	
	var.type <- match.arg(var.type, ITEM.TYPES)
	
	if(var.type == ITEM.TYPES["block"]) {
		
		sel.vars <- variables[variables$trait.item.1 == trait, ]
		sel.var.names <- rownames(sel.vars)
		
		result <- paste0(
			sel.var.names, MPLUS.INITIALIZE.AT,
			ifelse(
				sel.vars$polarity.item.1 == POLARITY["+"],
				MPLUS.LOADING.INIT.POSITIVE, MPLUS.LOADING.INIT.NEGATIVE
			),
			ifelse(
				sel.var.names %in% fixed,
				paste0(" (", get.constraints(sel.var.names, trait), ")"), ""
			),
			collapse = MPLUS.BLOCKS.LINE.FEED
		)
		result <- paste0(MPLUS.BLOCKS.LINE.FEED, result)
		
		sel.vars <- variables[variables$trait.item.2 == trait, ]
		sel.var.names <- rownames(sel.vars)
		
		result.2 <- paste0(
			sel.var.names, MPLUS.INITIALIZE.AT,
			ifelse(
				sel.vars$polarity.item.2 == POLARITY["-"],
				MPLUS.LOADING.INIT.POSITIVE, MPLUS.LOADING.INIT.NEGATIVE
			),
			ifelse(
				sel.var.names %in% fixed,
				paste0(" (", get.constraints(sel.var.names, trait), ")"), ""
			),
			collapse = MPLUS.BLOCKS.LINE.FEED
		)
		
		result <- paste(result, result.2, sep = MPLUS.BLOCKS.LINE.FEED)
	} else {
		
		sel.vars <- variables[variables$trait == trait, ]
		sel.var.names <- rownames(sel.vars)
		
		result <- paste0(
			sel.var.names, MPLUS.INITIALIZE.AT,
			ifelse(
				sel.vars$polarity == POLARITY["+"],
				MPLUS.LOADING.INIT.POSITIVE, MPLUS.LOADING.INIT.NEGATIVE
			),
			ifelse(
				sel.var.names %in% fixed,
				paste0(" (", get.constraints(sel.var.names, trait), ")"), ""
			),
			collapse = MPLUS.BLOCKS.LINE.FEED
		)
		result <- paste0(MPLUS.BLOCKS.LINE.FEED, result)
	}
	
	if(end.line) result <- paste0(result, MPLUS.NEW.LINE)
	
  return(result)
}

get.constraints <- function(variables, trait) {
	
	return(paste0(variables, get.model.name.from.trait(trait, abbreviation = TRUE)))
}

get.mplus.option.var.range <- function(variables, suffix = "") {

  var.names <- rownames(variables)

  result <- paste0(var.names[1], suffix, "-", var.names[length(var.names)], suffix)

  return(result)
}

get.mplus.option.trait.range <- function(traits) {

  traits <- get.model.name.from.trait(traits, abbreviation = TRUE, upper.case = FALSE)
  result <- if(length(traits) == 1) traits else paste(traits[1], traits[length(traits)], sep = "-")

  return(result)
}

get.mplus.option.uniquenesses.corr <- function(items, traits) {
	
	traits <- get.model.name.from.trait(traits, abbreviation = TRUE, upper.case = FALSE)
	
	trait.range <- if(length(traits) == 1) traits else paste(traits[1], traits[length(traits)], sep = "-")
	
	item.range <- get.mplus.option.var.range(items, "_S")
	
	result <- paste0(paste(item.range, "WITH", paste0(item.range, "@0"), trait.range), "@0;")
	
	return(result)
}

get.mplus.option.threshold.labels <- function(items, blocks, threshold.level, with.parentheses = TRUE) {
	
	fix.blocks <- get.blocks.with.items(items, blocks, get.codes = TRUE)
	use.blocks <- fix.blocks[duplicated(fix.blocks)]
	item.names <- get.items.in.blocks(items, blocks[use.blocks, ], get.codes = TRUE)
	items <- item.names[get.blocks.with.items(items[item.names, ], blocks, return.pos = TRUE)$pos]
	
	result <- if(length(use.blocks) > 0)
		c(
			paste0(if(with.parentheses) "(", "T", items, if(with.parentheses) ")"),
			paste0(if(with.parentheses) "(", "T", use.blocks, if(with.parentheses) ")")
		)
	else ""
	
	return(result)
}

get.mplus.option.thresholds <- function(items, blocks, threshold.level) {
	
	fix.blocks <- get.blocks.with.items(items, blocks, get.codes = TRUE)
	use.blocks <- fix.blocks[duplicated(fix.blocks)]
	items <- get.items.in.blocks(items, blocks[use.blocks, ], get.codes = TRUE)
	
	result <- if(length(use.blocks) > 0)
		c(
			paste0("[", items, "$", threshold.level, "*]"),
			paste0("[", use.blocks, "$1*]")
		)
	else ""
	
	return(result)
}

label.thresholds <- function(items, blocks, threshold.level) {
	
	result <- paste0(
		get.mplus.option.thresholds(items, blocks, threshold.level), " ",
		get.mplus.option.threshold.labels(items, blocks, threshold.level), ";"
	)
	if(identical(result, " ;")) result <- ""
	
	return(result)
}

write.Mplus.CFA.model <- function(
	file.name, data.file, items,
	iteration = 1, traits = BIG.FIVE.TRAITS, init.params = TRUE,
	estimate.corrs = TRUE, estimator = c("ULSMV", "ML"), template.file = MPLUS.TIRT.TEMPLATE.FILE
) {
	
	traits <- match.arg(traits, several.ok = TRUE)
	
	template.file <- file(paste0(RESOURCE.PATH, template.file))
	model.template <- readLines(template.file)
	close(template.file)
	
	result <- if(!is.null(iteration))
		set.mplus.option(model.template, MPLUS.OPTION.ITERATION, iteration) else model.template
	result <- set.mplus.option(result, MPLUS.OPTION.RESPONSE.FILE, paste0(DATA.PATH, data.file))
	result <- set.mplus.option(result, MPLUS.OPTION.ITEM.NAMES, rownames(blocks))
	result <- set.mplus.option(result, MPLUS.OPTION.MISSING.VALUE, MPLUS.NA)
	
	for(trait in traits) {
		
		result <- set.mplus.option(
			result,
			MPLUS.OPTION.DIM,
			get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)
		)
		
		result <- set.mplus.option(
			result,
			get.mplus.option.trait.items(trait),
			get.init.vars(items, trait)
		)
	}
	
	result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.RANGE, get.mplus.option.trait.range(traits))
	result <- set.mplus.option(result, MPLUS.OPTION.ITEM.RANGE, get.mplus.option.var.range(blocks))
	result <- clean.mplus.output(result)
	
	
	on.exit(close(output.file))
	
	output.file <- get.Mplus.input.file(file.name)
	
	writeLines(result, output.file)
	
	invisible(NULL)
}

write.Mplus.FCQ.model <- function(
  file.name, data.file, blocks, iteration = 1,
  traits = BIG.FIVE.TRAITS, template.file = MPLUS.TIRT.TEMPLATE.FILE
) {

  traits <- match.arg(traits, several.ok = TRUE)

  template.file <- file(paste0(RESOURCE.PATH, template.file))
  model.template <- readLines(template.file)
  close(template.file)

  result <- if(!is.null(iteration))
  	set.mplus.option(model.template, MPLUS.OPTION.ITERATION, iteration) else model.template
  result <- set.mplus.option(result, MPLUS.OPTION.RESPONSE.FILE, paste0(DATA.PATH, data.file))
  result <- set.mplus.option(result, MPLUS.OPTION.ITEM.NAMES, rownames(blocks))

  blocks <- get.trait.blocks(blocks, traits, discard.traits = TRUE)
  result <- set.mplus.option(
    result,
    MPLUS.OPTION.SELECTED.ITEMS,
    if(all(BIG.FIVE.TRAITS %in% traits)) MPLUS.ALL.VARIABLES
    else rownames(get.trait.blocks(blocks, traits, discard.traits = TRUE))
  )
  result <- set.mplus.option(result, MPLUS.OPTION.MISSING.VALUE, MPLUS.NA)

  for(trait in traits) {

    result <- set.mplus.option(
      result,
      MPLUS.OPTION.DIM,
      get.model.name.from.trait(trait, abbreviation = TRUE, upper.case = FALSE)
    )

    result <- set.mplus.option(
      result,
      get.mplus.option.trait.items(trait),
      get.init.vars(blocks, trait)
    )
  }

  result <- set.mplus.option(result, MPLUS.OPTION.TRAIT.RANGE, get.mplus.option.trait.range(traits))
  result <- set.mplus.option(result, MPLUS.OPTION.ITEM.RANGE, get.mplus.option.var.range(blocks))
  result <- clean.mplus.output(result)


  on.exit(close(output.file))

  output.file <- get.Mplus.input.file(file.name)

  writeLines(result, output.file)

  invisible(NULL)
}

plot.grm <- function(discrimination, thresholds, thetas) {
	
	probs <- grm(discrimination, thresholds, thetas)
	
	margins <- par("mar")
	light.blue <- rgb(18, 178, 235, maxColorValue = 255)
	par(mar = margins + c(0, 1.2, -4, 0), cex = 1.2, cex.lab = 1.3)
	
	plot(
		thetas, probs[, 1], type = "l", col = light.blue,
		ylim = c(0, 1),
		xlab = expression("Nivel de rasgo latente (" ~ theta ~ ")"),
		ylab = expression(P(italic(x[i[p]][j]) == italic(k[i[p]]))),
		mar = margins
	)
		
	for(category in 2:(length(thresholds) + 1))
		lines(thetas, probs[, category], col = light.blue)
	par(mar = margins)
	
	invisible(NULL)
}
