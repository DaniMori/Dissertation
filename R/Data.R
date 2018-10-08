library(foreign)
library(XLConnect)


tryCatch(
  memory.limit(2^44-1),
  error = function(e) memory.limit(2^12-1)
)



### CONSTANTS:

## FILE STRUCTURE PATHS:
DATA.PATH <- "res/"
LIB.PATH <- "R/"
OUTPUT.PATH <- "res/"


APPLICATIONS <- c("APRA", "APRA2")
names(APPLICATIONS) <- APPLICATIONS

APRA2.INSTRUMENT <- "APRA2"
APRA2.RESPONSE.CATEGORIES <- c("A", "B", "C", "D", "E")
APRA2.BLOCK.RESPONSE.CATEGORIES <- c("A", "B")
APRA2.CONTROL.INSTRUMENT <- "CONTROL_APRA2"


## APRA FILE NAMES:
ITEM.FILE.NAME <- paste0(DATA.PATH, "Administracion - Banco items.xls")
DATA.FILE.NAME <- paste0(DATA.PATH, "datos_826.sav")
ITEM.SHEET <- "Banco"

## APRA2 RELEVANT CONSTANTS:
ITEM.TYPES <- c("item", "block")
names(ITEM.TYPES) <- ITEM.TYPES

POLARITY <- c("+", "-")
names(POLARITY) <- POLARITY

CONTROL.BLOCK.ID <- "CONTROL"
CONTROL.BLOCK.TRAIT <- "Control"
CONTROL.ITEM.CODES <- factor(c("D", "A", "E", "B", "C", "D"))
CONTROL.BLOCK.CODES <- factor(c("B", "A", "B"))


BIG.FIVE.TRAITS <- c("Neuroticismo", "Extraversion", "Apertura", "Amabilidad", "Responsabilidad")
names(BIG.FIVE.TRAITS) <- BIG.FIVE.TRAITS

TRAIT.LEVELS <- c(BIG.FIVE.TRAITS, Control = CONTROL.BLOCK.TRAIT)

BIG.FIVE.FACETS <- matrix(
  c(
    "Ansiedad",     "Hostilidad", "Depresion",      "Ansiedad_social",      "Impulsividad",       "Vulnerabilidad",
    "Cordialidad",  "Gregarismo", "Asertividad",    "Actividad",            "Busqueda_emociones", "Emociones_positivas",
    "Fantasia",     "Estetica",   "Sentimientos",   "Acciones",             "Ideas",              "Valores",
    "Confianza",    "Franqueza",  "Altruismo",      "Actitud_conciliadora", "Modestia",           "Sensibilidad_demas",
    "Competencia",  "Orden",      "Sentido_deber",  "Necesidad_logro",      "Autodisciplina",     "Deliberacion"
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(BIG.FIVE.TRAITS, NULL)
)

APRA2.FACETS <- matrix(
  c(
    "ANSIEDAD",     "HOSTILIDAD", "DEPRESION",      "ANSD.SOCIAL",    "IMPULSIVIDAD",   "VULNERABILIDAD",
    "CORDIALIDAD",  "GREGARISMO", "ASERTIVIDAD",    "ACTIVIDAD",      "BUSQ.EMOC.",     "EMOC.POSITV.",
    "FANTASIA",     "ESTETICA",   "SENTIMIENTOS",   "ACCIONES",       "IDEAS",          "VALORES",
    "CONFIANZA",    "FRANQUEZA",  "ALTRUISMO",      "ACT.CONCILIA.",  "MODESTIA",       "SENSIBILIDAD",
    "COMPETENCIA",  "ORDEN",      "SENT.DEBER",     "NECES.LOGRO",    "AUTODISCIPLINA", "DELIBERACION"
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(BIG.FIVE.TRAITS, NULL)
)


ITEM.FILE.NAME.APRA2 <- paste0(DATA.PATH, "booklets_APRA2016.xlsx")
ITEM.SHEETS.APRA2 <- paste("Cuadernillo", c(1:5, 7:8))
names(ITEM.SHEETS.APRA2) <- ITEM.SHEETS.APRA2

ITEM.FORMS.APRA2 <- c("A", "B")
names(ITEM.FORMS.APRA2) <- ITEM.SHEETS.APRA2[c("Cuadernillo 4", "Cuadernillo 5")]

DATA.FILE.NAME.APRA2 <- paste0(DATA.PATH, "BBDD_APRA2016.csv")
NON.RESPONSE.HEADERS <- c("MD5_code", "CURSO", "GRUPO")

GSQ.THRESHOLDS <- as.character(1:4)
names(GSQ.THRESHOLDS) <- GSQ.THRESHOLDS
FCQ.THRESHOLDS <- as.character(1)
names(FCQ.THRESHOLDS) <- FCQ.THRESHOLDS



### FUNCTIONS

read.items <- function(reset = FALSE, application = APPLICATIONS) {
  
  application <- match.arg(application, several.ok = TRUE)
  
  if(APPLICATIONS["APRA"] %in% application) {
    
    if(is.null(ITEM.DATA) | reset) {
      
      item.file <- loadWorkbook(ITEM.FILE.NAME)
      item.data <- readWorksheet(item.file, ITEM.SHEET)
      
      instrument <- get.instrument(item.data$Fac_Cod)
      
      facet <- get.facet(item.data)
      trait <- get.trait(item.data)
      
      ITEM.DATA <<- data.frame(
        code = item.data$Cod_Paco,
        instrument = instrument,
        trait = trait,
        facet = facet,
        facet.num = item.data$Item,
        polarity = as.factor(item.data$Sentido),
        position = item.data$Pos_Forma,
        form = item.data$Forma,
        stem = item.data$Enunciado,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if(APPLICATIONS["APRA2"] %in% application) {
    
    warning("Functionality for importing APRA2 items not implemented yet")
    # if(is.null(ITEM.DATA.APRA2) | reset) {
    #   
    #   item.file <- loadWorkbook(ITEM.FILE.NAME.APRA2)
    #   
    #   item.data <- list()
    #   for(sheet in ITEM.SHEETS.APRA2) {
    #     
    #     item.data <- append(item.data, list(readWorksheet(item.file, sheet)))
    #   }
    #   names(item.data) <- ITEM.SHEETS.APRA2
    #   
    #   instrument <- get.instrument(item.data$Fac_Cod)
    #   
    #   facet <- get.facet(item.data)
    #   trait <- get.trait(item.data)
    #   
    #   ITEM.DATA <<- data.frame(
    #     code = item.data$Cod_Paco,
    #     instrument = instrument,
    #     trait = trait,
    #     facet = facet,
    #     facet.num = item.data$Item,
    #     polarity = as.factor(item.data$Sentido),
    #     position = item.data$Pos_Forma,
    #     form = item.data$Forma,
    #     stem = item.data$Enunciado,
    #     stringsAsFactors = FALSE
    #   )
    # }
  }
  
  return(NULL)
}

# TODO: use function read.items() adapted properly
read.APRA2.new.items <- function(type = ITEM.TYPES) {
  
  type = match.arg(type, several.ok = TRUE)
  
  item.file <- loadWorkbook(ITEM.FILE.NAME.APRA2)
  sheets <- ITEM.SHEETS.APRA2[
    c(
      if(ITEM.TYPES["block"] %in% type)  "Cuadernillo 3",
      if(ITEM.TYPES["item"] %in% type)   c("Cuadernillo 4", "Cuadernillo 5")
    )
  ]
  
  item.data <- list()
  for(sheet in sheets) {
    
    item.data <- append(item.data, list(readWorksheet(item.file, sheet)))
  }
  names(item.data) <- sheets
  
  facets <- BIG.FIVE.FACETS
  names(facets) <- APRA2.FACETS
  
  if(ITEM.TYPES["item"] %in% type) {
    
    items <- data.frame(
      type = factor(ITEM.TYPES["item"], levels = ITEM.TYPES),
      sheet = factor(ITEM.SHEETS.APRA2["Cuadernillo 4"]),
      code = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Cod_APRA,
      instrument = factor(
        ifelse(
          item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Rasgo == CONTROL.BLOCK.ID,
          APRA2.CONTROL.INSTRUMENT,
          APRA2.INSTRUMENT
        )
      ),
      local.code = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Item,
      trait = factor(
        ifelse(
          item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Rasgo == CONTROL.BLOCK.ID,
          CONTROL.BLOCK.TRAIT,
          item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Rasgo
        ),
        levels = c(BIG.FIVE.TRAITS, CONTROL.BLOCK.TRAIT)
      ),
      facet = factor(facets[item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Faceta]),
      facet.num = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Pos_faceta,
      polarity = factor(
        ifelse(
          item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Polaridad == "DIRECTO", POLARITY["+"],
          ifelse(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Polaridad == "INVERSO", POLARITY["-"], NA)
        ),
        levels = POLARITY
      ),
      position = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 4"]]]$Pos_cuadernillo,
      form = factor(ITEM.FORMS.APRA2["Cuadernillo 4"]),
      stem = item.data$`Cuadernillo 4`$Enunciado,
      stringsAsFactors = FALSE, row.names = "local.code"
    )
    items <- rbind(
      items,
      data.frame(
        type = factor(ITEM.TYPES["item"], levels = ITEM.TYPES),
        sheet = factor(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Cuadernillo),
        code = item.data$`Cuadernillo 5`$Cod_APRA,
        instrument = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Rasgo == CONTROL.BLOCK.ID,
            APRA2.CONTROL.INSTRUMENT,
            APRA2.INSTRUMENT
          )
        ),
        local.code = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Item,
        trait = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Rasgo == CONTROL.BLOCK.ID,
            CONTROL.BLOCK.TRAIT,
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Rasgo
          ),
          levels = c(BIG.FIVE.TRAITS, CONTROL.BLOCK.TRAIT)
        ),
        facet = factor(facets[item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Faceta]),
        facet.num = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Pos_faceta,
        polarity = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Polaridad == "DIRECTO", POLARITY["+"],
            ifelse(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Polaridad == "INVERSO", POLARITY["-"], NA)
          ),
          levels = POLARITY
        ),
        position = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Pos_cuadernillo,
        form = factor(ITEM.FORMS.APRA2["Cuadernillo 5"]),
        stem = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 5"]]]$Enunciado,
        stringsAsFactors = FALSE, row.names = "local.code"
      )
    )
  }
  
  if(ITEM.TYPES["block"] %in% type) {
    
    suppressWarnings(
      blocks <- data.frame(
        position = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Posicion,
        sheet = factor(3),
        instrument = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Pos_original == CONTROL.BLOCK.ID,
            APRA2.CONTROL.INSTRUMENT,
            APRA2.INSTRUMENT
          )
        ),
        local.code = paste0("P3_", item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Posicion),
        original.pos = as.numeric(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Pos_original),
        type = factor(ITEM.TYPES["block"], levels = ITEM.TYPES),
        code.item.1 = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$code_1,
        trait.item.1 = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Pos_original == CONTROL.BLOCK.ID,
            CONTROL.BLOCK.TRAIT, item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$trait_1
          ),
          levels = c(BIG.FIVE.TRAITS, CONTROL.BLOCK.TRAIT)
        ),
        polarity.item.1 = factor(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$polarity_1, levels = POLARITY),
        stem.item.1 = as.character(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$stem_1),
        code.item.2 = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$code_2,
        trait.item.2 = factor(
          ifelse(
            item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$Pos_original == CONTROL.BLOCK.ID,
            CONTROL.BLOCK.TRAIT, item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$trait_2
          ),
          levels = c(BIG.FIVE.TRAITS, CONTROL.BLOCK.TRAIT)
        ),
        polarity.item.2 = factor(item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$polarity_2, levels = POLARITY),
        stem.item.2 = item.data[[ITEM.SHEETS.APRA2["Cuadernillo 3"]]]$stem_2,
        stringsAsFactors = FALSE, row.names = "local.code"
      )
    )
  }
  
  result <- if(identical(type, ITEM.TYPES)) list(items = items, blocks = blocks)
            else if(identical(type, ITEM.TYPES["item"])) items
            else if(identical(type, ITEM.TYPES["block"])) blocks
            else stop("Unknown item type")
  
  return(result)
}


get.instrument <- function(facet.code) {
  
  as.factor(ifelse(facet.code < 60, "APRA", ifelse(facet.code < 70, "NEO", ifelse(facet.code < 80, "PDS", "CONTROL"))))
}


get.facet <- function(items) {
  
  instrument <- get.instrument(items$Fac_Cod)
  
  as.factor(ifelse(instrument == "APRA", items$Fac_Nombre, NA))
}


get.trait <- function(items) {
  
  instrument <- get.instrument(items$Fac_Cod)
  
  as.factor(ifelse(instrument == "APRA", BIG.FIVE.TRAITS[items$Fac_Cod %/% 10], items$Fac_Nombre))
}

get.instrument.items <- function(
  items = ITEM.DATA,
  instrument = c("APRA", "NEO", APRA2.INSTRUMENT, APRA2.CONTROL.INSTRUMENT), strings.as.factors = TRUE
) {
  
  instrument <- match.arg(instrument, several.ok = TRUE)
  
  items <- items[items$instrument %in% instrument, ]
  
  # Call "factor" on all the factors, to drop the empty factor levels
  item.names <- rownames(items)
  items <- as.data.frame(
  	lapply(items, function(prop) if(is.factor(prop)) factor(prop) else prop),
  	stringsAsFactors = strings.as.factors
  )
  rownames(items) <- item.names
  
  return(items)
}

get.items.by.code <- function(items, codes, control = FALSE) {
	
	if(control)
		codes <- c(codes, as.character(items$code[items$instrument == APRA2.CONTROL.INSTRUMENT]))
	
	items <- items[as.character(items$code) %in% codes, ]
	
	return(items)
}

get.items.in.blocks <- function(items, blocks, control = TRUE, get.codes = FALSE) {
	
	items.in.FCQ <- with(blocks, c(as.character(code.item.1), as.character(code.item.2)))
	
	result <- get.items.by.code(items, items.in.FCQ, control)
	
	if(get.codes) result <- rownames(result)
	
	return(result)
}

get.blocks.with.items <- function(items, blocks, get.codes = FALSE, return.pos = FALSE) {
	
	item.codes <- as.character(items$code)
	
	sel.blocks <- c(
		rownames(blocks[blocks$code.item.1 %in% item.codes, ]),
		rownames(blocks[blocks$code.item.2 %in% item.codes, ])
	)
	
	blocks.res <- if(get.codes) sel.blocks else blocks[sel.blocks, ]
	
	result <- if(return.pos) {
		
		item.codes <- as.character(get.items.in.blocks(items, blocks[unique(sel.blocks), ])$code)
		
		positions <- numeric(length(item.codes))
		names(positions) <- item.codes
		positions <- ifelse(item.codes %in% as.character(blocks$code.item.1), 1, 2)
		
		list(blocks = blocks.res, pos = as.factor(positions))
	} else blocks.res
	
	return(result)
}

filter.APRA.items.blocks <- function(blocks) {
	
	result <- blocks[-grep("*_SI1", blocks$code.item.2), ]
	
	return(result)
}

get.questionnaire.form <- function(form = c("A", "B", "C", "D")) {
  
  form <- match.arg(form)
  
  item.form <- ITEM.DATA[ITEM.DATA$form == form, ]
  return(item.form[item.form$position, ])
}


get.facet.items <- function(facet, items = ITEM.DATA, polarity = c("both", "+", "-")) {
  
  polarity <- match.arg(polarity)
  
  facet.items <- items$facet == facet
  
  if(polarity != "both") items <- items[items$polarity == polarity, ]
  
  return(items[facet.items & !is.na(facet.items), ])
}


## TODO: other possible orderings
get.trait.items <- function(
  trait = TRAIT.LEVELS,
  items = ITEM.DATA,
  instrument = c("APRA", "NEO", APRA2.INSTRUMENT, APRA2.CONTROL.INSTRUMENT),
  polarity = c("both", "+", "-"),
  ordered = c("as.is", "facet"),
  type = ITEM.TYPES,
  return.blocks = c("both", "item.1", "item.2")
) {
  
  trait <- match.arg(trait, several.ok = TRUE)
  instrument <- match.arg(instrument, several.ok = TRUE)
  polarity <- match.arg(polarity)
  ordered <- match.arg(ordered)
  type <- match.arg(type)
  return.blocks <- match.arg(return.blocks)
  
  items <- items[items$instrument %in% instrument, ]
  
  if(polarity != "both") items <- items[items$polarity == polarity, ]
  
  if(ordered == "facet") items <- items[order(items$facet), ]
  
  items <-  if(type == ITEM.TYPES["item"]) items[items$trait %in% trait, ]
            else {
              if(return.blocks == "both") items[items$trait.item.1 %in% trait | items$trait.item.2 %in% trait, ]
              else if(return.blocks == "item.1") items[items$trait.item.1 %in% trait, ]
              else items[items$trait.item.2 %in% trait, ]
            }
  
  return(items)
}

get.trait.blocks <- function(
  blocks,
  trait = TRAIT.LEVELS, in.pos = c("both", "first", "second"),
  polarity = c("all", "homopolar", "direct.inverse", "inverse.direct"),
  ordered = c("as.is", "by.trait", "by.trait2", "by.pol", "by.trait.pol", "by.pol.trait"),
  discard.traits = FALSE
) {
  
  trait <- match.arg(trait, several.ok = TRUE)
  in.pos <- match.arg(in.pos)
  polarity <- match.arg(polarity)
  ordered <- match.arg(ordered)
  
  if(discard.traits) {
    
    trait <- BIG.FIVE.TRAITS[!(BIG.FIVE.TRAITS %in% trait)]
    
    result <- switch(
      in.pos,
      first   = blocks[!(blocks$trait.item.1 %in% trait), ],
      second  = blocks[!(blocks$trait.item.2 %in% trait), ],
      both    = blocks[!(blocks$trait.item.1 %in% trait) & !(blocks$trait.item.2 %in% trait), ]
    )
  } else {
    
    result <- switch(
      in.pos,
      first   = blocks[blocks$trait.item.1 %in% trait, ],
      second  = blocks[blocks$trait.item.2 %in% trait, ],
      both    = blocks[(blocks$trait.item.1 %in% trait) | (blocks$trait.item.2 %in% trait), ]
    )
  }
  
  result <- switch(
    polarity,
    all = result,
    homopolar       = result[result$polarity.item.1 == result$polarity.item.2, ],
    direct.inverse  = result[result$polarity.item.1 == POLARITY["+"] & result$polarity.item.2 == POLARITY["-"], ],
    inverse.direct  = result[result$polarity.item.1 == POLARITY["-"] & result$polarity.item.2 == POLARITY["+"], ]
  )
  result <- switch(
    ordered,
    as.is         = result,
    by.trait      = result[order(result$trait.item.1, result$trait.item.2), ],
    by.trait      = result[order(result$trait.item.2, result$trait.item.1), ],
    by.pol        = result[order(result$polarity.item.1, result$polarity.item.2), ],
    by.trait.pol  = result[
      order(result$trait.item.1, result$trait.item.2, result$polarity.item.1, result$polarity.item.2), 
    ],
    by.pol.trait  = result[
      order(result$polarity.item.1, result$polarity.item.2, result$trait.item.1, result$trait.item.2), 
    ]
  )
  
  return(result)
}

get.control.item.codes <- function(items = ITEM.DATA, instrument = APRA2.CONTROL.INSTRUMENT, type = ITEM.TYPES) {
  
  type <- match.arg(type)
  
  control.items <- get.trait.items(CONTROL.BLOCK.TRAIT, items, instrument, type = type)
  
  control.codes <- if(type == ITEM.TYPES["item"]) CONTROL.ITEM.CODES else CONTROL.BLOCK.CODES
  names(control.codes) <- rownames(control.items)
  
  return(control.codes)
}

get.control.item.matches <- function(codes, responses, discard.na = TRUE) {
  
  matches <- apply(responses, 1, "==", codes[colnames(responses)])
  
  matches[is.na(matches)] <- if(discard.na) FALSE else TRUE
  
  return(t(matches))
}

get.n.control.item.matches <- function(codes, responses, mismatches = FALSE, discard.na = TRUE) {
  
  matches <- get.control.item.matches(codes, responses, discard.na = discard.na)
  
  n.matches <- rowSums(matches)
  
  result <- if(mismatches) length(codes) - n.matches else n.matches
  
  return(result)
}


select.items.by.trait <- function(items, trait = BIG.FIVE.TRAITS) {
  
  trait <- match.arg(trait)
  
  result <- get.trait.items(trait, items)
  if(nrow(result) != nrow(items)) warning("Items not corresponding to trait ", trait, " dropped.")
  
  return(result)
}


read.responses <- function(item.codes) {
  
  if(is.null(RESPONSE.DATA)) {
    tryCatch(
      {
        suppressWarnings(responses <- as.data.frame(read.spss(DATA.FILE.NAME)))
        
        responses[responses == -1] <- NA
        
        RESPONSE.DATA <<- responses[item.codes]
      },
      error = function(e) {
        warning(geterrmessage())
        warning(DATA.FILE.NAME, "not loaded.")
      }
    )
  }
  
  return(NULL)
}

## TODO: function header (explain "non.responses")
read.APRA2.responses <- function(variables = NULL, item.file = DATA.FILE.NAME.APRA2, non.responses = TRUE) {
  
  
  responses <- read.csv2(item.file, stringsAsFactors = TRUE, na.strings = "", row.names = 1)
  
  responses <- responses[names(responses) != "OMISIONES"] # Drop column "OMISIONES"
  responses$MD5_code <- as.character(responses$MD5_code)
  
  if(!is.null(variables)) {
    
    if(is.data.frame(variables)) variables <- rownames(variables)
    
    reponse.names <- names(responses)
    response.vars <- reponse.names[!(reponse.names %in% NON.RESPONSE.HEADERS)]
    drop.vars <- response.vars[!(response.vars %in% variables)]
    responses <- responses[!(reponse.names %in% drop.vars)]
  }
  
  if(!isTRUE(non.responses)) {
    
    if(identical(non.responses, FALSE)) {
      
      responses <- responses[!(names(responses) %in% NON.RESPONSE.HEADERS)]
    } else {
      
      non.responses <- match.arg(non.responses, NON.RESPONSE.HEADERS, several.ok = TRUE)
      drop.vars <- NON.RESPONSE.HEADERS[!(NON.RESPONSE.HEADERS %in% non.responses)]
      
      responses <- responses[!(names(responses) %in% drop.vars)]
    }
  }
  
  return(responses)
}


get.facet.responses <- function(facet, items = ITEM.DATA, polarity = c("both", "+", "-")) {
  
  polarity <- match.arg(polarity)
  
  items <- get.facet.items(facet, items)
  
  if(polarity != "both") items <- items[items$polarity == polarity, ]
  
  return(RESPONSE.DATA[items$code])
}


get.trait.responses <- function(
  responses = RESPONSE.DATA,
  trait = TRAIT.LEVELS,
  items = ITEM.DATA,
  instrument = c("APRA", "NEO", APRA2.INSTRUMENT, APRA2.CONTROL.INSTRUMENT),
  polarity = c("both", "+", "-")
) {
  
  trait <- match.arg(trait)
  items <- get.trait.items(trait, items, instrument, polarity)
  instrument <- match.arg(instrument)
  
  item.codes <- if(instrument %in% c("APRA", "NEO")) items$code else rownames(items)
  
  return(responses[, item.codes])
}

get.instrument.responses <- function(
  responses = RESPONSE.DATA,
  items = ITEM.DATA,
  instrument = c("APRA", "NEO", APRA2.INSTRUMENT, APRA2.CONTROL.INSTRUMENT)
) {
  
  instrument <- match.arg(instrument)
  
  items <- get.instrument.items(items, instrument)
  item.codes <- if(instrument %in% c("APRA", "NEO")) items$code else rownames(items)
  
  return(responses[, item.codes])
}

get.facets <- function(traits, ordered = FALSE) {
  
  result <- BIG.FIVE.FACETS[rownames(BIG.FIVE.FACETS) %in% traits]
  if(ordered) result <- result[order(result)]
  return(result)
}


get.n.omissions <- function(responses = ITEM.DATA) {
  
  result <- rowSums(is.na(responses))
  names(result) <- rownames(responses)
  
  return(result)
}

filter.by.omissions <- function(cutoff, responses = ITEM.DATA) {
  
  omissions <- get.n.omissions(responses)
  
  responses <- responses[omissions < cutoff, ]
  
  return(responses)
}

discard.wrong.responses <- function(responses, type = ITEM.TYPES) {
  
  type = match.arg(type)
  
  response.categories <- if(type == ITEM.TYPES["item"]) APRA2.RESPONSE.CATEGORIES else APRA2.BLOCK.RESPONSE.CATEGORIES
  
  result <- sapply(
    responses,
    function(variable) {
      
      variable[!(variable %in% response.categories)] <- NA
      return(variable)
    }
  )
  result <- as.data.frame(result)
  
  return(result)
}

filter.by.control.items <- function(
  item.responses, items, tolerance = 0, type = ITEM.TYPES, instrument = APRA2.CONTROL.INSTRUMENT, discard.na = TRUE
) {
  
  type <- match.arg(type)
  
  control.responses <- if(type == ITEM.TYPES["item"]) {
    
    get.trait.responses(item.responses, CONTROL.BLOCK.TRAIT, items, instrument)
  } else {
    
    get.control.block.responses(item.responses, items, instrument)
  }
  
  control.codes <- get.control.item.codes(items, instrument, type)
  
  control.matches <- get.n.control.item.matches(
  	control.codes, control.responses, mismatches = TRUE, discard.na = discard.na
  )
  
  result <- item.responses[control.matches <= tolerance, ]
  
  return(result)
}

get.control.block.responses <- function(responses, items, instrument = APRA2.CONTROL.INSTRUMENT) {
  
  instrument <- match.arg(instrument)
  
  items <- get.instrument.items(items, instrument)
  
  item.codes <- if(instrument %in% c("APRA", "NEO")) items$code else rownames(items)
  
  return(responses[, item.codes])
}

format.as.numeric <- function(responses, response.categories = APRA2.RESPONSE.CATEGORIES, TIRT.format = FALSE) {
  
  result <- sapply(responses, as.numeric, levels = response.categories, USE.NAMES = TRUE)
  result <- as.data.frame(result, row.names = rownames(responses))
  
  if(TIRT.format) result[result == 2] <- 0
  
  return(result)
}

get.n.facets <- function(trait = BIG.FIVE.TRAITS) {
  
  trait <- match.arg(trait)
  facets <- get.facets(trait)
  
  return(length(facets))
}

get.n.factors <- function(trait = BIG.FIVE.TRAITS) {
  
  trait <- match.arg(trait)
  
  return(get.n.facets(trait) + 1)
}

dichotomize <- function(responses, threshold = GSQ.CUTOFF.THRESHOLDS) {
	
	threshold <- match.arg(threshold, GSQ.CUTOFF.THRESHOLDS)
	responses <- ifelse(responses > threshold, 1, 0)
	
	return(responses)
}

format.vars <- function(
	responses, variables, omission.cutoff, control.tolerance,
	var.type = ITEM.TYPES, to.num = TRUE, dichotomize = FALSE, instrument = APRA2.INSTRUMENT, discard.na = TRUE
) {
	
	var.type <- match.arg(var.type)
	
	responses <- discard.wrong.responses(responses, var.type)
	if(omission.cutoff > 0) responses <- filter.by.omissions(omission.cutoff, responses)
	responses <- filter.by.control.items(
		responses, variables,
		type = var.type, tolerance = control.tolerance, discard.na = discard.na
	)
	
	responses <- get.instrument.responses(responses, variables, instrument)
	
	if(to.num) responses <- format.as.numeric(responses, TIRT.format = (var.type == ITEM.TYPES["block"]))
	
	if(dichotomize != FALSE) responses <- dichotomize(responses, threshold = dichotomize)
	
	return(responses)
}

invert.item.responses <- function(responses, items, traits = BIG.FIVE.TRAITS) {
	
	traits <- match.arg(traits, several.ok = TRUE)
	
	items <- get.trait.items(traits, items, instrument = APRA2.INSTRUMENT)
	
	responses.invert <- responses[, rownames(items)]
	response.max <- max(as.matrix(responses.invert), na.rm = TRUE)
	responses.invert <- response.max + 1 - responses.invert
	
	responses[, rownames(items)] <- responses.invert
	
	return(responses)
}

merge.reponses <- function(item.reponses, block.responses, ordering = c("items.first", "block.first")) {
	
	ordering <- match.arg(ordering)
	
	block.responses <- block.responses[rownames(block.responses) %in% rownames(item.responses), ]
	item.responses <- item.responses[rownames(item.responses) %in% rownames(block.responses), ]
	
	block.responses <- block.responses[order(rownames(block.responses)), ]
	item.responses <- item.responses[order(rownames(item.responses)), ]
	
	result <- if(ordering == "items.first") cbind(item.responses, block.responses)
		else cbind(block.responses, item.responses)
	
	return(result)
}


ITEM.DATA <- NULL
# ITEM.DATA.APRA2 <- NULL

RESPONSE.DATA <- NULL
# RESPONSE.DATA.APRA2 <- NULL

read.items(application = "APRA")
read.responses(ITEM.DATA$code)
