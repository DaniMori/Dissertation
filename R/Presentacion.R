rm(list = ls())

library(tidyverse)

"R/Rmarkdown_code.R" %>% source(encoding = 'UTF-8')

############################ STUDY 1 ############################

## ---- simulation_recovery_result_matrix ----

study_1_results <- tribble(
  ~Algoritmo,      ~Parametro,   ~Variable,      ~Nivel,  ~Estadistico, ~Valor,
      "MCMC", "Correlaciones",      "OPBP",       "2/3",        "RMSE",   .040,
      "MCMC", "Correlaciones",      "OPBP",       "1/3",        "RMSE",   .040,
      "MCMC", "Correlaciones",      "OPBP",         "0",        "RMSE",   .055,
      "MCMC", "Rasgo latente",      "OPBP",       "2/3",        "RMSE",   .433,
      "MCMC", "Rasgo latente",      "OPBP",       "1/3",        "RMSE",   .438,
      "MCMC", "Rasgo latente",      "OPBP",         "0",        "RMSE",   .544,
      "MCMC", "Rasgo latente",      "OPBP",       "2/3",  "Fiabilidad",   .810,
      "MCMC", "Rasgo latente",      "OPBP",       "1/3",  "Fiabilidad",   .806,
      "MCMC", "Rasgo latente",      "OPBP",         "0",  "Fiabilidad",   .700,
      "MCMC", "Rasgo latente", "OPBP x QL",  "2/3 x 18",  "Fiabilidad",   .762,
      "MCMC", "Rasgo latente", "OPBP x QL",  "1/3 x 18",  "Fiabilidad",   .756,
      "MCMC", "Rasgo latente", "OPBP x QL",    "0 x 18",  "Fiabilidad",   .633,
      "MCMC", "Rasgo latente", "OPBP x QL",  "2/3 x 36",  "Fiabilidad",   .858,
      "MCMC", "Rasgo latente", "OPBP x QL",  "1/3 x 36",  "Fiabilidad",   .856,
      "MCMC", "Rasgo latente", "OPBP x QL",    "0 x 36",  "Fiabilidad",   .768,
      "MCMC",       "Escalas",      "OPBP",       "2/3", "Correlación",   .994,
      "MCMC",       "Escalas",      "OPBP",       "1/3", "Correlación",   .992,
      "MCMC",       "Escalas",      "OPBP",         "0", "Correlación",   .972,
       "CFA", "Correlaciones",      "OPBP",       "2/3",        "RMSE",   .043,
       "CFA", "Correlaciones",      "OPBP",       "1/3",        "RMSE",   .042,
       "CFA", "Correlaciones",      "OPBP",         "0",        "RMSE",   .081,
       "CFA", "Rasgo latente",      "OPBP",       "2/3",        "RMSE",   .434,
       "CFA", "Rasgo latente",      "OPBP",       "1/3",        "RMSE",   .439,
       "CFA", "Rasgo latente",      "OPBP",         "0",        "RMSE",   .548,
       "CFA", "Rasgo latente",      "OPBP",       "2/3",  "Fiabilidad",   .809,
       "CFA", "Rasgo latente",      "OPBP",       "1/3",  "Fiabilidad",   .805,
       "CFA", "Rasgo latente",      "OPBP",         "0",  "Fiabilidad",   .695,
       "CFA", "Rasgo latente", "OPBP x QL",  "2/3 x 18",  "Fiabilidad",   .762,
       "CFA", "Rasgo latente", "OPBP x QL",  "1/3 x 18",  "Fiabilidad",   .756,
       "CFA", "Rasgo latente", "OPBP x QL",    "0 x 18",  "Fiabilidad",   .633,
       "CFA", "Rasgo latente", "OPBP x QL",  "2/3 x 36",  "Fiabilidad",   .858,
       "CFA", "Rasgo latente", "OPBP x QL",  "1/3 x 36",  "Fiabilidad",   .856,
       "CFA", "Rasgo latente", "OPBP x QL",    "0 x 36",  "Fiabilidad",   .768,
       "CFA",       "Escalas",      "OPBP",       "2/3", "Correlación",   .991,
       "CFA",       "Escalas",      "OPBP",       "1/3", "Correlación",   .989,
       "CFA",       "Escalas",      "OPBP",         "0", "Correlación",   .960,
      "MCMC", "Rasgo latente",    "Modelo",          "",     "95% ICr",   .950,
       "CFA", "Rasgo latente",    "Modelo",          "",     "95% ICr",   .937,
      "MCMC", "Rasgo latente",        "QL",        "18",     "95% ICr",   .950,
      "MCMC", "Rasgo latente",        "QL",        "36",     "95% ICr",   .950,
       "CFA", "Rasgo latente",        "QL",        "18",     "95% ICr",   .933,
       "CFA", "Rasgo latente",        "QL",        "36",     "95% ICr",   .940
) %>% mutate_if(is.character, as.factor)


## ---- empirical_reliabilities ----

emp_rels <- tribble(
  ~Rasgo,                      ~`Método`, ~Fiabilidad,
   "Estabilidad emocional",     "MCMC",    .721,
   "Estabilidad emocional",     "CFA",     .654,
   "Extraversión",              "MCMC",    .722,
   "Extraversión",              "CFA",     .645,
   "Apertura a la experiencia", "MCMC",    .579,
   "Apertura a la experiencia", "CFA",     .531,
   "Amabilidad",                "MCMC",    .592,
   "Amabilidad",                "CFA",     .541,
   "Responsabilidad",           "MCMC",    .669,
   "Responsabilidad",           "CFA",     .656
) %>% mutate(`Método`= `Método` %>% factor(levels = unique(.)))

############################ STUDY 2 ############################

## ---- study_2_simulation_results ----

study_2_results <- tribble(
            ~Variable,      ~Nivel,         ~Estadistico, ~Valor,
         "Nº bloques",        "18",         "Fiabilidad",   .618,
         "Nº bloques",        "36",         "Fiabilidad",   .741,
      "Corr. escalas",       "-.7",         "Fiabilidad",   .755,
      "Corr. escalas",         "0",         "Fiabilidad",   .705,
      "Corr. escalas",        ".7",         "Fiabilidad",   .579,
  "Corr. dimensiones",       ".00",         "Fiabilidad",   .704,
  "Corr. dimensiones",       ".25",         "Fiabilidad",   .673,
  "Corr. dimensiones",       ".50",         "Fiabilidad",   .661,
         "Nº bloques",        "18", "Dif. correlaciones", -0.211,
         "Nº bloques",        "36", "Dif. correlaciones", -0.142,
      "Corr. escalas",       "-.7", "Dif. correlaciones", -0.076,
      "Corr. escalas",         "0", "Dif. correlaciones", -0.135,
      "Corr. escalas",        ".7", "Dif. correlaciones", -0.319,
  "Corr. dimensiones",       ".00", "Dif. correlaciones", -0.216,
  "Corr. dimensiones",       ".25", "Dif. correlaciones", -0.193,
  "Corr. dimensiones",       ".50", "Dif. correlaciones", -0.120,
  "BSC x LTC", "-.7 x .00",         "Fiabilidad",           .755,
  "BSC x LTC", "-.7 x .25",         "Fiabilidad",           .751,
  "BSC x LTC", "-.7 x .50",         "Fiabilidad",           .757,
  "BSC x LTC", "  0 x .00",         "Fiabilidad",           .719,
  "BSC x LTC", "  0 x .25",         "Fiabilidad",           .699,
  "BSC x LTC", "  0 x .50",         "Fiabilidad",           .696,
  "BSC x LTC", " .7 x .00",         "Fiabilidad",           .637,
  "BSC x LTC", " .7 x .25",         "Fiabilidad",           .570,
  "BSC x LTC", " .7 x .50",         "Fiabilidad",           .530
)


## ---- study_2_load_sim_data ----

load("res/Summary_results_3D_to_SPSS.Rdata")

result.table.3D <- result.table[result.table$Dimensions == 3, ] %>%
  as_tibble %>% rename(
    Fiabilidad                 = `mean empirical reliability`,
    `Distorsión correlaciones` = `mean true-estimates corr diff`,
    `Corr. escalas`            = `Item condition`
  ) %>%
  mutate(
    `Corr. escalas` = `Corr. escalas` %>%
      factor(levels = levels(.), labels = c("-.7", " .0", " .7"))
  )


############################ STUDY 3 ############################

## ---- data_load ----

# Item and block properties:
vars <- load.vars()
items <- vars$items
blocks <- vars$blocks


# Log-likelihood ratio tests:
LL.ratio.tests <- load.LL.ratio.tests(blocks)	%>% mutate(
  Block_code = Block,
  Block = n() %>% seq_len %>% as.character,
  `Item 2 missing` = `Item 2` %>% (assertive::is_na) %>% if_else("X", "")
)

# Unrestricted model:
Wald.tests.model <- readModels(target = "res/apra2_fcq_gsq_joint_es_op_ag_co_0_mlr_wald.out")


## ---- data_formatting ----

trait.levels <- TRAIT.NAMES[-2]
trait.levels.ES <- c("Neuroticismo", "Apertura", "Amabilidad", "Responsabilidad")

# Joint estimation parameters tibble:

scale.params		 <- Wald.tests.model$parameters$unstandardized %>% filter(paramHeader %>% grepl(".BY", x = .))
intercept.params <- Wald.tests.model$parameters$unstandardized %>% filter(paramHeader %>% grepl("Thresholds", x = .))

scale.params %<>% mutate(paramHeader = paramHeader %>% magrittr::extract(TRAIT.NAMES, .) %>% factor)

block.item.params.joint <- blocks %>% rownames_to_column(var = "Block_num") %>% left_join(
  scale.params %>% filter(param %in% blocks$Block) %>%
    select(Block = param, `Trait 1` = paramHeader, `Block scale 1` = est)
) %>%
  left_join(
    scale.params %>% filter(param %in% blocks$Block) %>%
      transmute(Block = param, `Trait 2` = paramHeader, `Block scale 2` = -est)
  ) %>%
  left_join(
    scale.params %>% filter(param %in% blocks$`Item 1`) %>% select(`Item 1` = param, `Scale item 1` = est)
  ) %>% 
  left_join(
    scale.params %>% filter(param %in% blocks$`Item 2`) %>% select(`Item 2` = param, `Scale item 2` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P3_[0-9]{1,3}\\$1$", x = .)) %>%
      transmute(`Block` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Intercept` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$1$", x = .)) %>%
      transmute(`Item 1` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 1 Item 1` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$1$", x = .)) %>%
      transmute(`Item 2` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 1 Item 2` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$2$", x = .)) %>%
      transmute(`Item 1` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 2 Item 1` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$2$", x = .)) %>%
      transmute(`Item 2` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 2 Item 2` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$3$", x = .)) %>%
      transmute(`Item 1` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 3 Item 1` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$3$", x = .)) %>%
      transmute(`Item 2` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 3 Item 2` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$4$", x = .)) %>%
      transmute(`Item 1` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 4 Item 1` = est)
  ) %>% 
  left_join(
    intercept.params %>% filter(param %>% grepl("P[4-5]_[0-9]{1,3}\\$4$", x = .)) %>%
      transmute(`Item 2` = param %>% str_split("\\$") %>% map_chr(magrittr::extract, 1), `Threshold 4 Item 2` = est)
  ) %>% mutate(
    `Threshold 1 diff` = `Threshold 1 Item 1` - `Threshold 1 Item 2`,
    `Threshold 2 diff` = `Threshold 2 Item 1` - `Threshold 2 Item 2`,
    `Threshold 3 diff` = `Threshold 3 Item 1` - `Threshold 3 Item 2`,
    `Threshold 4 diff` = `Threshold 4 Item 1` - `Threshold 4 Item 2`
  ) %>% filter(`Block scale 1` %>% is_not_na) %>% 
  mutate(
    `Trait 1` = `Trait 1` %>% factor(levels = trait.levels),
    `Trait 2` = `Trait 2` %>% factor(levels = trait.levels)
  )

scales.joint.collapsed <- block.item.params.joint %>% 
  collapse.scale.params(`Block scale 1`, `Block scale 2`, `Scale item 1`, `Scale item 2`)


## ---- unidimensionality_assessment ----

unidimensionality.assessment <- assess.unidimensionality(
  get.instrument.items(read.APRA2.new.items()$items, instrument = APRA2.INSTRUMENT, strings.as.factors = FALSE),
  TRAIT.NAMES[-2]
)
unidimensionality.assessment$item.scales %<>%
  mutate(Polarity = Polarity %>% parse_factor(levels = POLARITY))


## ---- LR_tests ----

n.contrasts <- LL.ratio.tests %>% summarise_at(vars(ends_with("p-value")), ~(is_not_na(.) %>% sum)) %>% gather %>%
  summarize(n = value %>% sum) %>% extract2("n")

sig <- .05
alpha <- sig / n.contrasts

LL.sig <- LL.ratio.tests %>% mutate_at(vars(ends_with("p-value")), magrittr::is_less_than, alpha) %>% 
  select(-`Polarity 1`, -ends_with("LR"))

LL.scales.collapsed <- LL.ratio.tests %>% 
  collapse.scale.params(`Scale 1 LR`, `Scale 2 LR`, `Scale 1 p-value`, `Scale 2 p-value`) %>%
  rename(`LLR` = `Block scale`, `p-value` = `Item scale`) %>%
  mutate(NI = `p-value` < alpha) %>% filter(NI %>% is_not_na) %>% 
  mutate(
    Trait = Trait %>% parse_factor(levels = TRAIT.NAMES[-2]),
    `Trait other` = `Trait other` %>% parse_factor(levels = TRAIT.NAMES[-2])
  )

LL.summary <- LL.sig %>% {
  bind_rows(
    summarize_at(., vars(ends_with("p-value")), .funs = sum, na.rm = TRUE),
    summarize_at(., vars(ends_with("p-value")), .funs = ~(mean(., na.rm = TRUE) * 100))
  ) %>% select(
    `Scale 1` = `Scale 1 p-value`, `Scale 2` = `Scale 2 p-value`,
    `Intercept - threshold 1` = `Intercept 1 p-value`, `Intercept - threshold 2` = `Intercept 2 p-value`,
    `Intercept - threshold 3` = `Intercept 3 p-value`, `Intercept - threshold 4` = `Intercept 4 p-value`
  )
} %>% t %>% data.frame(stringsAsFactors = FALSE) %>%
  rownames_to_column(var = "Parameter") %>% rename(Count = X1, `%` = X2) %>%
  bind_rows(
    bind_cols(
      Parameter = "Scales",
      LL.scales.collapsed %>% summarize(
        Count = NI %>% sum(na.rm = TRUE), `%` = NI %>% mean(., na.rm = TRUE) * 100
      )	
    )
  )


## ---- all_stats_table ----

block.item.params.joint %<>% mutate(
  `Scale 1 bias` = `Block scale 1` - `Scale item 1`,
  `Scale 2 bias` = `Block scale 2` - `Scale item 2`,
  `Intercept - thresh 1 bias` = Intercept - `Threshold 1 diff`,
  `Intercept - thresh 2 bias` = Intercept - `Threshold 2 diff`,
  `Intercept - thresh 3 bias` = Intercept - `Threshold 3 diff`,
  `Intercept - thresh 4 bias` = Intercept - `Threshold 4 diff`,
  `Scale 1 rel bias` = `Scale 1 bias` / `Scale item 1`,
  `Scale 2 rel bias` = `Scale 2 bias` / `Scale item 2`,
  `Intercept - thresh 1 rel bias` = `Intercept - thresh 1 bias` / `Threshold 1 diff`,
  `Intercept - thresh 2 rel bias` = `Intercept - thresh 2 bias` / `Threshold 2 diff`,
  `Intercept - thresh 3 rel bias` = `Intercept - thresh 3 bias` / `Threshold 3 diff`,
  `Intercept - thresh 4 rel bias` = `Intercept - thresh 4 bias` / `Threshold 4 diff`,
  `Scale 1 sq. err.` = `Scale 1 bias`^2,
  `Scale 2 sq. err.` = `Scale 2 bias`^2,
  `Intercept - threshold 1 sq. err.` = `Intercept - thresh 1 bias`^2,
  `Intercept - threshold 2 sq. err.` = `Intercept - thresh 2 bias`^2,
  `Intercept - threshold 3 sq. err.` = `Intercept - thresh 3 bias`^2,
  `Intercept - threshold 4 sq. err.` = `Intercept - thresh 4 bias`^2
)

intercept.stats <- block.item.params.joint %>% summarize_at(
  vars(ends_with("bias")),
  mean, na.rm = TRUE
) %>% gather(key = "Parameter", value = "Bias") %>%
  mutate(
    Rel = Parameter %>% str_detect("rel bias$") %>%
      if_else("Mean rel. bias", "Mean bias")
  ) %>% select(-Parameter) %>%
  bind_cols(
    Parameter = c("Scale 1", "Scale 2", "Intercept - threshold" %>% paste(1:4)) %>%
      rep(2)
  ) %>% spread(key = Rel, value = Bias) %>%
  full_join(
    block.item.params.joint %>% summarize_at(
      vars(ends_with("sq. err.")), mean, na.rm = TRUE
    ) %>% gather(key = "Parameter", value = RMSE) %>%
      mutate(Parameter = Parameter %>% str_remove(" sq. err."))
  ) %>% filter(Parameter %>% str_detect("Intercept")) %>%
  full_join(
    block.item.params.joint %>% summarize_at(
      vars(ends_with("diff")),
      ~cor(., block.item.params.joint$Intercept, use = "pairwise")
    ) %>% gather(key = "Parameter", value = "Correlation") %>% 
      mutate(Parameter = "Intercept - threshold" %>% paste(1:4)),
    .
  )

scales.joint.collapsed %<>% mutate(
  Bias = `Block scale` - `Item scale`,
  `Relative bias` = Bias / `Item scale`,
  `Square error` = Bias^2
) %>%
  left_join(LL.scales.collapsed %>% select(Item, LLR, `p-value`, NI)) %>%
  left_join(
    unidimensionality.assessment$item.scales %>% select(Item, I_ECV, Rel_bias)
  ) %>%
  left_join(
    unidimensionality.assessment$item.scales %>%
      select(
        `Item other` = Item, `I_ECV other` = I_ECV, `Rel_bias other` = Rel_bias
      )
  ) %>%
  mutate(
    `Block polarity` = (Polarity == `Polarity other`) %>%
      if_else("Homopolar", "Heteropolar", NA_character_) %>%
      parse_factor(levels = NULL)
  )

scale.stats <- scales.joint.collapsed %>% summarize(
  Parameter = "Scales",
  `Correlation` = cor(`Block scale`, `Item scale`, use = "pairwise"),
  `Mean bias` = Bias %>% mean(na.rm = TRUE),
  `Mean rel. bias` = `Relative bias` %>% mean(na.rm = TRUE),
  RMSE = `Square error` %>% mean(na.rm = TRUE) %>% sqrt
)

joint.estimation.stats <- scale.stats %>% bind_rows(intercept.stats)

block.item.params.joint %<>% rename(
  `Predicción umbral 1` =  `Threshold 1 diff`,
  `Predicción umbral 2` =  `Threshold 2 diff`,
  `Predicción umbral 3` =  `Threshold 3 diff`,
  `Predicción umbral 4` =  `Threshold 4 diff`,
  `Intersección` = Intercept, Bloque = Block_num
)


## ---- intercepts_preprocessing ----

block.item.params.joint %<>% rename(`Block_code` = Block) %>%
  left_join(LL.ratio.tests) %>%
  left_join(
    LL.sig %>%
      rename_at(vars(ends_with("p-value")), ~str_replace(., "p-value", "NI"))
  ) %>%
  mutate(
    `Neuroticism block` = (`Trait 1` == "Neuroticism") |
      (`Trait 2` == "Neuroticism"),
    `Conscientiousness block` = (`Trait 1` == "Conscientiousness") |
      (`Trait 2` == "Conscientiousness"),
    `Openness block` = (`Trait 1` == "Openness") |
      (`Trait 2` == "Openness"),
    `Agreeableness block` = (`Trait 1` == "Agreeableness") |
      (`Trait 2` == "Agreeableness"),
    Polarity = (`Polarity 1` %>% as.character == `Polarity 2` %>% as.character) %>%
      if_else("Homopolar", "Heteropolar")
  )

intercept.predictions <- block.item.params.joint %>%
  select(
    Block, Block_code, matches("^Trait"), matches("block$"),
    matches("^Polarity"), matches("^Item"), matches("^Block scale"),
    matches("^Scale "), `Intersección`
  ) %>%
  full_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Non-invariant", factor_key = TRUE,
      matches("Intercept [1-4] NI")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Non-invariant`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Threshold item 1", factor_key = TRUE,
      matches("Threshold [1-4] item 1")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Threshold item 1`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Threshold item 2", factor_key = TRUE,
      matches("Threshold [1-4] item 2")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Threshold item 2`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Threshold diff", factor_key = TRUE,
      matches("Predicción umbral [1-4]")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Threshold diff`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Intercept deviation", factor_key = TRUE,
      matches("Intercept - thresh [1-4] bias")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Intercept deviation`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Intercept relative bias", factor_key = TRUE,
      matches("Intercept - thresh [1-4] rel bias")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Intercept relative bias`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Intercept square error", factor_key = TRUE,
      matches("Intercept - threshold [1-4] sq. err.")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Intercept square error`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Intercept LR", factor_key = TRUE,
      matches("Intercept [1-4] LR")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Intercept LR`)
  ) %>%
  left_join(
    block.item.params.joint %>% gather(
      key = "Threshold category", value = "Intercept p-value", factor_key = TRUE,
      matches("Intercept [1-4] p-value")
    ) %>% mutate(`Threshold category` = `Threshold category` %>% as.integer) %>%
      select(Block_code, `Threshold category`, `Intercept p-value`)
  ) %>%
  mutate(
    `Threshold category` = `Threshold category` %>% parse_factor(NULL),
    Block = Block %>% parse_factor(NULL)
  )

non.invariant.intercepts <- intercept.predictions %>%
  filter(`Non-invariant`) %>%
  mutate(
    `Threshold category` = `Threshold category` %>% factor(labels = "Umbral "),
    `Polarity 2` = `Polarity 2` %>%
      factor(labels = c("Homopolar", "Heteropolar")),
    `Trait 1` = `Trait 1` %>% factor(levels = trait.levels, labels = trait.levels.ES),
    `Trait 2` = `Trait 2` %>% factor(levels = trait.levels, labels = trait.levels.ES)
  ) %>%
  rename(
    Bloque = Block, `Desviación intersección` = `Intercept deviation`,
    `Rasgo 1` = `Trait 1`, `Rasgo 2` = `Trait 2`
  )
