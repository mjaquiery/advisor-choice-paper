# Statistical analysis of experiment data
# 
# The data are loaded via the esmData:: package

# Prerequisites -----------------------------------------------------------

### Libraries -----------------------------------------------------------

library(tidyverse)  # Pipes, tidyselectors, string manipulation, data wrangling, etc.
library(glue) # Nice string construction
library(broom) # tidy-style tests
library(ez) # provides a convenient interface to ANOVA
library(esmData)    # My own data package holding the data for the project (`remotes::install_github('oxacclab/esmData')`)

### Configuration -------------------------------------------------------

set.seed(20211217)

options(
  tinytex.verbose = TRUE,  # Verbose output to track down warnings
  scipen = 10,  # Stop scientific notation for p-values
  warning.length = 8170  # Print lots of warning text for tracing tex warnings
)

# Set the ggplot theme to something that looks nice
theme_set(
  theme_light() +
    theme(
      rect = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      text = element_text(size = 14),
      legend.position = 'top',
      strip.text = element_text(colour = 'black')
    )
)

### Functions -----------------------------------------------------------

#' Turn a number into a </= X representation
n2s <- function(n, digits = 2, limit = NULL) {
  if (!is.null(limit)) {
    if_else(n < limit, glue('< {limit}'), glue('= {round(n, digits)}'))
  } else {
    glue('= {round(n, digits)}')
  }
}

#' Go through the workspace and remove excluded pids from all tbls with a pid 
#' field, creating backups along the way
#' @param exclusions tbl with a pid column and n>1 logical columns where TRUE indicates that a participant is excluded for columnName reason
#' @param envir environment in which to do modifications
#' @param backup whether to back up previous version of objects as all.[objectName]
do_exclusions <- function(exclusions, envir = .GlobalEnv, backup = T) {
  # Prepare excluded pid list
  exclusions$excluded <- exclusions %>% select(-pid) %>% apply(1, any)
  pids <- exclusions$pid[!exclusions$excluded]
  
  for (o in ls(envir = envir)) {
    if (o == 'exclusions') next()
    if (str_starts(o, 'all\\.')) next()
    obj <- get(o, envir = envir)
    if ('pid' %in% names(obj)) {
      if (backup & paste0('all.', o) %in% ls(envir = envir)) {
        warning(paste0('all.', o, ' already exists: overwriting.'))
      }
      if (backup) {
        assign(paste0('all.', o), obj, envir = envir)
      }
      obj <- obj[obj$pid %in% pids, ]
      assign(o, obj, envir = envir)
    }
  }
}

#' Return the name of an advice type profile
#' @param advisorType vector of advisor types
#' @details Used for advisors in the Dots task data.
#' @return vector of type names
advisor_profile_name <- function(advisorType) {
  case_when(
    advisorType == 3 ~ 'Bias-sharing',
    advisorType == 4 ~ 'Anti-bias',
    advisorType == 5 ~ 'High accuracy',
    advisorType == 6 ~ 'Low accuracy',
    advisorType == 7 ~ 'High agreement',
    advisorType == 8 ~ 'Low agreement',
    advisorType == 9 ~ 'High accuracy',
    advisorType == 10 ~ 'High agreement',
    T ~ NA_character_
  )
}

#' Return the name of an advice type profile for the Dates task data
#' @param advisor0idDescription vector of advisor descriptions
#' @details Used for advisors in the Dates task data.
#' @return vector of nice names
advisor_description_name <- function(advisor0idDescription) {
  case_when(
    advisor0idDescription == 'highAccuracy' ~ 'High accuracy',
    advisor0idDescription == 'lowAccuracy' ~ 'Low accuracy',
    advisor0idDescription == 'highAgreement' ~ 'High agreement',
    advisor0idDescription == 'lowAgreement' ~ 'Low agreement',
    advisor0idDescription == 'Accurate' ~ 'High accuracy',
    advisor0idDescription == 'Agreeing' ~ 'High agreement',
    advisor0idDescription == 'inGroup' ~ 'Always honest',
    advisor0idDescription == 'outGroup' ~ 'Sometimes deceptive',
    advisor0idDescription == 'honest' ~ 'Honest group',
    advisor0idDescription == 'deceptive' ~ 'Deceptive group',
    advisor0idDescription == 'mass' ~ 'Group member',
    advisor0idDescription == 'single' ~ 'Consistent individual',
    advisor0idDescription == 'lowConf' ~ 'Low confidence',
    advisor0idDescription == 'highConf' ~ 'High confidence',
    T ~ NA_character_
  )
}

#' Return a copy of x with the factors of x ordered how we want them to ensure
#' consistency across plots
#' @param x tbl whose factors should be reordered
#' @return \code{x} with reordered factors
order_factors <- function(x) {
  #' Reorder a factor 
  #' Logic is a bit twisted because R kept reversing factors even
  #' when asked nicely not to
  .f <- function(f) {
    if (length(levels(f)) != 2)
      return(f)
    if (
      all(str_detect(levels(f), c('^[hH]igh ?accuracy', 
                                  '^[hH]igh ?agreement'))) ||
      all(str_detect(levels(f) , c('^[dD]eceptive', '^[hH]onest'))) ||
      all(str_detect(levels(f), c('^[bB]ias', '^[aA]nti'))) ||
      all(str_detect(levels(f), c('^[hH]igh', '^[lL]ow'))) ||
      all(str_detect(levels(f), c('^[fF]inal', '^[iI]nitial'))) ||
      all(str_detect(levels(f), c('^[fF]eedback', '^[nN]o'))) ||
      all(str_detect(levels(f), c('^[cC]orrect', '^[iI]ncorrect'))) ||
      all(str_detect(levels(f), c('^[aA]gree', '^[dD]isagree'))) ||
      all(str_detect(levels(f), c('^[aA]s planned', '^[aA]nomalous')))
    )
    fct_rev(f)
    else
      f
  }
  mutate(x, across(.cols = where(is.factor), .f))
}


## Printing stats ------------------------------------------------------------

#' Produce marginal means summaries for a 2x2 ANOVA on long data
#' @param data tbl to process
#' @param dv dependent variable
#' @param wid identifier for cases
#' @param interaction name for the interaction
#'
#' @details Within variables are collapsed first for the interaction, in order
#'   of their listing, followed by between variables.
#'   Will process all variables that aren't the DV or the WID.
#'
#' @note Will not work where the number of factors exceeds 2
marginalMeans <- function(
    data, 
    dv = names(data)[2], 
    wid = "pid",
    interaction = "Difference"
) {
  fx <- function(x) str_to_title(x) %>% str_replace_all(" ", "")
  tmp <- data %>% 
    ungroup %>%
    rename(
      pid = {{wid}},
      dv = {{dv}}
    )
  vars <- select(tmp, c(-pid, -dv)) %>% names()
  out <- list()
  # factors
  for (v in vars) {
    out[[v]] <- tmp %>% group_by(pid, across(all_of(v))) %>%
      summarise(dv = mean(dv), .groups = "drop") %>%
      group_by(across(all_of(v))) %>%
      summarise(
        s = glue("$M_{{{fx(.data[[v]])}}} = {sprintf('%.02f', mean(dv))}$"),
        .groups = "drop"
      ) %>%
      unique()
  }
  if (length(vars) == 2) {
    # interaction
    tmp <- tmp %>%
      group_by(pid, across(all_of(vars))) %>%
      summarise(dv = mean(dv), .groups = "drop") %>%
      pivot_wider(names_from = vars[1], values_from = dv) 
    
    out$.interactionExpression <- glue("{names(tmp)[3]} - {names(tmp)[length(names(tmp))]}")
    
    out$interaction <- tmp %>%
      mutate(dv = .[[3]] - .[[4]]) %>%
      group_by(across(all_of(vars[2]))) %>%
      summarise(
        s = glue("$M_{{{fx(interaction)}|{fx(.data[[vars[2]]])}}} = {sprintf('%.02f', mean(dv))}$"),
        .groups = "drop"
      ) %>%
      unique()
  }
  out
}

p2str <- function(p) {
  s <- sprintf("%.03f", p)
  if (s == '0' || str_detect(s, '-?0\\.0+$'))
    return(" < .001")
  s <- str_replace(s, '0\\.', '.')
  return(glue(" = {s}"))
}

#' Produce a summary for an ANOVA and marginal means set
#' @param ANOVA ANOVA object from an ezANOVA call
#' @param mMeans marginal means list from a marginalMeans call
summariseANOVA <- function(ANOVA, mMeans) {
  slug <- function(s) str_replace_all(str_to_lower(s), " ", "")
  if (class(ANOVA) == "list" && has_name(ANOVA, "ANOVA"))
    ANOVA <- ANOVA$ANOVA
  mm <- mMeans[lapply(mMeans, is_tibble) == T]
  mm <- lapply(1:length(mm), \(i) {
    tibble(
      Effect = slug(names(mm)[i]),
      mm = mm[[i]] %>% pull(s) %>% paste(collapse = ", ")
    )
  }) %>% 
    bind_rows()
  ANOVA %>%
    mutate(Effect = if_else(str_detect(Effect, ":"), "interaction", Effect)) %>%
    nest(d = -Effect) %>%
    mutate(
      aov = map_chr(
        d, 
        ~ glue("$F({.$DFn},{.$DFd}) = {sprintf('%.02f', .$F)}$, $p{p2str(.$p)}$")
      ),
      Effect = slug(Effect)
    ) %>%
    unnest(d) %>%
    left_join(mm, by = "Effect") %>%
    mutate(s = paste(aov, mm, sep = "; "))
}


# Dots task ---------------------------------------------------------------

# Setup environments for each experiment. 
# Each experiment uses essentially the same analysis structure, so we'll 
# save time and space by looping over these different environments to run the 
# analysis.
dots <- list(
  acc = new.env(),
  agr = new.env(),
  ava = new.env()
)

# Confidence-contingent advice is not included in the main report
cca <- new.env()


# Accuracy
select_experiment(
  project = 'dotstask',
  function(x) filter(x, study == 'Accuracy', version == '120 practice trials'),
  envir = dots$acc
)
# Agreement
select_experiment(
  project = 'dotstask',
  function(x) filter(x, study == 'Agreement'),
  envir = dots$agr
)
# Accuracy versus agreement
select_experiment(
  project = 'dotstask',
  function(x) 
    filter(x, study == 'Accuracy vs agreement', version == 'v2 Serial design'),
  envir = dots$ava
)
# Confidence-contingent advice
select_experiment(
  project = 'dotstask',
  function(x) filter(x, study == 'MetaCog', version == '2c Fixed'),
  envir = cca
)

## Preprocess data --------------------------------------------------------

for (E in c(dots, cca)) {
  E$trials <- annotate_responses(E$trials)
}

# Add in nice confidence category for CCA trials
cca$trials <- cca$trials %>%
  mutate(
    Confidence = factor(
      case_when(
        confidenceCategory == 0 ~ "Low",
        confidenceCategory == 1 ~ "Medium",
        confidenceCategory == 2 ~ "High"
      )
    )
  )


## Exclusions -----------------------------------------------------------

# Criteira
nMaxOutliers <- 2
zThresh <- 3
accuracyRange <- c(.6, .85)
minTrialsPerCategory <- 12

for (E in c(dots, cca)) {
  tmp <- E$trials %>% 
    nest(d = -pid) %>%
    mutate(d = map_dbl(d, ~ mean(.$initialAnswerCorrect)))
  
  E$exclusions <- tibble(pid = unique(E$trials$pid)) %>%
    mutate(
      `Accuracy too low` = pid %in% filter(tmp, d < accuracyRange[1])$pid,
      `Accuracy too high` = pid %in% filter(tmp, d > accuracyRange[2])$pid
    )
  
  tmp <- E$trials %>% 
    filter(!practice) %>%
    nest(d = c(-pid, -confidenceCategory)) %>%
    mutate(n = map_int(d, nrow)) %>%
    select(-d) %>%
    pivot_wider(names_from = confidenceCategory, 
                names_prefix = "cc", 
                values_from = n) %>%
    mutate(
      anyNA = is.na(cc0) | is.na(cc1) | is.na(cc2),
      lowest = pmin(cc0, cc1, cc2, na.rm = T)
    )
  
  E$exclusions <- E$exclusions %>% 
    mutate(
      `Missing confidence categories` = pid %in% filter(tmp, anyNA)$pid,
      `Skewed confidence categories` = 
        pid %in% filter(tmp, lowest < minTrialsPerCategory)$pid
    )
  
  do_exclusions(E$exclusions, envir = E)
  
  E$exclusions$`Total excluded` <- E$exclusions %>% 
    select(-pid) %>% 
    apply(1, any)
  
  E$exclusions_summary <- E$exclusions %>% 
    summarise(across(where(is.logical), sum)) %>%
    mutate(`Total remaining` = length(unique(E$trials$pid))) %>% 
    pivot_longer(
      everything(), 
      names_to = "Reason", 
      values_to = "Participants excluded"
    )
  
  E$familiarisation <- E$trials %>%
    mutate(Advisor = factor(advisor_profile_name(adviceType))) %>%
    filter(typeName == "force", !is.na(Advisor)) %>%
    order_factors()
  
  E$test <- E$trials %>% 
    filter(hasChoice) %>%
    mutate(
      Advisor = advisor_profile_name(adviceType),
      Advisor = factor(Advisor)
    ) %>%
    order_factors()
}


## Recombine data -------------------------------------------------------

familiarisation <- NULL

for (n in names(dots)) {
  familiarisation <- bind_rows(
    familiarisation,
    dots[[n]]$familiarisation %>% mutate(E = n)
  )
}

test <- NULL

for (n in names(dots)) {
  test <- bind_rows(
    test,
    dots[[n]]$test %>% mutate(E = n)
  )
}

# Dates task --------------------------------------------------------------

dates <- list(
  acc = new.env(),
  agr = new.env(),
  ava = new.env(),
  ava.pre = new.env()
)

# Accuracy
select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'accuracyDates', manipulationOK),
  envir = dates$acc
)
# Agreement
select_experiment(
  project = 'datequiz',
  function(x) filter(x, study == 'agreementDates', manipulationOK),
  envir = dates$agr
)
# Accuracy versus Agreement
select_experiment(
  project = 'datequiz',
  f = function(x) filter(x, study == 'advisorChoice', replication),
  envir = dates$ava
)
select_experiment(
  'datequiz', 
  function(x) filter(x, study == 'advisorChoice', version == 'v0-0-6'), 
  envir = dates$ava.pre
)

## Preprocess data -------------------------------------------------------
for (E in dates) {
  E$AdvisedTrial <- annotate_responses(E$AdvisedTrial) %>%
    filter(!is.na(block))
}

## Exclusions ------------------------------------------------------------

# Criteria for all
maxTrialRT <- 60000   # trials take < 1 minute
minTrials <- 11       # at least 11 trials completed
minChangeRate <- .1   # some advice taken on 10%+ of trials
# Differences by experiment
minKeyTrials.binary <- 10
minKeyTrials.cont <- 8
markerList <- c(7, 13, 21) # marker widths only used in continuous

for (E in dates) {
  continuous <- has_name(E$AdvisedTrial, 'responseMarkerWidth')
  if (continuous) {
    minKeyTrials <- minKeyTrials.cont
  } else {
    minKeyTrials <- minKeyTrials.binary
  }
  
  E$AdvisedTrial <- E$AdvisedTrial %>% filter(timeEnd <= maxTrialRT)
  
  E$exclusions <- E$AdvisedTrial %>% 
    nest(d = -pid) %>%
    mutate(
      `Too few trials` = map_lgl(d, ~ nrow(.) < minTrials),
      `Too few choice trials` = map_lgl(
        d, 
        ~ sum(!is.na(.$advisorChoice)) < minKeyTrials
      )
    )
  
  if (continuous) {
    E$exclusions <- E$exclusions %>%
      mutate(
        `Insufficient advice-taking` = 
          map_lgl(
            d, 
            ~ mean(.$responseEstimateLeft != .$responseEstimateLeftFinal) < 
              minChangeRate
          ),
        # Only applies to continuous experiments
        `Wrong markers` = 
          map_lgl(
            d, 
            ~ !all(
              all(.$responseMarkerWidth %in% markerList),
              all(.$responseMarkerWidthFinal %in% markerList)
            )
          ),
        `Non-numeric advice` = map_lgl(d, ~ any(is.na(.$advisor0adviceCentre)))
      )
  } else {
    E$exclusions <- E$exclusions %>%
      mutate(
        `Insufficient advice-taking` = 
          map_lgl(
            d, 
            ~ mean(.$responseAnswerSide != .$responseAnswerSideFinal |
                     .$responseConfidence != .$responseConfidenceFinal) < minChangeRate
          )
      )
  }
  
  E$exclusions <- E$exclusions %>% select(-d)
  
  do_exclusions(E$exclusions, envir = E)
  
  
  E$exclusions$`Total excluded` <- E$exclusions %>% select(-pid) %>% apply(1, any)
  E$exclusions_summary <- E$exclusions %>% 
    summarise(across(where(is.logical), sum)) %>%
    mutate(`Total remaining` = length(unique(E$AdvisedTrial$pid))) %>% 
    pivot_longer(
      everything(), 
      names_to = "Reason", 
      values_to = "Participants excluded"
    )
  
  E$fb <- E$AdvisedTrial %>% 
    group_by(pid) %>%
    summarise(Feedback = max(feedback)) %>%
    mutate(Feedback = if_else(Feedback == 1, "Feedback", "No feedback"))
  
  tmp <- E$AdvisedTrial %>%
    left_join(E$fb, by = "pid") %>%
    mutate(
      Advisor = advisor_description_name(advisor0idDescription),
      Advisor = factor(Advisor),
      Feedback = factor(Feedback)
    ) %>%
    order_factors()
  
  E$Familiarisation <- tmp %>% filter(is.na(advisorChoice) | !advisorChoice) 
  
  E$Test <- tmp %>% filter(advisorChoice)
}


## Recombine data --------------------------------------------------------

Familiarisation <- NULL

for (n in names(dates)) {
  Familiarisation <- bind_rows(
    Familiarisation,
    dates[[n]]$Familiarisation %>% mutate(E = n)
  )
}

Test <- NULL

for (n in names(dates)) {
  Test <- bind_rows(
    Test,
    dates[[n]]$Test %>% mutate(E = n)
  )
}


### Join binary data ---------------------------------------------------

Bin <- bind_rows(
  familiarisation %>%
    filter(!practice) %>%
    transmute(
      task = "Dots",
      experiment = E,
      pid = paste0("dots_", pid),
      feedback = F,
      Advisor,
      has_choice = hasChoice,
      answer_initial = initialAnswer,
      answer_final = finalAnswer,
      confidence_initial = initialConfidence,
      confidence_final = finalConfidence,
      influence = advisorInfluenceRaw,
      influence_capped = advisorInfluence
    ),
  test %>%
    filter(!practice) %>%
    transmute(
      task = "Dots",
      experiment = E,
      pid = paste0("dots_", pid),
      feedback = F,
      Advisor,
      has_choice = hasChoice,
      answer_initial = initialAnswer,
      answer_final = finalAnswer,
      confidence_initial = initialConfidence,
      confidence_final = finalConfidence,
      influence = advisorInfluenceRaw,
      influence_capped = advisorInfluence
    ),
  Test %>% 
    filter(!is.na("responseAnswerSide")) %>%
    transmute(
      task = "Dates",
      experiment = E,
      pid = paste0("dates_", pid),
      feedback,
      Advisor,
      has_choice = advisorChoice,
      answer_initial = responseAnswerSide,
      answer_final = responseAnswerSideFinal,
      confidence_initial = responseConfidenceScore,
      confidence_final = responseConfidenceScoreFinal,
      influence = advisor0Influence,
      influence_capped = advisor0InfluenceCapped
    ),
  Familiarisation %>% 
    filter(!is.na("responseAnswerSide")) %>%
    transmute(
      task = "Dates",
      experiment = E,
      pid = paste0("dates_", pid),
      feedback,
      Advisor,
      has_choice = advisorChoice,
      answer_initial = responseAnswerSide,
      answer_final = responseAnswerSideFinal,
      confidence_initial = responseConfidenceScore,
      confidence_final = responseConfidenceScoreFinal,
      influence = advisor0Influence,
      influence_capped = advisor0InfluenceCapped
    )
)

# Data collection dates ---------------------------------------------------

# dots
bind_rows(
  select(cca$participants, starts_with('time')),
  select(dots$acc$participants, starts_with('time')),
  select(dots$agr$participants, starts_with('time')),
  select(dots$ava$participants, starts_with('time'))
) %>%
  summarise(startTime = min(timeStart), endTime = max(timeEnd))
# 2018-09-16 - 2020-06-23

min(Familiarisation$serverTime, na.rm = T)  # 2020-05-13
max(Familiarisation$serverTime, na.rm = T)  # 2020-06-22

