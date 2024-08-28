# Reviewer 1 asked if we could explore whether participants were more likely
# to select the accurate advisor over the agreeing advisor when they had high
# as opposed to low confidence in their initial estimates.
source("src/R/analysis.R")

# Grab all participant data
p_data <- bind_rows(
  familiarisation %>%
    filter(E == "ava") %>% 
    mutate(test = F) %>%
    select(pid, test, initialConfidence, Advisor),
  test %>%
    filter(E == "ava") %>% 
    mutate(test = T) %>%
    select(pid, test, initialConfidence, Advisor)
)

# Median split confidence into high/low by pid
with_conf_split <- p_data %>%#
  nest(d = -pid) %>%
  mutate(d = map(d, \(x) {
    m <- median(x$initialConfidence)
    x %>% mutate(highConfidence = initialConfidence > m)
  })) %>%
  unnest(cols = d)

diff <- with_conf_split %>%
  group_by(pid, highConfidence) %>%
  # average within participant
  summarise(pPickAcc = mean(Advisor == "High accuracy"), .groups = "drop") %>%
  # drop participants who didn't select both advisors
  pivot_wider(
    names_from = highConfidence, 
    names_glue = '{.value}_hc{highConfidence}',
    values_from = pPickAcc
  ) %>%
  filter(
    !is.na(pPickAcc_hcTRUE),
    !is.na(pPickAcc_hcFALSE)
  ) %>%
  # calculate difference
  transmute(diff = pPickAcc_hcTRUE - pPickAcc_hcFALSE)

diff

hist(diff$diff)

t.test(diff$diff, mu = 0)
