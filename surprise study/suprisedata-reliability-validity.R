# set the working directory as source ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# note: surveytools2 is not found on CRAN. To install please run the following:
## if (!require("devtools")) install.packages("devtools")
## devtools::install_github("peterhurford/surveytools2")

# load libraries ----
library(tidyverse)
library(broom) 
library(patchwork)
library(surveytools2) 
library(cowplot)
library(corrr)
library(naniar)
library(missMethods)
library(mice)
library(Rtsne)
library(factoextra)
library(ggthemes)
library(RColorBrewer)
library(labeling)
library(wesanderson)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(ggchicklet)
library(wesanderson)
library(flextable)
library(showtext)
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

# pivot to tidy ----
(responses <- 
  read_csv("respondents-classification_only.csv",
           show_col_types = FALSE) %>%
  select(pid = `participant ID`,
         rating,
         variable) %>%
  pivot_wider(names_from = "variable",
              values_from = "rating",
              values_fn = list) %>%
  unnest(cols = everything()
         )
)

# Get participant IDs
parids <- 
  responses %>%
  select(pid)

# remove id column ----
numeric_items <- 
  responses %>%
  select(-pid)

missingness <- 
  gg_miss_var(numeric_items) +
  theme_fivethirtyeight() +
  scale_color_brewer(palette = "Spectral")

# only id column ----
character_items <- 
  responses  %>% 
  select(where(is.character)); character_items

# detect negative correlations ----
numeric_items %>%
  correlate() %>%
  select(-term) %>% 
  colMeans(na.rm = TRUE) %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column()
# reverse code S01, S02, E02, S03

# Missing values descriptives ----
## Missing descriptives per item ----
(missvals <- 
  numeric_items %>%
  gather(key = "item", 
         value = "num_miss") %>%
  mutate(is.missing = is.na(num_miss)) %>%
  group_by(item, is.missing) %>%
  summarise(num_missing = n()) %>%
  filter(is.missing == T) %>%
  select(-is.missing) %>%
  arrange(desc(num_missing)) %>%
  ungroup() %>%
  mutate(pct_miss = round(num_missing / sum(num_missing), 3)))
 
## Missing descriptives per item count----
count(missvals,
      name = "total item count")

## Basic descriptives ----
summary(missvals)

## Plot of missingness ----
missvals %>%
  ggplot() +
  geom_bar(aes(x = item, 
               y = num_missing, 
               fill = num_missing), 
           stat = 'identity',
           show.legend = FALSE) +
  coord_flip() +
  labs(x='item', 
       y="number of missing values", 
       title='Number of missing values') +
  theme_minimal(base_family = "roboto") +
  theme(axis.text.x = element_text(angle = 0, 
                                   hjust = 1)) + 
  scale_fill_gradientn(colors = wes_palette("Zissou1", 100, 
                                            type = "continuous")) +
  theme(strip.text = element_text(color = '#000000'),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))) +
  labs(title = "Group Frequencies",
       x = "Group",
       y = "Frequency") 

# Test for MAR/MCAR ----
mcar_test(numeric_items)
# Since p is relatively high, we reject the null and say that the data is 
# likely at least MAR

# Reverse code negatively correlated items ----
(numeric_items_rev <- 
  numeric_items %>%
  mutate(S01 = reverse_code(S01)) %>%
  mutate(S02 = reverse_code(S02)) %>%
  mutate(E02 = reverse_code(E02)) %>%
  mutate(S03 = reverse_code(S03)))

# analysis without imputation ----
psych::alpha(numeric_items_rev, 
             check.keys=TRUE)$total$raw_alpha
# ~0.79 Good internal consistency

# Factor order cols by descending count ----
(missing_counts_factored <- 
  numeric_items_rev %>%
  pivot_longer(
    cols = everything(),
    names_to = "Item",
    values_to = "Choice"
  ) %>%
  filter(is.na(Choice)) %>%
  count(Item))

# Plot no. of missing counts before imputation ----
missing_counts <- 
  ggplot(missing_counts_factored,
         aes(x = Item,
             y = n,
             fill = reorder(n, desc(n)))) +
  geom_chicklet(radius = grid::unit(5.0, 'mm'),
             width = 0.75) +
 # geom_col()  +
  coord_flip() +
  theme_minimal(base_family = "roboto") +
  theme(plot.title = element_text(family = "roboto",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20,
                                  margin = margin(b = 1.5, 
                                                  unit = "cm")),
        axis.title.x = element_text(margin = margin(t = 1.5, 
                                                    b = 0.7,
                                                    unit = "cm"),
                                    size = 13),
        axis.title.y = element_text(margin = margin(r = 1.5, 
                                                    unit = "cm"),
                                    size = 13),
        axis.text.x = element_text(size = 12,
                                   color = "#7f7f7f"),
        axis.text.y = element_text(size = 14,
                                   color = "#3f3f3f"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.box = "horizontal") +
  labs(title = "Count of Missing Values Before Imputation",
       x = "Item",
       y = "Frequency") +
  guides(fill = guide_legend(nrow = 1,
                             label.position = "bottom")) +
  scale_fill_manual(values = wes_palette("Zissou1", 12, 
                                         type = "continuous"),
                    breaks = c(8,9,11,19,20)) +
  scale_x_discrete(limits=rev); missing_counts

ggsave(missing_counts,
       file = "missingBeforeImputation.pdf",
       device = "pdf",
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600
       ) 

# impute data set ----
numeric_rev_imputed <- 
  mice(numeric_items_rev, 
       m = 15, 
       method = 'pmm')  %>%
  complete(1) %>%
  as_tibble()

# merge with participant ids ----
imputed_responses <- 
  dplyr::bind_cols(parids, numeric_rev_imputed)

write_csv(imputed_responses,
          "imputed_responses.csv")

# imputed counts factored (check) ----
(imputed_counts_factored <- 
   numeric_rev_imputed %>%
    pivot_longer(
      cols = everything(),
      names_to = "Item",
      values_to = "Choice"
    ) %>%
    filter(is.na(Choice)) %>%
    count(Item))

# Plot no. of missing counts for imputed data ----
missing_imputed <- 
  numeric_rev_imputed %>%
  pivot_longer(
    cols = everything(),
    names_to = "Item",
    values_to = "Choice"
  ) %>%
  add_column(n = 0) %>%
  select(-Choice) %>%
  ggplot(aes(x = Item,
      y = n,
      fill = reorder(n, desc(n)))) +
  geom_point(shape = 21,
             size = 4,
             stroke = 0,
             color = "transparent") +
  coord_flip() +
  theme_minimal(base_family = "roboto") +
  theme(plot.title = element_text(family = "roboto",
                                  face = "bold",
                                  hjust = 0.5,
                                  size = 20,
                                  margin = margin(b = 1.5, 
                                                  unit = "cm")),
        axis.title.x = element_text(margin = margin(t = 1.5, 
                                                    b = 0.7,
                                                    unit = "cm"),
                                    size = 13),
        axis.title.y = element_text(margin = margin(r = 1.5, 
                                                    unit = "cm"),
                                    size = 13),
        axis.text.x = element_text(size = 12,
                                   color = "#7f7f7f"),
        axis.text.y = element_text(size = 14,
                                   color = "#3f3f3f"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.box = "horizontal") +
  labs(title = "Count of Missing Values After Imputation",
       x = "Item",
       y = "Frequency") +
  guides(fill = guide_legend(nrow = 1,
                             label.position = "bottom",
                             override.aes = list(alpha = 1))) +
  scale_fill_manual(values = c("#c0c5ce")) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = c(0,5,10,15,20)); missing_imputed

ggsave(missing_imputed,
       file = "missingAfterImputation.pdf",
       device = "pdf",
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600
       ) 

# Patchworks ----
naked_counts <-
  missing_counts + 
  labs(title = "Before Imputation"); naked_counts

naked_imputed <- 
missing_imputed + 
  theme(axis.text.y = element_blank()) +
  labs(title = "After Imputation",
       x = "",
       y = ""); naked_imputed

patched_plot <-
naked_counts + naked_imputed + 
  plot_annotation(title = 'Missingness',
                  theme = theme(plot.title = element_text(size = 20,
                                                          family = "roboto",
                                                          face = "bold",
                                                          hjust = 0.5))); patched_plot

ggsave(patched_plot,
       file = "missingBefore&AfterImputation.pdf",
       device = "pdf",
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600
) 

# EFA ----
## Step 1: Check data suitability for analysis

# 1. Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
psych::KMO(numeric_rev_imputed) # Overall MSA = 0.62, < 0.7 (likely inadequate)

# 2. Bartlet's test of sphericity
psych::cortest.bartlett(numeric_rev_imputed)  # p < 0.05, items are correlated

## Determine the number of factors

# 1. Kaiser's eigenvalue > 1 rule & 2. Cattell's scree test.
scree = psych::scree(numeric_rev_imputed)
# look at --o-- FA --> 2 factors.
print(numeric_rev_imputed)
# Eigenvalues of factors > 1 = 2, then number of factor = 2

# 3. Parallel analysis.
parallel = psych::fa.parallel(numeric_rev_imputed, 
                       fa = "both")
# 2 triangles above dashed line, factor = 2
print(parallel)
# Parallel analysis suggests:
# number of factors =  2

## 2 factors
fa = psych::fa(numeric_rev_imputed, 
        nfactors = 2, 
        fm = "pa", 
        rotate = "oblimin")

print(fa, digits = 3)
print(fa, cut = .3, digits = 3)  # cut = .3 to view only FLs > 0.3
## Interpretation
# All FL < .3

## Step 3: Repeat EFA
# complexity > 1, item not specific to a factor

## Remove S01
fa_1 = psych::fa(numeric_rev_imputed[-c(1)], 
                 nfactors = 2, 
                 fm = "pa", 
                 rotate = "oblimin")

print(fa_1, cut = .3, digits = 3)


## Remove S02, L02
fa_2 = psych::fa(numeric_rev_imputed[-c(4,5)], 
                 nfactors = 2, 
                 fm = "pa", 
                 rotate = "oblimin")

print(fa_2, cut = .3, digits = 3)

## Remove S01, S02, S03
fa_2 = psych::fa(numeric_rev_imputed[-c(1,4,7)], 
                 nfactors = 2, 
                 fm = "pa", 
                 rotate = "oblimin")

print(fa_2, cut = .3, digits = 3)

#----
# No combination results in complexity < 1 where 2 or 
# possibly 3 factors would work
#----

## Factor scores (not used)
fa_2$scores
