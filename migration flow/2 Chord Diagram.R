## Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(tidyverse)
library(igraph)
library(tidygraph)
library(chorddiag)
library(viridis)
library(scales)
library(htmlwidgets)

# Take a tibble -> tidy igraph object -> adjacency matrix -> matrix 

movement_state <- 
  as.matrix(
    as_adjacency_matrix(
      as_tbl_graph(
        migration)
      )
    )

# Create a color palette based on the viridis package
n <-
  migration %>%
  na.omit() %>%
  distinct() %>%
  group_by(to) %>%
  summarize(n()) %>%
  nrow()

viridis_no <- viridis_pal()(n) 
state_colors <-  substr(viridis_no, 0, 7)
show_col(state_colors)

state_chords <-
chorddiag(data = movement_state, 
          groupnamePadding = 40,
          groupPadding = 3,
          groupColors = state_colors,
          chordedgeColor = "",
          groupnameFontsize = 15,
          showTicks = FALSE,
          tooltipGroupConnector = "        &#x261E        ",
          tooltipFontsize = 20,
          margin = 200) %>%
  frameWidget(width = 1200, height = 1000)

state_chords 

saveWidgetframe(state_chords,
                "state_chords.html")
