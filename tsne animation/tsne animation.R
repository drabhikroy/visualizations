library(tidyverse)
library(gganimate)
library(tweenr)
library(data.table)
library(cowplot)

# Set the working directory as source ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ss = 180

## tSNE dimensions
set.seed(ss)
tsne_dims <- 
  read_csv("data/tsne_dims.csv",
           show_col_types = FALSE) %>%
  mutate(tsne_x = tsne_x * runif(1, 1, 1.4),
         tsne_y = tsne_y * runif(1, 0.6, 1.2)) 

## Random dimensions
random_disc_jitter <- 
  function(num_points,
           disc_radius,
           random_seed = ss) {
    
    if(!is.null(random_seed)) {
      set.seed(random_seed)
    }
    
    # Random radius positions
    r <- runif(num_points, 0, disc_radius ^ 2)
    
    # Random angles
    t <- runif(num_points, 0, 2 * pi)
    
    # Convert radius and angles to Cartesian coordinates
    data.frame(x = sqrt(r) * cos(t),
               y = sqrt(r) * sin(t))
  }

# Get the max x or y value from the tsne dims for the radius
max_tsne <- 
  max(abs(c(tsne_dims$tsne_x, 
            tsne_dims$tsne_y)))

tsne_dims <- 
  tsne_dims %>%
  mutate(random_x = random_disc_jitter(num_points = n(),
                                       disc_radius = max_tsne)$x,
         random_y = random_disc_jitter(num_points = n(),
                                       disc_radius = max_tsne)$y)

## Calculate Fermat spiral dimensions
fermat_jitter <- 
  function(num_points, 
           size, 
           center_x, 
           center_y) {
    
    golden_ratio <- (sqrt(5) + 1) / 2
    
    fibonacci_angle <- 360 / (golden_ratio ^ 2)
    
    circle_r <- sqrt(size / num_points)
    
    x <- rep(center_x, num_points)
    y <- rep(center_y, num_points)
    
    for (m in 1:(num_points - 1)) {
      n <- m - 1
      r <- circle_r * sqrt(n)
      theta <- fibonacci_angle * (n)
      x[n] <- center_x + r * cos(theta)
      y[n] <- center_y + r * sin(theta)
      
    }
    
    data.frame(x = x, 
               y = y)
    
  }

# Some trial and error for this value to find a size that packs the points without getting too much overlap
max_size <- 16

# Calculate the max cluster n to scale the jitter radius
max_n <- 
  max(table(tsne_dims$cluster_id))

tsne_dims <- 
  tsne_dims %>%
  group_by(cluster_id) %>%
  mutate(centroid_x = mean(tsne_x, trim = 0),
         centroid_y = mean(tsne_y, trim = 0)) %>%
  mutate(fermat_x = fermat_jitter(num_points = n(),
                                  size = max_size * n() / max_n,
                                  center_x = centroid_x[1],
                                  center_y = centroid_y[1])$x,
         fermat_y = fermat_jitter(num_points = n(),
                                  size = max_size * n() / max_n,
                                  center_x = centroid_x[1],
                                  center_y = centroid_y[1])$y) %>%
  ungroup()

## Build the animation
state_0 <- 
  data.frame(x = tsne_dims$random_x,
             y = tsne_dims$random_y,
             fill = "#a7adba",
             stroke = 1)
  
state_1 <- 
  data.frame(x = tsne_dims$random_x,
             y = tsne_dims$random_y,
             fill = tsne_dims$cluster_color,
             stroke = 0)

state_2 <- 
  data.frame(x = tsne_dims$tsne_x,
             y = tsne_dims$tsne_y,
             fill = tsne_dims$cluster_color,
             stroke = 0)

state_3 <- 
  data.frame(x = tsne_dims$fermat_x,
             y = tsne_dims$fermat_y,
             fill = tsne_dims$cluster_color,
             stroke = 0)

state_list <- 
  list(state_0, state_1, state_2, state_3, state_2, state_1, state_0)

tweened_states <- 
  tween_states(state_list, 
               tweenlength = 2,
               statelength = 1,
               ease = "cubic-in-out",
               nframes = 96)

tweened_plot <- 
  tweened_states %>% 
  as_tibble() %>%
  rename(frame = .frame,
         id = .id,
         phase = .phase) %>%
  mutate(id = as.factor(frame)) %>%
  ggplot(aes(x = x, 
             y = y)) +
  geom_point(aes(fill = fill,
                 stroke = stroke),
             color = "#fdfdfd",
             shape = 21,
             size = 2.2) +
  scale_fill_identity() +
  theme_nothing() +
  transition_manual(frame)

tsne_anim <-
  animate(tweened_plot, 
          nframes = 600, 
          fps = 60, 
          bg = 'transparent'); tsne_anim

anim_save("tsne_anim.gif", tsne_anim)
