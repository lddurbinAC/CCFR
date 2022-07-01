library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(dplyr) # A Grammar of Data Manipulation
library(patchwork) # The Composer of Plots
library(ggtext) # Improved Text Rendering Support for 'ggplot2'

# filter by Local Board and service delivery model, then plot coordinates for the visual
choose_facilities <- function(local_board) {
  facilities |> 
    filter(local_board == {{local_board}}) |> 
    arrange(name) |> 
    mutate(coord_x = (row_number()-1) %% 6, coord_y = floor((row_number()/6)-0.1))
}

count_facilities <- function(local_board, delivery_model) {
  facilities |> 
    count(local_board, delivery_model) |> 
    filter(local_board == {{local_board}} & delivery_model == {{delivery_model}}) |> 
    pull(n)
}

build_visual <- function(facilities_blocks) {
  cols <- c("council led" = "#007CB9", "community led" = "#6E963C")
  community_count <- count_facilities(facilities_blocks$local_board[[1]], "community led")
  council_count <- count_facilities(facilities_blocks$local_board[[1]], "council led")
  
  ggplot(data = facilities_blocks, aes(x = coord_x, y = -coord_y)) +
    geom_point(aes(colour = delivery_model),shape=15,size=27) +
    geom_text(aes(label = stringr::str_wrap(name, 10)), colour = "white", size = 2.8, fontface = "bold") +
    scale_x_continuous(limits = c(-0.5,6.5)) +
    scale_y_continuous(limits = c(-5.5,0.25)) +
    scale_colour_manual(values = cols) +
    theme_void() +
    ggtitle(paste0("There are ", community_count, " <span style='color:", cols[["community led"]], "'><strong>community-led</strong></span> facilities and\n", council_count, " <span style='color:", cols[["council led"]], "'><strong>Council-led</strong></span> facilities in this Local Board")) +
    theme(
      legend.position = "none",
      plot.title = element_markdown(lineheight = 1.1, size = 16, colour = "#515555")
    ) 
}

path <- here::here("data")

facilities <- readRDS(fs::dir_ls(path, glob = "*.rds")) |> filter(local_board != "Great Barrier")

plot <- choose_facilities("Franklin") |> build_visual()

