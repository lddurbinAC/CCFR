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

build_visual <- function(facilities_blocks) {
  cols <- c("council led" = "#00304B", "community led" = "#6E963C")
  
  ggplot(data = facilities_blocks, aes(x = coord_x, y = -coord_y)) +
    geom_point(aes(colour = delivery_model),shape=15,size=27) +
    geom_text(aes(label = stringr::str_wrap(name, 10)), colour = "white", size = 2.8, fontface = "bold") +
    scale_x_continuous(limits = c(-0.5,6.5)) +
    scale_y_continuous(limits = c(-5.5,0.25)) +
    scale_colour_manual(values = cols) +
    theme_void() +
    theme(legend.position = "none") +
    ggtitle(paste0("There are XX facilities in the ", facilities_blocks$local_board, " Local Board, listed below"))
}

path <- here::here("data")

facilities <- readRDS(fs::dir_ls(path, glob = "*.rds")) |> filter(local_board != "Great Barrier")

choose_facilities("Franklin") |> build_visual()

# ggsave(
#   here::here("plots/patchwork.png"),
#   plot = plot,
#   device = "png",
#   bg = "white",
#   dpi = 300,
#   height = 10,
#   width = 15
#   )

