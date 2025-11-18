

# 0 :  Necessary Packages ------------------------------------------------------


# List of required packages
required_packages <- c("ggspatial", "ggplot2", "sf", "tmap", "here", "magick",
                       "grid", "cowplot", "gganimate", "gifski", "leaflet",
                       "png", "leaflet.extras", "viridis", "dplyr", "wordcloud2",
                       "webshot", "htmltools", "rnaturalearthdata", "grid",
                       "patchwork")

# Install packages that are not already  installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

#Load required libraries
library(ggplot2)       # For creating plots and visualization
library(sf)            # For working with spatial data
library(tmap)          # For thematic mappin g
library(here)          # For managing file paths
library(magick)        # For image manipulation
library(grid)          # For working with grid graphics 
library(cowplot)       # For combining plots and adding elements (e.g., logos)
library(ggspatial)     # For scale bars and north arrows in ggplot maps
library(leaflet)       # For building interactive maps
library(leaflet.extras)# For Extends the capabilities of leaflet
library(viridis)       # For a visually appealing colors
library(sf)            # For spatial data handling
library(wordcloud2)    # For word cloud
library(dplyr)         # For data manipulation
library(cowplot)       # For combining plots
library(webshot)       # For saving web as an image
library(png)           # For reading PNG images
library(htmlwidgets)   # For working with HTML files
library(htmltools)     # For Tools of  HTML files
library(rnaturalearthdata)
library(grid)          # For annotation_custom
library(patchwork)     # For arranging plots
library(gganimate)     # For making animation
library(tidyverse)     # For data manipulation and visualization
library(gganimate)     # For creating animated plots

#  00 : Load Spatial Data ---------------------------

#  Disappeared (water and polder) mills shape file path
disappeared_mills <- here("00-Mills Memory", "data", "shp", "verdwenenmolens.shp")
#  Existing (water and Polder) mills shape file  path
existing_mills <- here("00-Mills Memory", "data", "shp", "Molens.shp")
#  other types of mills shape file path
other_mills <- here("00-Mills Memory", "data", "shp", "weidemolens en windmotoren.shp") 

# Read shape files for various spatial features
north_sea <- st_read(here("00-Mills Memory", "data", "shp", "NorthSea.shp"))
# Netherlands, Germany and Belgium border shape file path
nl_border <- st_read(here("00-Mills Memory", "data", "shp", "gadm41_NLD_0.shp"))
gr_border <- st_read(here("00-Mills Memory", "data", "shp", "GR.shp"))
bl_border <- st_read(here("00-Mills Memory", "data", "shp", "BG.shp"))

#border of provinces and cities in Netherlands shape file path
nl_stats_border <- st_read(here("00-Mills Memory", "data", "shp", "gadm41_NLD_1.shp"))
nl_cities_border <- st_read(here("00-Mills Memory", "data", "shp", "gadm41_NLD_2.shp"))
#Surface water and Populated places  in Netherlands shape file path
oppervlaktewater <- st_read(here("00-Mills Memory", "data", "shp", "oppervlaktewater.shp"))
nl_populated_palces <- st_read(here("00-Mills Memory", "data", "shp", "populated_places.shp"))


# Load and preprocess shape files for disappeared mills
ex_mills <- st_read(here(disappeared_mills)) |>
  st_transform(crs = st_crs(nl_border)) |>
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box

# Load existing mills shape file
mills <- st_read(existing_mills)
other_mills <- st_read(other_mills)

# Calculate centroids for the borders for labling and other usages ..
nl_stats_border <- nl_stats_border %>%
  mutate(
    x = st_coordinates(st_centroid(geometry))[, 1],
    y = st_coordinates(st_centroid(geometry))[, 2]
  )

#  000 : Data Inspection --------------------------------------------------------

# Inspect first few rows of each data set
head(mills)
head(ex_mills)
head(other_mills)

print(nl_populated_palces)
head(nl_stats_border)
head(gr_border)
head(north_sea)

# Summarize datasets
summary(mills)
summary(ex_mills)
summary(other_mills)

# Count number of features in each data set
num_disappeared_mills <- nrow(ex_mills)
cat("Total disappeared mills:", num_disappeared_mills, "\n")
num_existing_mills <- nrow(mills)
cat("Total existing mills:", num_existing_mills, "\n")
num_other_mills <- nrow(other_mills)
cat("Total other mills:", num_other_mills, "\n")

# List all spatial datasets
spatial_data <- list(
  "North Sea" = north_sea,
  "GR Border" = gr_border,
  "BL Border" = bl_border,
  "NL Border" = nl_border,
  "NL Provinces" = nl_stats_border,
  "NL Cities" = nl_cities_border,
  "Surface Water" = oppervlaktewater,
  "Populated Places" = nl_populated_palces,
  "Existing Mills" = mills,
  "Disappeared Mills" = ex_mills,
  "Other Mills" = other_mills
)

# Check and print the CRS for each data set
for (name in names(spatial_data)) {
  crs_name <- st_crs(spatial_data[[name]])$Name
  cat(name, "Coordinate System:", crs_name, "\n")
}

# Make ggplot basic map for Overview of Mills data and Key Features

# 1-Get the bounding box of the Netherlands shape file
bbox <- st_bbox(nl_border)
bbox
# 2-Create a data frame for external region labels
external_labels <- data.frame(
  name = c("North Sea", "Germany", "Belgium"),
  x = c(4.0, 7.0, 5),  # Approximate longitude for labels
  y = c(53.5, 51.5, 51) # Approximate latitude for labels
)
# 3-Define a base map with Netherlands' border and North Sea
base_map <- ggplot() +
  # North Sea,Netherlands',Germany' and Belgium' border
  geom_sf(data = north_sea, fill = "darkgrey", color = "black", lwd = 0.5) + 
  geom_sf(data = nl_border, fill = "white", color = "black", lwd = 0.5) + 
  geom_sf(data = gr_border, fill = "lightgrey", color = "black", lwd = 0.5) + 
  geom_sf(data = bl_border, fill = "lightgrey", color = "black", lwd = 0.5) + 
  theme_minimal() +
  labs(title = "Overview of Mills data and Key Features for data check")
# 4-Add other spatial features to the map
full_map <- base_map +
  geom_sf(data = nl_stats_border,
          color = "black", fill = NA, linetype = "dotted") + # Province borders
  geom_sf(data = nl_cities_border,
          color = "black", fill = NA, linetype = "dashed") + # City borders
  geom_sf(data = oppervlaktewater, 
          color = "blue", fill = NA, lwd = 0.3) + # Surface water
  geom_sf(data = other_mills,
          color = "lightgray", size = 0.8, shape = 17) + # Other mills
  geom_sf(data = ex_mills,
          color = "gray", size = 0.8, shape = 15) + # Disappeared mills
  geom_sf(data = mills,
          color = "black", size = 0.8) + # Existing mills
  geom_sf(data = nl_populated_palces,
          color = "red", size = 3) + # Populated places
  # Crop to Netherlands' bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"]))  
# 5-Add centroids for labeling
full_map <- full_map +
  geom_text(data = nl_stats_border, aes(x = x, y = y, label = NAME_1), 
            color = "black", size = 3, check_overlap = TRUE)
# 6-Add external region labels
full_map <- full_map +
  geom_text(data = external_labels, aes(x = x, y = y, label = name), 
            color = "darkblue", size = 4, fontface = "italic")

# 7-Plot the first map to check data
print(full_map)


#  01 : Map of Existing and Disappeared Mills  ---------------------------------

# +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+ #
# |M|a|p| |o|f| |E|x|i|s|t|i|n|g| |a|n|d| |D|i|s|a|p|p|e|a|r|e|d| |M|i|l|l|s| #
# +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+ #
  
main_plot <- ggplot() +
  # Add geographic background features
  geom_sf(data = north_sea,
          fill = "lightblue", color = NA, alpha = 0.5) + # North Sea
  geom_sf(data = gr_border,
          color = NA, alpha = 0.5) +                     # Germany border
  geom_sf(data = bl_border, 
          color = NA, alpha = 0.5) +                   # Belgium border
  geom_sf(data = oppervlaktewater,
          fill = "lightblue", color = NA, alpha = 0.5) + # Surface water in Nl
  geom_sf(data = nl_border,
          fill = "black", color = NA, alpha = 0.3) +     # Nl border(dark mode)
  geom_sf(data = nl_stats_border,
          fill = NA , color = NA) +            # Netherlands Province borders
  # Add mills data
  geom_sf(data = ex_mills,
          aes(color = "Disappeared Mills"), size = 0.5) +  # Disappeared mills
  geom_sf(data = mills,
          aes(color = "Existing Mills"), size = 0.5) +     # Existing mills
  # Add populated cities 
  geom_sf(data = nl_populated_palces, 
          aes(shape = "circle"), size = 2, show.legend = FALSE) +
  # Add main text labels (smaller, black text on top)       
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "black",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  #add external label of North Sea,Germany and Belgium
  geom_text(data = external_labels, aes(x = x, y = y, label = name), 
            color = "darkgray", size = 3, fontface = "bold.italic")+
  # Customize color legend for mills
  scale_color_manual(
    name = "▪ Legend",  # Legend title
    values = c(
      "Existing Mills" = "#993404",  # Darker brown for existing mills
      "Disappeared Mills" = "#fec44f"  # Lighter yellow for disappeared mills
    ),
    labels = c(
      paste0("Disappeared Mills\n No.= (", num_disappeared_mills, ")\n"),
      paste0("Existing Mills \n No.= (", num_existing_mills, ")")
    )
  ) +
  guides( # Customize legend symbols
    color = guide_legend(override.aes = list(size = 3)) 
  )+
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills' Memory : Water and Wind",
    subtitle = "▪ Existing and Disappeared Mills in the Netherlands",
    caption = 
    "▪ Data : www.molendatabase.nl  |  www.molendatabase.net
▪ Map visualization : Massoud Ghaderian | R Studio | 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  
  # Apply minimal theme with customizations
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold",
                              margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14,
                                 margin = margin(t = 0, b=8)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", 
                                margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
   
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5), 
    legend.text = element_text(size = 10),  # Increase size 
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5), 
    # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(# Move x-axis labels to the right (hjust = 1)
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),
    ),
    axis.text.y = element_text(# Move y-axis labels up (vjust = 1.5)
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(# Choose a style for the north arrow
      fill = c("white", "white"), line_col = "black"),
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.4, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.6, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Display  main plot
main_plot

# Read and convert logo to raster
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')  
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)
# Combine main plot with logo using cowplot
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.80, y = 0.70, width = 0.20, height = 0.20)

# Display the final plot
final_plot
# Save the final plot 
ggsave("Exiting and Disappeared Mills.jpg", plot = final_plot, 
       width = 8, height = 10, dpi = 300, 
       path = here("00-Mills Memory/outputs"))


# 02 : Interactive Map of Disappeared Mills ------------------------------------

# +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+ #
# |I|n|t|e|r|a|c|t|i|v|e| |M|a|p| |o|f| |D|i|s|a|p|p|e|a|r|e|d| |M|i|l|l|s| #
# +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+ #


# Load and preprocess shape files for disappeared mills
ex_mills <- st_read(here(disappeared_mills)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box
head(ex_mills) 
  
  # Create interactive map
leaflet_map <- leaflet(data = ex_mills) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~st_coordinates(ex_mills)[, 2],   # Latitude from geometry
    lng = ~st_coordinates(ex_mills)[, 1],   # Longitude from geometry
    color = "#fec44f",                      # Circle color (yellow)
    radius = 2,                            # Size of the yellow circle
    popup = ~paste("<b>", molen_naam, "</b><br>",  # Show the name of the mill
                   "<a href='", infolink, "' target='_blank'>Click here for more info</a>")
    )%>%  
  addProviderTiles(providers$CartoDB.Positron) %>%  # Lighter and clean tile
  addCircleMarkers(
    lat = ~st_coordinates(ex_mills)[, 2],  
    lng = ~st_coordinates(ex_mills)[, 1],  
    color = "#fec44f", 
    radius = 6,
    clusterOptions = markerClusterOptions(),
    popup = ~paste("<b>", molen_naam, "</b><br>",  
                   "<a href='", infolink, "' target='_blank'>Click here for more info</a>")
  ) %>%
  addLegend(
    "bottomright",
    colors = c("#fec44f"),
    labels = c("Disappeared Mills"),
    title = "Legend"
  ) %>%
  addScaleBar(position = "bottomleft") %>%  # Add a scale bar
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Zoom to Fit",
    onClick = JS("function(btn, map){ map.fitBounds(map.getBounds()); }")
  ))  # Add a zoom-to-fit button 

# Define  titles and caption
leaflet_map <- tags$html(
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; height: 100%; width: 100%; } /* Ensure no scroll bars */
      .map-container { height: 100%; width: 100%; display: flex; flex-direction: column; }
      .map-title { font-size: 24px; font-weight: bold; text-align: center; margin: 10px 0 5px; }
      .map-subtitle { font-size: 18px; color: gray; text-align: center; margin-bottom: 10px; }
      .map-caption { font-size: 14px; text-align: center; margin-top: 10px; color: darkslategray; }
      #map { flex-grow: 1; } /* Make the map fill remaining space */
    "))
  ),
  tags$body(
    div(class = "map-container",
        div(class = "map-title", "▪ Mills' Memory : Water and Wind"),
        div(class = "map-subtitle", "▪ A geospatial visualization of Disappeared Mills in the Netherlands"),
        div(id = "map", leaflet_map),  # Map div
        div(class = "map-caption", "Source: Dutch Heritage Dataset | Created by Your Name")
    )
  )
)

# Save the map as an HTML file
# saveWidget(leaflet_map, "00-Mills Memory/outputs/Disappeared Mills.html" , selfcontained = TRUE)

# Save the map with titles and caption
save_html(leaflet_map, "00-Mills Memory/outputs/Disappeared_Mills_with_Titles.html")

# 03 : Heat map of Disappeared mills  -----------------------------------------

   #    +-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
   #    |H|e|a|t| |m|a|p| |o|f| |D|i|s|a|p|p|e|a|r|e|d| |m|i|l|l|s|   #
   #    +-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
  
# Make sure it is projected for spatial analysis (e.g., EPSG: 3857 for meters)
ex_mills <- st_transform(ex_mills, crs = 3857) 

# Load the Netherlands border shape file
netherlands_border <- st_read("00-Mills Memory/data/shp/gadm41_NLD_0.shp")
netherlands_border <- st_transform(netherlands_border, crs = 3857)

# Extract coordinates for disappeared mills
mills_coords <- st_coordinates(ex_mills)
# Convert mills to a data frame for ggplot2
mills_df <- data.frame(x = mills_coords[, 1], y = mills_coords[, 2])

# Create the heat map with the Netherlands border
heatmap_plot <- ggplot() +
  # Add the heat map
  stat_density_2d(data = mills_df, aes(
    x = x, y = y, fill = ..density..), geom = "raster", contour = FALSE,alpha = .8) +
  scale_fill_viridis(option = "magma", name = "Density") +  # Use a magma color palette
  # Add the  border of Netherlands and Province
  geom_sf(data = netherlands_border, fill = NA, color = NA, size = 0.5) +
  geom_sf(data = nl_stats_border, fill = NA , color = "white") +    
  #Add populated cites and their labels 
  # geom_sf(data = nl_populated_palces, aes(shape = "circle"),
  #         size = 2,color = "white" ,show.legend = FALSE) +
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "white",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills' Memory : Water and Wind",
    subtitle = "▪ Heat Map of  Disappeared Mills in the Netherlands",
    caption = 
      "▪ Data : www.molendatabase.nl  |  www.molendatabase.net
▪ Map visualization : Massoud Ghaderian | R Studio | 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme_minimal() +
  theme(
    #black background
    plot.background = element_rect(fill = "black", color = NA),
    # panel.background = element_rect(fill = "black", color = NA),
    
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold",
                              margin = margin(b = 0), color = "white"),
    plot.subtitle = element_text(hjust = -0.01, size = 14,
                                 margin = margin(t = 0 , b= 10), color = "white"),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic",
                                margin = margin(t = 15), color = "white"),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  #  legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "black", color = NA, size = 0.5),  
    legend.text = element_text(size = 10, color = "white"),  
    legend.title = element_text(size = 12, color = "white"),  
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "white", size = 0.5), 
    panel.grid.minor = element_line(color = "white", size = 0.5),

    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "white", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "white", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "white",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 0.5 ,margin = margin(t = -.01),
      # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -.01),
      # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(
      fill = c("white", "white"), # Arrow fill colors
      line_col = "black",         # Border color for arrows
      text_col = "white"            # Set the color of the "N"
    ),
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.6, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, #  the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), #  the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white"),
    text_col = "white"  #  the scale bar text color
    )

# Display the heat map
print(heatmap_plot)

# Save the final plot 
ggsave("Heat Map of  Disappeared Mills.jpg", plot = heatmap_plot, 
       width = 8, height = 10, dpi = 300, 
       path = here("00-Mills Memory/outputs"))


# 04 : Histogram of Disappearance Years ----------------------------------------

#    +-+-+-+-+-+-+-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
#    |H|i|s|t|o|g|r|a|m| |o|f| |D|i|s|a|p|p|e|a|r|a|n|c|e| |Y|e|a|r|s|   #
#    +-+-+-+-+-+-+-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
  
#Data Preparation 
head(ex_mills)
num_disappeared_mills <- nrow(ex_mills)
cat("Total disappeared mills:", num_disappeared_mills, "\n")

# Convert 'verdwenen1' to numeric for disappeared mills
ex_mills <- ex_mills[ex_mills$verdwenen1 != 0, ] #to reject 0 values 
ex_mills$year <- as.integer(ex_mills$verdwenen1)  #to reject NA and invalid values
ex_mills$year
summary(ex_mills$year)

# Calculate the frequency of each year
year_freq <- table(ex_mills$year)
year_freq
# Find the maximum frequency and  associated  year(s)  
max_freq <- max(year_freq)
max_freq
max_years <- names(year_freq[year_freq == max_freq])
max_years
# find maximum valid frequency ( not NA or 0 or not mentioned)
sum_freq <- sum(year_freq)
print(sum_freq)

# Aggregate data to count the number of ex-mills per year
ex_mills_data_aggregated <- ex_mills %>%
  group_by(year) %>%
  summarise(Count = n())  # Count the number of records per year


# Filter years where the count of disappeared mills is greater than 300
ex_mills_above_300 <- ex_mills_data_aggregated %>%
  filter(Count > 300)

##  Combine 1 ------------------------------------------------------------

# Filter dataset for mills disappeared between 1800 and 2024
ex_mills_filtered <- ex_mills %>%
  filter(year >= 1800 & year <= 2024)
head(ex_mills_filtered)
# Convert data to an sf object (spatial format)
ex_mills_sf <- st_as_sf(ex_mills_filtered, coords =
                          c("longitude", "latitude"), crs = 4326)

# Load a base map for the Netherlands (using rnaturalearth)
netherlands_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Netherlands")

# Create the map plot (foreground)
Map_plot_ex_mills <- ggplot() +
  geom_sf(data = netherlands_map, fill = "#f7f7f7", color = "black", size = 1) +  # Basemap
  geom_sf(data = ex_mills_sf, aes(color = year), size = 2, alpha = 0.7) +  # Mills points
  scale_color_viridis_c(option = "plasma", name = "Year", direction = -1) +  # Color scale for years
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Create the line plot (background)
Line_plot_ex_mills <- ggplot(ex_mills_data_aggregated, aes(x = year, y = Count)) +
  geom_line(color = "gray", size = 1) +  # Line plot
  geom_point(color = "black", size = 2) +  # Optional: Add points to show data point
    # Add labels for years with more than 300 disappeared mills
  geom_text(data = ex_mills_above_300, 
            aes(label = Count), 
            color = "red", 
            size = 3, 
            vjust = -0.5,  # Adjust vertical position of the label
            fontface = "bold") +  # Make labels bold
  theme_minimal() +  # Use minimal theme
  scale_x_continuous(
    breaks = c(min(ex_mills_data_aggregated$year), 1800, 2000, 2020),  # Set breaks
    labels = c(min(ex_mills_data_aggregated$year), "1800", "2000", 2020)  # Set labels
  )+
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.text.y = element_blank(),  # Remove y-axis numbers
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_text(
      angle = 90, 
      vjust = 0.5, 
      hjust = 3,
      size = 10,  # Adjust font size
      color = ifelse(
        c(min(ex_mills_data_aggregated$year), 1200, 1800, 2000, 2020) %in% c(1200, 1800, 2000, 2020), 
        "red", 
        "black"
      )  # Apply red color to specific years
    )
  )
# Save Line plot as a rasterized object
line_rasterized <- ggplotGrob(Line_plot_ex_mills)

# Combine the map and line plot (line plot in the background)
combined_plot <- ggplot() +
  geom_sf(data = netherlands_map, fill = "#f7f7f7", color = "#cccccc", size = 0.3) +  # Map plot
  geom_sf(data = ex_mills_sf, aes(color = year), size = 2, alpha = 0.7) +  # Mills points
  # Add populated cities 
  geom_sf(data = nl_populated_palces, 
          aes(shape = "circle"), size = 2, show.legend = FALSE) +
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "black",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  scale_color_viridis_c(option = "plasma", name = "Year") +  # Color scale for years
  annotation_custom(grob = line_rasterized,
                    xmin = bbox["xmin"] + 0, xmax = bbox["xmax"] + 1, 
                    ymin = bbox["ymin"] -0.5, ymax = bbox["ymin"] + 3
  )+
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills' Memory : Water and Wind",
    subtitle = "▪ Location/Time/Number of disappeared mills in the Netherlands (1800-2020)",
    caption = "▪ Data : www.molendatabase.nl  |  www.molendatabase.net
▪ Map visualization : Massoud Ghaderian | R Studio | 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme_minimal() +
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold",
                              margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14,
                                 margin = margin(t = 0, b=8)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", 
                                margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "top"),  # legend's top-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5), 
    legend.text = element_text(size = 10),  # Increase size 
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5), 
    # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(# Move x-axis labels to the right (hjust = 1)
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),
    ),
    axis.text.y = element_text(# Move y-axis labels up (vjust = 1.5)
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(# Choose a style for the north arrow
      fill = c("white", "white"), line_col = "black"),
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.8, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("Combined plot ex_mills.jpg", plot = combined_plot, 
       width = 8 , height = 10, dpi = 300, 
       path = here("00-Mills Memory/outputs"))

##  Combine 2 ------------------------------------------------------------

# Filter data set for mills disappeared between 1800 and 2024
ex_mills_filtered <- ex_mills %>%
  filter(year >= 1800 & year <= 2024)

# Convert data to an sf object (spatial format)
ex_mills_sf <- st_as_sf(ex_mills_filtered, coords = c("longitude", "latitude"), crs = 4326)

# Load a basemap for the Netherlands (using rnaturalearth)
netherlands_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Netherlands")

# Create the map plot (foreground)
Map_plot_ex_mills <- ggplot() +
  geom_sf(data = netherlands_map, fill = "#f7f7f7", color = "#cccccc", size =1) +  # Basemap
  geom_sf(data = ex_mills_sf, aes(color = year), size = 2, alpha = 0.7) +  # Mills points
  scale_color_viridis_c(option = "plasma", name = "Year") +  # Color scale for years
  labs(
    title = "▪ Mills' Memory : Water and Wind",
    subtitle = "▪ Locations/Time/Number of disappeared mills in the Netherlands (1800-2020)",
  ) +
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold",
                              margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14,
                                 margin = margin(t = 0, b=8)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", 
                                margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5), 
    legend.text = element_text(size = 10),  # Increase size 
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5), 
    # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(# Move x-axis labels to the right (hjust = 1)
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),
    ),
    axis.text.y = element_text(# Move y-axis labels up (vjust = 1.5)
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(# Choose a style for the north arrow
      fill = c("white", "white"), line_col = "black"),
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.5, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Create the line plot (background)
Line_plot_ex_mills <- ggplot(ex_mills_data_aggregated, aes(x = year, y = Count)) +
  geom_line(color = "gray", size = 1) +  # Line plot
  geom_point(color = "black", size = 2) +  # Optional: Add points to show data point
  theme_minimal() +  # Use minimal theme
  scale_x_continuous(
    breaks = c(min(ex_mills_data_aggregated$year), 1800, 2000, 2020),  # Set breaks
    labels = c(min(ex_mills_data_aggregated$year), "1800", "2000", 2020)  # Set labels
  )+
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.text.y = element_blank(),  # Remove y-axis numbers
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  
  )


# Use patchwork to combine the map and line plot
combined_plot <- Map_plot_ex_mills + 
  Line_plot_ex_mills + 
  plot_layout(ncol = 1, heights = c(3, 1))  # Place line plot below the map plot

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("Combined_plot_ex_mills_left_aligned.jpg", plot = combined_plot, 
       width = 12, height = 10, dpi = 300, 
       path = here("00-Mills Memory/outputs"))


# 05 : Animation of  Disappearance Years  --------------------------------------

  #   +-+-+-+-+-+-+-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
  #   |A|n|i|m|a|t|i|o|n| |o|f| |D|i|s|a|p|p|e|a|r|a|n|c|e| |Y|e|a|r|s|   #
  #   +-+-+-+-+-+-+-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+   #
  
# Base plot setup (no animation yet)
base_map <- ggplot() +
  # Netherlands national border (dark mode)
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.9) +   
  geom_sf(data = ex_mills, size = 0.7 ,aes(color = "Disappeared Mills")) +
  geom_sf(data = nl_populated_palces, aes(shape = "Populated Places"),
          size = 2, color = "white",show.legend = FALSE) +
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 2,  # Adjust size of label
            color = "white",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = .2, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  labs(
    title = "▪ Mills' Memory : Water and Wind",
    subtitle = "▪ Time-Animationa of Disappeared Mills in the Netherlands: {frame_time}",
    caption = "▪ Data : www.molendatabase.nl  |  www.molendatabase.net
▪ Map visualization : Massoud Ghaderian | R Studio | 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  )+
  #add external label of North Sea,Germany and Belgium
  geom_text(data = external_labels, aes(x = x, y = y, label = name), 
            color = "darkgray", size = 4, fontface = "bold.italic")+
  scale_color_manual(
    name = "▪ Legend",  # Legend title
    values = c("Disappeared Mills" = "#fec44f"),
    labels = c("Disappeared Mills")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 2)  # Change legend point size
    )
  ) +
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(
      hjust = -0.01, size = 18, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(
      hjust = -0.01, size = 14, margin = margin(t = 0)),
    plot.caption = element_text(
      hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),
    # Optional: Add background and border to legend
    legend.text = element_text(size = 10),  # Increase size to 10 (adjust as needed)
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    # Major grid lines: gray color, thickness 0.
    
  
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "black",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10), 
      # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),
      # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(fill = c("white", "white"),
                                           line_col = "black"),
    # Choose a style for the north arrow
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.8, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.9, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )
#show static map
base_map
# Save the 3d base map to check
ggsave("3D basemap.jpg", plot = base_map, 
       width = 12, height = 10, dpi = 300, 
       path = here("00-Mills Memory/outputs"))

##  Animation 1 --------------------------------------------

# Add transition for animation using the 'year' field for time-based animation
animated_map <- base_map +
  transition_time(year) +          # Transition over years (animation)
  ease_aes('linear')               # Ensure smooth transition between years
# Show the animation
animated_map

# Render the animation as a video (MP4 format)
animated_map_output <- animate(
  animated_map, 
  width = 800, 
  height = 600, 
  fps = 15, 
  duration = 30, 
  res =150,
  # renderer = av_renderer()  # Use av_renderer to create a video
)

# Optionally, save the animation as a GIF
anim_save("/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/ex_millsAni.gif"
          , animated_map_output)

# Optionally, save the animation as an MP4 file
anim_save("/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/ex_millsAni.mp4"
          , animated_map_output)


##  Animation 2 --------------------------------------------

# Prepare Cumulative Data
str(ex_mills)
head(ex_mills)
ex_mills
# Create a sequence of years
years_list <- seq(min(ex_mills$year, na.rm = TRUE),
                  max(ex_mills$year, na.rm = TRUE))
years_list
# Expand data for each mill and year
expanded_data <- expand.grid(mill_id = 1:nrow(ex_mills), year = years_list) %>%
  left_join(ex_mills %>% mutate(mill_id = row_number()), by = "mill_id") %>%
  mutate(
    year = as.integer(year),  # Explicitly convert 'year' to integer
    disappeared_year = as.integer(verdwenen1),  # Ensure 'verdwenen1' is numeric
    visible = ifelse(year < verdwenen1, TRUE, FALSE)  # Visibility logic
  )

head(expanded_data)

# Base Map Setup
base_map <- ggplot() +
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.8) +  
  geom_sf(data = expanded_data %>% filter(visible), 
          aes(geometry = geometry, group = mill_id), 
          size = 0.7, col = "#fec44f") +
  labs(
    title = "Timeline of Mills in the Netherlands: {closest_state}",
    subtitle = "Yellow: Mills Visible Before Disappearance",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Add Animation Transition
animated_map <- base_map +
  transition_states(
    states = year,                  # Transition over years
    transition_length = 2,          # Adjust transition speed
    state_length = 1                # Pause per year
  ) +
  ease_aes('linear')                # Smooth transitions

animated_map

# Render Animation
animated_map_output <- animate(
  animated_map, 
  width = 800, 
  height = 600, 
  fps = 10,                         # Frames per second
  duration = length(years) * 2,     # Total duration
  renderer = av_renderer()
)
# Save the Animation
anim_save("/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/mills_timeline.mp4"
          , animated_map_output)


# 06 : Word Cloud Map of Existing Mills' Function ------------------------------

   # +-+-+-+-+ +-+-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+ +-+-+-+-+-+-+-+-+ #
   # |W|o|r|d| |C|l|o|u|d| |M|a|p| |o|f| |M|i|l|l|s|'| |F|u|n|c|t|i|o|n| #
   # +-+-+-+-+ +-+-+-+-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+ +-+-+-+-+-+-+-+-+ #


# بیان مسئله باید عوض شود ...می خواهیم لیبلگذاری کنیم با این شرط که عناوین مشابه اجماع شوند و سایزوشن بزرگتر شود
# Ensure your dataset (mills) is loaded as an sf object and projected correctly
mills <- st_transform(mills, crs = 3857)  # Transform to a projected CRS for spatial visualization
head(mills)
# Check the unique mill dunctions
unique(mills$FUNCTIE)
# Check the unique mill types
unique(mills$TYPE)

# Load the Netherlands boundary shapefile (assumed to be a shapefile)
netherlands_shapefile <- st_read("00-Mills Memory/data/shp/gadm41_NLD_0.shp")  # Adjust path accordingly
netherlands <- st_transform(netherlands_shapefile, crs = 3857)  # Transform to the same CRS

# # Generate the frequency of each mill function
# head(mills)
# mill_function_freq <- mills %>%
#   count(FUNCTIE) %>%
#   arrange(desc(n)) %>%
#   rename(words = FUNCTIE, freq = n)

# Generate the frequency of each mill function
head(mills)
mill_function_freq <- mills %>%
  count(TYPE) %>%
  arrange(desc(n)) %>%
  rename(words = TYPE, freq = n)

my_colors <- c("#993404", "#fec44f")  # Shades of blue

# Create a word cloud (first as an HTML file)
wordcloud_html <- wordcloud2(data = mill_function_freq, 
                             size = 0.5,  # Smaller font size to fit more words
                             color = my_colors, 
                             backgroundColor = "white" ,
                             rotateRatio = 0 ) # Prevent rotation of words

# Save the word cloud as HTML
html_file <- "/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/wordcloud.html"
htmlwidgets::saveWidget(wordcloud_html, file = html_file, selfcontained = TRUE)

# Convert the HTML to PNG
png_file <- "/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/wordcloud.png"
webshot(html_file, file = png_file, vwidth = 800, vheight = 600)  # Larger width and height

# Read the PNG image into R
wordcloud_image <- png::readPNG(png_file)

# Create a plot where the word cloud is surrounded by the Netherlands border
wordcloud_map <- ggplot() +
  annotation_raster(wordcloud_image, xmin = st_bbox(netherlands)[1], xmax = st_bbox(netherlands)[3],  # Stretch the word cloud to Netherlands boundary
                    ymin = st_bbox(netherlands)[2], ymax = st_bbox(netherlands)[4]) +
  geom_sf(data = netherlands, fill = "black", color = "black", alpha = 0) +  # Add Netherlands boundary (border)
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills' Memory",
    subtitle = "▪Typology Existing Mills in Netherlands",
    caption = "▪ Data Source: www.molendatabase.org | Map visualization by Massoud Ghaderian | R Studio | 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14, margin = margin(t = 0)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),  # Optional: Add background and border to legend
    legend.text = element_text(size = 10),  # Increase size to 10 (adjust as needed)
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),  # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),  # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),  # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "black"),# Choose a style for the north arrow
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.5, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )+
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank())  # Remove axis and legend

wordcloud_map

# Save the final combined plot with the word cloud surrounded by the Netherlands boundary
ggsave("Mills WordCloud .jpg", plot = wordcloud_map, 
       width = 8.27, height = 12, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/00-Mills Memory/outputs/")

# 07  : 3D Map of Existing Mills on DSM  ---------------------------------------

  #    +-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+-+-+ +-+-+ +-+-+-+   #
  #    |3|D| |M|a|p| |o|f| |E|x|i|s|t|i|n|g| |M|i|l|l|s| |o|n| |D|S|M|   #
  #    +-+-+ +-+-+-+ +-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+-+-+ +-+-+ +-+-+-+   #
  
library(terra)
library(sf)
library(rayshader)

# Load the mills data
mills <- st_read("00-Mills Memory/data/shp/Molens.shp")

# Get the bounding box of the mills (expand it slightly for better context)
mills_bbox <- st_bbox(mills)
expanded_bbox <- mills_bbox + c(-0.01, -0.01, 0.01, 0.01)  # Expand by 0.01 degrees

# WCS API call to fetch DSM for the bounding box
wcs_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"
dsm <- rast(paste0(
  wcs_url,
  "?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage",
  "&COVERAGEID=layer_name",  # Replace with specific layer name
  "&FORMAT=image/tiff",
  "&SUBSET=Lat(", expanded_bbox["ymin"], ",", expanded_bbox["ymax"], ")",
  "&SUBSET=Long(", expanded_bbox["xmin"], ",", expanded_bbox["xmax"], ")"
))

# Convert DSM raster to a matrix for rayshader
dsm_matrix <- as.matrix(dsm, wide = TRUE)

# Render the 3D map
dsm_matrix %>%
  rayshader::height_shade() %>%
  rayshader::plot_3d(heightmap = ., zscale = 10, windowsize = c(1000, 800))

# Overlay mills (optional: convert coordinates to match DSM matrix)
mills_coords <- st_coordinates(mills)
points3d(mills_coords[, 1], mills_coords[, 2], col = "blue", size = 5)

# Adjust the view
rayshader::render_camera(theta = 45, phi = 30, zoom = 0.8)



# 06  : existing mill on DSM in Amsterdam -------------------------------------
# Load required libraries
library(leaflet)
library(sf)
library(raster)

# Define the WMS URL for DSM
wms_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"

# Load mill locations (replace with your file path)
mills_shapefile <- st_read("00-Mills Memory/data/shp/Molens.shp")

# Check the structure of the shapefile (optional)
head(mills_shapefile)

# Transform the CRS to match WGS 84 (EPSG:4326)
mills_shapefile <- st_transform(mills_shapefile, crs = 4326)

# Verify mill locations by plotting them (optional)
plot(st_geometry(mills_shapefile))

# Define the bounding box for Amsterdam area (this will not be passed directly to the WMS)
bbox_amsterdam <- c(4.75, 52.25, 5.05, 52.45)  # Approximate bbox for Amsterdam

# Create a leaflet map
leaflet() %>%
  # Add WMS basemap for DSM using the defined parameters
  addWMSTiles(
    wms_url,
    layers = "ahn3_05m_dsm",  # Ensure this layer name is correct
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE
    )
  ) %>%
  # Add existing mills as points (use the correct CRS)
  addCircleMarkers(
    data = mills_shapefile,
    color = "red",
    radius = 5,
    popup = ~NAAM,  # Pop-up with the name of the mill
    fillOpacity = 0.8
  ) %>%
  # Customize map view (adjust zoom level and center to Amsterdam)
  setView(lng = 4.9041, lat = 52.3676, zoom = 12)  # Centered on Amsterdam

