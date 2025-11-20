# Install once if needed:
# install.packages(c("sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(magick)
library(cowplot)
library(grid)

# Turn off s2 processing to avoid geometry errors
sf_use_s2(FALSE)

# -------------------------------------------------
# 1) Read your GeoJSON files
#    (put this script in the same folder as the .geojson files,
#     or adjust the paths below, e.g. "data/Tobacco_Heritage.geojson")
# -------------------------------------------------

barns <- st_read("Tobacco_Heritage.geojson", quiet = TRUE)
regions <- st_read("regions.geojson", quiet = TRUE)

# Check CRS (coordinate reference system)
st_crs(barns)
st_crs(regions)

# -------------------------------------------------
# 2) Get Netherlands outline (for context)
# -------------------------------------------------
nl <- ne_countries(
    scale = "medium", # nolint
    country = "Netherlands",
    returnclass = "sf"
)

# Make sure everything is in the same CRS (EPSG:4326)
target_crs <- 4326
barns <- st_transform(barns, target_crs)
regions <- st_transform(regions, target_crs)
nl <- st_transform(nl, target_crs)

# Get populated places (cities) for Netherlands - use larger area
cities <- ne_download(scale = "large", type = "populated_places", returnclass = "sf")
cities <- st_transform(cities, target_crs)

# Expand search area to include nearby cities
cities_filtered <- cities %>%
    filter(ADM0NAME == "Netherlands") %>%
    filter(
        st_coordinates(.)[, 1] >= 4.8 & st_coordinates(.)[, 1] <= 5.8 &
            st_coordinates(.)[, 2] >= 51.7 & st_coordinates(.)[, 2] <= 52.2
    ) %>%
    filter(POP_MAX > 5000) # Only cities with population > 5000

# Print cities found
cat(sprintf("Found %d cities in the area\n", nrow(cities_filtered)))

# Load municipalities (gemeenten) data
municipalities_url <- "https://cartomap.github.io/nl/wgs84/gemeente_2025.geojson"
municipalities <- st_read(municipalities_url, quiet = TRUE)
municipalities <- st_transform(municipalities, target_crs)

# Filter municipalities to map extent
municipalities_filtered <- municipalities %>%
    st_filter(st_as_sfc(st_bbox(c(xmin = 5.35, xmax = 5.65, ymin = 51.90, ymax = 52.07), crs = target_crs)))

# -------------------------------------------------
# 3) Define colors & shapes (similar to your web map)
# -------------------------------------------------
type_colors <- c(
    "Tabaksschuur"   = "#000000", # barns - black # nolint: indentation_linter.
    "Sigarenfabriek" = "#8B4513", # cigar factory - brown
    "Tabakshofstede" = "#228B22", # farmstead - green
    "Tabakspakhuis"  = "#4B0082" # warehouse - indigo
)

type_shapes <- c(
    "Tabaksschuur"   = 17, # triangle
    "Sigarenfabriek" = 15, # square
    "Tabakshofstede" = 16, # filled circle
    "Tabakspakhuis"  = 18 # diamond
)

# Make sure Type is a factor with a fixed order
barns$Type <- factor(barns$Type,
    levels = names(type_colors)
)

# -------------------------------------------------
# 4) Build the map with ggplot2
# -------------------------------------------------
p <- ggplot() +
    # Netherlands background
    geom_sf(
        data = nl,
        fill = "grey98",
        color = "grey60",
        linewidth = 0.5
    ) +

    # Municipality boundaries
    geom_sf(
        data = municipalities_filtered,
        fill = NA,
        color = "grey50",
        linewidth = 0.3,
        linetype = "dashed"
    ) +

    # Municipality labels
    geom_sf_text(
        data = municipalities_filtered,
        aes(label = statnaam),
        size = 2.5,
        color = "grey40",
        fontface = "plain",
        alpha = 0.7
    ) +

    # Cities/towns as small points
    geom_sf(
        data = cities_filtered,
        color = "grey30",
        size = 1,
        alpha = 0.6
    ) +

    # City labels
    geom_sf_text(
        data = cities_filtered,
        aes(label = NAME),
        size = 2.5,
        color = "grey20",
        nudge_y = 0.008,
        fontface = "italic"
    ) +

    # Tobacco regions outline (red like in your web map)
    geom_sf(
        data = regions,
        fill = NA,
        color = "#b10000",
        linewidth = 0.8
    ) +

    # Region labels
    geom_sf_text(
        data = regions,
        aes(label = name),
        size = 3.5,
        fontface = "bold",
        color = "#b10000",
        nudge_y = -0.018,
        vjust = 1
    ) +

    # Tobacco heritage points
    geom_sf(
        data = barns,
        aes(color = Type, shape = Type),
        size = 2.5,
        stroke = 0.3
    ) +
    scale_color_manual(
        values = type_colors,
        name   = "Heritage type"
    ) +
    scale_shape_manual(
        values = type_shapes,
        name   = "Heritage type"
    ) +
    coord_sf(
        xlim = c(5.40, 5.60),
        ylim = c(51.93, 52.04),
        expand = TRUE
    ) +
    labs(
        title    = "Tobacco Heritage in the Netherlands",
        subtitle = "Locations of tobacco barns and related heritage",
        caption  = "Data: Tobacco_Heritage.geojson & regions.geojson"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"),
        panel.grid.minor = element_blank(),
        axis.title       = element_blank(),
        legend.position  = "right",
        plot.title       = element_text(face = "bold", size = 16),
        plot.subtitle    = element_text(size = 11),
        plot.caption     = element_text(size = 8)
    )

# -------------------------------------------------
# 5) Add Rabanism Logo
# -------------------------------------------------
# Read and convert logo to raster
rbanism_logo <- image_read("https://rbanism.org/assets/imgs/about/vi_l.jpg")
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)

# Combine main plot with logo using cowplot
final_plot <- ggdraw(p) +
    draw_grob(rbanism_logo_raster, x = 0.80, y = 0.70, width = 0.20, height = 0.20)

# Show the map in VS Code / R
print(final_plot)

# -------------------------------------------------
# 6) Export as PNG image
# -------------------------------------------------
cat("Saving plot to tobacco_heritage_netherlands.png...\n")
ggsave(
    filename = "tobacco_heritage_netherlands.png",
    plot     = final_plot,
    width    = 8, # inches
    height   = 6,
    dpi      = 300,
    device   = "png"
)
cat("Plot saved successfully!\n")
