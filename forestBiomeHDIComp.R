##############################
## 0. Load Necessary Libraries
##############################

library(rnaturalearth)     # for country boundaries
library(rnaturalearthdata) # comes with 'rnaturalearth'
library(dplyr)
library(ggplot2)
library(sf)
library(countrycode)
library(ggrepel)



##############################
## 1. Example Data Frames
##############################

# Suppose your data frames look like this:
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets")

df_forest_area <- read.csv("forest_biome_area_by_country.csv")

df_forest <- data.frame(
  ISO3 = df_forest_area$Country,
  forest_area = df_forest_area$total_area,
  forest_prop = df_forest_area$area_prop
)

df_pub <- read.csv("pubProportionForestPost2010s.csv")
df_pub <- data.frame(
  ISO3 = df_pub$sepISO3,
  pub_prop = df_pub$proportion
)

df_hdi <- read.csv("UNHDIValuesWithISO3.csv")
df_hdi <- data.frame(
  ISO3 = df_hdi$iso3,
  HDI  = df_hdi$HDI
)



##############################
## 2. Get Country Boundaries 
##    + Land Area from Natural Earth
##############################

# Load Natural Earth Admin-0 country polygons 
# scale = "small" => 1:110m, "medium" => 1:50m, "large" => 1:10m
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Check the available attribute columns
# colnames(world)

# 'world$ADM0_A3' often has the ISO3 codes
# 'world$AREA' might be area in sq. km (depends on scale). 
# Alternatively, you can compute area from the geometry.

##############################
## 3. Prepare a Data Frame of 
##    Land Area by ISO3
##############################

# Method A: Use the 'AREA' attribute from Natural Earth 
#           (approx area in sq. km, but might be somewhat coarse)
# Or Method B: Compute exact area from geometry via st_area().

# Let's do Method B for better accuracy:

world <- st_make_valid(world)  # ensure no geometry issues
# Project to an equal-area projection for more accurate area calculations
# (e.g., Robinson or Eckert IV)
world_equal_area <- st_transform(world, crs = "+proj=eck4")

world_equal_area$land_area_km2 <- as.numeric(st_area(world_equal_area) / 10^6)

# Create a simplified data frame with ISO3 and land area
df_area <- world_equal_area %>%
  dplyr::select(iso_a3, land_area_km2) %>%
  st_drop_geometry()

df_area$ISO3 <- df_area$iso_a3

df_area <- df_area %>% select(ISO3, land_area_km2)
# Now df_area has columns: "ISO3", "land_area_km2"

##############################
## 4. Merge All Data 
##    (forest_prop, pub_prop, HDI, land_area_km2)
##############################

df_merged <- df_forest %>%
  left_join(df_pub,  by = "ISO3") %>%
  left_join(df_hdi,  by = "ISO3") %>%
  left_join(df_area, by = "ISO3")

# Check the merged data
df_merged
#     ISO3 forest_prop pub_prop   HDI land_area_km2
# 1    BRA        0.20    0.250 0.754       8358405  (example)
# 2    IND        0.05    0.030 0.645       2973193
# ... etc.

##############################
## 5. Filter Out Very Small Countries
##############################

# You can define a threshold for total land area in km2
# For instance, exclude countries with < 50,000 km^2 
# (Roughly smaller than e.g., Costa Rica / Slovakia).
# Adjust the threshold as needed.

threshold_km2 <- 1000

df_filtered <- df_merged %>%
  filter(land_area_km2 >= threshold_km2)

forest_area_threshold <- 0.1

df_filtered <- df_filtered %>%
  filter(forest_prop >= forest_area_threshold)

# This excludes tiny states like TTO (Trinidad & Tobago) 
# if it is below that threshold, etc.

##############################
## 6. Create a Plot
##    Compare Forest Proportion vs. Publication Proportion
##    with HDI as Color
##############################

##############################
## 7. (Optional) Look at Disparity
##############################

# If you want to see difference = pub_prop - forest_prop 
# or ratio = pub_prop / forest_prop, you can do:

df_filtered <- df_filtered %>%
  mutate(
    disparity = pub_prop - forest_prop,
    ratio     = ifelse(forest_prop > 0, pub_prop / forest_prop, NA)
  )

# Suppose df_filtered has a column "ISO3"
df_filtered$continent <- countrycode(
  sourcevar   = df_filtered$ISO3,
  origin      = "iso3c",
  destination = "continent"
)



df_filtered <- df_filtered %>% mutate(
  continent_merged = ifelse(continent == "Asia" | continent == "Oceania", "Asia/Oceania", continent)
)




# Join your data to the sf object
world_joined <- world %>%
  left_join(df_filtered, by = c("iso_a3" = "ISO3"))

forest_disparityMap <- ggplot(world_joined) +
  geom_sf(aes(fill = disparity), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_gradient2(
    low = "red",      # Set color for low difference values (indicating reduced participation)
    mid = "white",    # Set color for mid-point (0, indicating no change)
    high = "blue",    # Set color for high difference values (indicating increased participation)
    midpoint = 0,     # Define midpoint at 0 for the gradient
    name = "Publication -\n Forest area \n proportion",  # Label for the fill legend
    na.value = "grey50"  # Color for NA values
  ) 

forest_ratioMap <- ggplot(world_joined) +
  geom_sf(aes(fill = log(ratio)), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_gradient2(   # log transform the color scale
    low = "red",         # color for lower ratio
    mid = "white",       # color for ratio=1
    high = "blue",       # color for higher ratio
    midpoint = 0,        # the pivot in data space
    na.value = "grey50", # color for missing data
    name = "Publications : \n Forest area ratio",
    breaks = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)),
    labels = c("0.1", "0.5", "1", "2", "5", "10")
  ) + ggtitle("Forest")+
  theme(legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), # Removes the legend
        plot.title = element_text(hjust=0.5))


world_joined$forest_share_national <- world_joined$forest_area/world_joined$land_area_km2

# Example structure
# df <- data.frame(
#   ISO3 = c("BRA","IND","TTO","USA","CHN","FIN","MYS"),
#   forest_share_global = c(0.22,0.08,0.0005,0.02,0.09,0.003,0.01),
#   forest_share_national = c(0.6,0.21,0.4,0.33,0.25,0.73,0.68),
#   pub_ratio = c(1.2,0.8,2.5,0.5,1.7,1.0,0.9),
#   HDI = c(0.754,0.645,0.799,0.921,0.768,0.938,0.810)
# )
disp_cor_test <- cor.test(df_filtered$HDI, df_filtered$disparity, method = "spearman")
disp_r_val = round(disp_cor_test$estimate, 3)
disp_p_val = round(disp_cor_test$p.value,3)
disp_cor_label <- paste0(
  "R = ", disp_r_val,
  ", p = ", disp_p_val
)

ratio_cor_test <- cor.test(df_filtered$HDI, log(df_filtered$ratio), method = "spearman")
ratio_r_val = round(ratio_cor_test$estimate, 3)
ratio_p_val = round(ratio_cor_test$p.value,3)
ratio_cor_label <- paste0(
  "R = ", ratio_r_val,
  ", p = ", ratio_p_val
)


forest_disp_plot <- ggplot(df_filtered, aes(
  x = HDI,
  y = disparity,
  color = disparity
)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(size = forest_prop), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Global forest\nshare")  +
  scale_color_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, name = "Disparity"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Human Development Index",
    y = "Publication share - Global forest area share"
  ) + annotate("text", x = 0.3, y = 4, label = disp_cor_label, hjust = 0)

forest_ratio_plot <- ggplot(df_filtered, aes(
  x = HDI,
  y = log(ratio),         # plotting log(ratio)
  color = log(ratio),     # coloring by log(ratio)
)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +  # ratio=1 line in log space
  geom_point(aes(size = forest_prop), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Global forest\nshare")  +
  
  # 2) Force y-axis to display ratio ticks (0.1, 1, 10, etc.) even though data is in log-space
  scale_y_continuous(
    name = "Publication / Forest area ratio",
    breaks = log(c(0.1, 0.5, 1, 2, 5, 10)),  # log-space breaks
    labels = c("0.1", "0.5", "1", "2", "5", "10")  # normal ratio labels
  ) +
  
  # 3) Control color scale similarly, giving breaks in log space, labels in normal ratio
  scale_color_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,   # 0 in log-space => ratio=1
    name = "Ratio",
    
    # For the legend:
    breaks = log(c(0.1, 0.5, 1, 2, 5, 10)),  # log-space
    labels = c("0.1", "0.5", "1", "2", "5", "10")  # normal scale
  ) +
  
  theme_minimal(base_size = 14) +
  
  labs(
    x = "Human Development Index"
    # y is already defined in scale_y_continuous
  )+
  annotate("text", x = 0.3, y = 2, label = ratio_cor_label, hjust = 0)


df_filtered <- df_filtered %>%
  mutate(top_forest_flag = ifelse(forest_prop > 1.390163, TRUE, FALSE)) # threshold at 2%, e.g.



# Basic scatter with a regression line
forest_disparity_corr_plot <- ggplot(df_filtered, aes(x = HDI, y = disparity)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(aes(size = forest_prop, color = top_forest_flag), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Global forest\nshare") +
  scale_color_manual(
    values = c("FALSE" = "grey50", "TRUE" = "red"),
    name = "Top 15 countries \n by forest area?"
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    x = "HDI",
    y = "Publication share -\n Global forest area share"
  ) +
  annotate("text", x = 0.3, y = 4, label = disp_cor_label, hjust = 0)


forest_ratio_corr_plot <- ggplot(df_filtered, aes(
  x = HDI,
  y = log(ratio),         # plotting log(ratio)
)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +  # ratio=1 line in log space
  geom_point(aes(size = forest_prop,color = top_forest_flag), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Global forest\nshare")  +
  scale_color_manual(
    values = c("FALSE" = "grey50", "TRUE" = "#F5C710"),
    name = "Top 15 countries \n by forest area?"
  )+
  # 2) Force y-axis to display ratio ticks (0.1, 1, 10, etc.) even though data is in log-space
  scale_y_continuous(
    name = "Publication / Forest area ratio",
    breaks = log(c(0.1, 0.5, 1, 2, 5, 10)),  # log-space breaks
    labels = c("0.1", "0.5", "1", "2", "5", "10")  # normal ratio labels
  )  +
  geom_text_repel(
    data = df_filtered %>% filter(top_forest_flag == TRUE),
    aes(label = ISO3),
    size = 3,
    # Optional: you can adjust repel behavior
    max.overlaps = 30
  ) +theme_minimal(base_size = 14) +
  labs(
    x = "HDI",
    y = "Publication share /\n Global forest area share"
  ) + theme(legend.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))  +
  annotate("text", x = 0.3, y = 3, label = ratio_cor_label, hjust = 0)


# 1) Filter top 15
top_forest <- world_joined %>%
  arrange(desc(forest_prop)) %>%
  slice_head(n = 15)



df_bar_plot_width_adj <- top_forest %>%
  # Sort by HDI (ascending)
  arrange(HDI) %>%
  
  # Factor the country column so ggplot uses that sorted order
  mutate(country = factor(name, levels = name)) %>%
  
  # Scale forest share so the max forest holder => width=1
  mutate(width_scaled = log(forest_prop) / max(log(forest_prop)))

forest_bar_plot <- ggplot(df_bar_plot_width_adj, aes(x = country, y = HDI)) +
  geom_col(
    aes(fill = disparity, width = width_scaled),
    # position_identity ensures each bar uses the raw 'width' 
    # without spacing them evenly
    position = position_identity()
  ) +
  coord_flip() +
  # Diverging color scale for disparity
  scale_fill_gradient2(
    low = "red", mid = 'white', high = "blue",
    midpoint = 0, name = "Disparity\n(publication share - \nglobal forest share)"
  ) +
  labs(
    y = "HDI",
    x = "Country",
    subtitle = "Bar thickness = Proportion of global forest area"
  ) +
  theme_minimal(base_size = 14)

forest_ratio_bar_plot <- ggplot(df_bar_plot_width_adj, aes(x = country, y = HDI)) +
  geom_col(
    aes(fill = ratio, width = width_scaled),
    position = position_identity()
  ) +
  coord_flip() +
  scale_fill_gradient2(
    trans = "log",  
    breaks = c(0.2,1,4),
    labels = c("0.2","1", "4"),# Another log-like transform for negative/positive
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Ratio\n(pub share / \nglobal forest share)"
  ) +labs(
    y = "HDI",
    x = "Country",
    subtitle = "Bar thickness = Proportion of global forest area"
  )+
  theme_minimal()+theme(legend.text = element_text(size = 14),
                        axis.title.x = element_text(size = 14),
                        axis.title.y = element_text(size = 14),
                        axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 12)) 


