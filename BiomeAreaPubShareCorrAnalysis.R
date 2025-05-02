##############################
## 0. Load Libraries
##############################

library(rnaturalearth)     
library(rnaturalearthdata) 
library(dplyr)
library(ggplot2)
library(sf)
library(countrycode)
library(ggrepel)
library(scales)        # for custom transforms if needed, e.g. asinh_trans
library(patchwork)     # for arranging multiple plots if desired

##############################
## 1. Read Input CSVs
##############################
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets")
# 1A. Savanna
df_savanna_area <- read.csv("grassy_biome_area_by_country.csv")
df_savanna_pub  <- read.csv("pubProportionGrassyPost2010s.csv")

df_savanna_area <- data.frame(
  ISO3          = df_savanna_area$Country,    # or adapt col name
  savanna_area  = df_savanna_area$total_area, # total absolute area?
  savanna_prop  = df_savanna_area$area_prop   # fraction of global savanna?
)

df_savanna_pub <- data.frame(
  ISO3     = df_savanna_pub$sepISO3,
  pub_prop = df_savanna_pub$proportion
)

# 1B. Forest
df_forest_area <- read.csv("forest_biome_area_by_country.csv")
df_forest_pub  <- read.csv("pubProportionForestPost2010s.csv")

df_forest_area <- data.frame(
  ISO3         = df_forest_area$Country,
  forest_area  = df_forest_area$total_area,
  forest_prop  = df_forest_area$area_prop
)

df_forest_pub <- data.frame(
  ISO3     = df_forest_pub$sepISO3,
  pub_prop = df_forest_pub$proportion
)

# 1C. HDI
df_hdi <- read.csv("UNHDIValuesWithISO3.csv")
df_hdi <- data.frame(
  ISO3 = df_hdi$iso3,
  HDI  = df_hdi$HDI
)

##############################
## 2. World Shapefile + Land Area
##############################

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- st_make_valid(world)
world_equal_area <- st_transform(world, crs = "+proj=eck4")
world_equal_area$land_area_km2 <- as.numeric(st_area(world_equal_area) / 1e6)

world_equal_area$gdp_per_capita <- (world_equal_area$gdp_md*1000000)/world_equal_area$pop_est

df_area <- world_equal_area %>%
  dplyr::select(name, iso_a3, land_area_km2, pop_est, gdp_md, gdp_per_capita) %>%
  st_drop_geometry() %>%
  rename(ISO3 = iso_a3)

prepare_biome_data <- function(
    area_df,     # e.g. df_savanna_area or df_forest_area
    pub_df,      # e.g. df_savanna_pub or df_forest_pub
    hdi_df,      # df_hdi
    world_area   # df_area
) {
  # Merge
  df_merged <- area_df %>%
    left_join(pub_df,   by = "ISO3", suffix = c("_area","_pub")) %>%
    left_join(hdi_df,   by = "ISO3") %>%
    left_join(world_area, by = "ISO3")
  
  # Filter out NAs if needed, or small countries
  df_filtered <- df_merged %>%
    filter(!is.na(pub_prop), 
           land_area_km2 > 1e3)  # remove microstates under 1000 km2
  
  # Compute ratio, disparity
  # 'x_area' = either 'savanna_area' or 'forest_area'
  # 'x_prop' = either 'savanna_prop' or 'forest_prop'
  # We'll guess the columns are named ???. Let's detect them:
  biome_col <- setdiff(names(area_df), c("ISO3", "total_area", "area_prop"))
  # The user may have "savanna_area" / "savanna_prop" or "forest_area" / "forest_prop"
  # but let's do a simpler approach by direct naming:
  
  # if 'savanna_prop' in names(df_filtered), use that, else 'forest_prop'
  if ("savanna_prop" %in% names(df_filtered)) {
    df_filtered <- df_filtered %>%
      mutate(
        disparity = pub_prop - savanna_prop,
        ratio     = ifelse(savanna_prop > 0, pub_prop / savanna_prop, NA)
      )
  } else {
    # assume forest
    df_filtered <- df_filtered %>%
      mutate(
        disparity = pub_prop - forest_prop,
        ratio     = ifelse(forest_prop > 0, pub_prop / forest_prop, NA)
      )
  }
  
  df_filtered
}

df_savanna <- prepare_biome_data(df_savanna_area, df_savanna_pub, df_hdi, df_area)
df_forest  <- prepare_biome_data(df_forest_area,  df_forest_pub,  df_hdi, df_area)

all_land_threshold_km2 <- 1000

df_savanna <- df_savanna %>%
  filter(land_area_km2 >= all_land_threshold_km2)

df_forest <- df_forest %>%
  filter(land_area_km2 >= all_land_threshold_km2)

biome_area_threshold <- 0.1

df_savanna <- df_savanna %>%
  filter(savanna_prop >= biome_area_threshold)

df_forest <- df_forest %>%
  filter(forest_prop >= biome_area_threshold)

# For savanna
with(df_savanna, cor.test(savanna_prop, pub_prop, method="spearman"))

# For forest
with(df_forest, cor.test(forest_prop, pub_prop, method="spearman"))

sav_area_pub_cor <- cor.test(df_savanna$savanna_prop, df_savanna$pub_prop, method="spearman")
cat("Savanna area vs. pub: Spearman rho=", sav_area_pub_cor$estimate, ", p=", sav_area_pub_cor$p.value, "\n")

forest_area_pub_cor <- cor.test(df_forest$forest_prop, df_forest$pub_prop, method="spearman")
cat("Forest area vs. pub: Spearman rho=", forest_area_pub_cor$estimate, ", p=", forest_area_pub_cor$p.value, "\n")

library(dplyr)
library(ggplot2)
library(ggrepel)


plot_log_area_vs_log_pubprop_with_top15 <- function(
    df,
    area_col  = "savanna_prop",  # x variable (must be >0)
    ratio_col = "pub_prop",      # y variable (must be >0)
    iso_col   = "ISO3",
    biome_name= "Savanna",
    top_n     = 15,
    x_breaks  = c(0, 5, 10, 25),  # normal-scale x-axis breaks
    y_breaks  = c(0.1, 0.5, 1, 2, 5, 10)        # normal-scale y-axis breaks
) {
  # 1) Filter to ensure area>0, ratio>0
  df_sub <- df %>%
    filter(!is.na(.data[[area_col]]),
           !is.na(.data[[ratio_col]]),
           .data[[area_col]] > 0,
           .data[[ratio_col]] > 0)
  
  # 2) Identify top_n countries by area_col
  df_sub <- df_sub %>%
    arrange(desc(.data[[area_col]])) %>%
    mutate(is_top = row_number() <= top_n)
  
  # ========== CORRELATION: ALL COUNTRIES ==========
  # log-transform for correlation
  cor_x_all <- log(df_sub[[area_col]])
  cor_y_all <- log(df_sub[[ratio_col]])
  
  cor_out_all <- cor.test(cor_x_all, cor_y_all, method="spearman")
  rho_all <- round(unname(cor_out_all$estimate), 3)
  p_all   <- formatC(unname(cor_out_all$p.value), format="f", digits=3)
  cor_label_all <- paste0("All: ρ=", rho_all, ", p=", p_all)
  
  # ========== CORRELATION: TOP 15 COUNTRIES ==========
  df_top15 <- df_sub %>% filter(is_top == TRUE)
  if(nrow(df_top15) >= 3) {
    cor_x_top <- log(df_top15[[area_col]])
    cor_y_top <- log(df_top15[[ratio_col]])
    
    cor_out_top <- cor.test(cor_x_top, cor_y_top, method="spearman")
    rho_top  <- round(unname(cor_out_top$estimate), 3)
    p_top    <- formatC(unname(cor_out_top$p.value), format="f", digits=3)
    cor_label_top <- paste0("Top ", top_n, ": ρ=", rho_top, ", p=", p_top)
  } else {
    cor_label_top <- paste0("Top ", top_n, ": not enough data")
  }
  
  # 3) Create plotting columns in log-space
  df_sub <- df_sub %>%
    mutate(
      plot_x = log(.data[[area_col]]),
      plot_y = log(.data[[ratio_col]])
    )
  df_top15 <- df_top15 %>%
    mutate(
      plot_x = log(.data[[area_col]]),
      plot_y = log(.data[[ratio_col]])
    )
  # 4) Axis label
  x_label <- paste0(biome_name, " area proportion")
  y_label <- "Publication proportion"
  
  # 5) Determine annotation coords
  x_min <- min(df_sub$plot_x, na.rm=TRUE)
  x_max <- max(df_sub$plot_x, na.rm=TRUE)
  y_min <- min(df_sub$plot_y, na.rm=TRUE)
  y_max <- max(df_sub$plot_y, na.rm=TRUE)
  
  ann_x <- x_min + 0.05*(x_max - x_min)
  ann_y1 <- y_max - 0.05*(y_max - y_min) # for all countries
  ann_y2 <- y_max - 0.15*(y_max - y_min) # for top 15
  
  # 6) Build the ggplot
  p <- ggplot(df_sub, aes(x = plot_x, y = plot_y)) +
    # (a) Smoothing line for all countries
    geom_smooth(method="lm", se=FALSE, color="black", linetype="dashed") +
    
    # (b) Smoothing line for top 15
    geom_smooth(
      data = df_top15,
      aes(x=plot_x, y=plot_y),
      method="lm", se=FALSE, color="darkcyan"
    ) +
    
    # (c) Points: highlight top 15 in color, others in grey
    geom_point(
      aes(color=ifelse(is_top, "Top 15", "Other")),
      size=3, alpha=0.7
    ) +
    scale_color_manual(
      values = c("Top 15"="#F5C710","Other"="grey50"),
      name=paste0("Top ", top_n," by ", biome_name," area?")
    ) +
    
    # (d) Label top 15 countries
    geom_text_repel(
      data = df_sub %>% filter(is_top),
      aes(label=.data[[iso_col]]),
      size=3, max.overlaps=30
    ) +
    
    # (e) X-axis in log scale but normal numeric tick labels
    scale_x_continuous(
      breaks = log(x_breaks),
      labels = as.character(x_breaks),
      name = x_label
    ) +
    
    # (f) Y-axis in log scale but normal numeric tick labels
    scale_y_continuous(
      breaks = log(y_breaks),
      labels = as.character(y_breaks),
      name = y_label
    ) +
    
    # (g) Correlation annotations
    annotate("text", x=ann_x, y=ann_y1, label=cor_label_all, hjust=0, size=4, color="black") +
    annotate("text", x=ann_x, y=ann_y2, label=cor_label_top, hjust=0, size=4, color="darkcyan") +
    
    theme_minimal(base_size=14)
  
  p
}

plot_area_vs_log_pubprop <- function(
    df,
    area_col  = "savanna_prop",  # or "forest_prop"
    ratio_col = "pub_prop",         # must be a positive numeric column
    iso_col   = "ISO3",
    biome_name= "Savanna",
    top_n     = 15
) {
  # 1) Filter to ensure area>0, ratio>0
  df_sub <- df %>%
    filter(!is.na(.data[[area_col]]),
           !is.na(.data[[ratio_col]]),
           .data[[area_col]] > 0,
           .data[[ratio_col]] > 0)
  
  # 2) Identify top_n countries by area_col
  df_sub <- df_sub %>%
    arrange(desc(.data[[area_col]])) %>%
    mutate(is_top = row_number() <= top_n)
  
  # 3) Compute Spearman correlation on (x=area_col, y=log(ratio_col))
  cor_data <- df_sub %>%
    filter(.data[[area_col]] > 0, .data[[ratio_col]] > 0)
  
  corr <- cor.test(log(cor_data[[area_col]]), log(cor_data[[ratio_col]]), method="spearman")
  rho_val <- round(corr$estimate, 3)
  p_val   <- formatC(corr$p.value, format="f", digits=3)
  cor_label <- paste0("rho=", rho_val, ", p=", p_val)
  
  # 4) Determine axis ranges for annotation
  x_min <- min(df_sub[[area_col]], na.rm=TRUE)
  x_max <- max(df_sub[[area_col]], na.rm=TRUE)
  y_min <- min(log(df_sub[[ratio_col]]), na.rm=TRUE)
  y_max <- max(log(df_sub[[ratio_col]]), na.rm=TRUE)
  
  # We'll place the correlation label near the top-left
  ann_x <- x_min + 0.05*(x_max - x_min)
  ann_y <- y_max - 0.02*(y_max - y_min)
  
  # 5) Create the scatter: x=area, y=log(ratio)
  p <- ggplot(df_sub, aes(x = .data[[area_col]], y = log(.data[[ratio_col]]))) +
    #geom_function(fun = log, color = "grey50", linetype = "dashed") +
    geom_point(aes(color = is_top), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    
    # Color scale for top_n countries
    scale_color_manual(
      values = c("FALSE"="grey50","TRUE"="#F5C710"),
      name   = paste0("Top ", top_n," by ", biome_name," area?")
    ) +
    
    # Repel labels for top countries
    geom_text_repel(
      data = df_sub %>% filter(is_top == TRUE),
      aes(label = .data[[iso_col]]),
      size = 3,
      max.overlaps = 30
    ) +
    
    # Show correlation annotation
    annotate("text", x=ann_x, y=ann_y, label=cor_label, hjust=0, size=4) +
    
    # We'll label the y-axis in normal ratio units, so we create breaks on log scale
    scale_y_continuous(
      name = "Publication proportion",
      breaks = log(c(0.1, 0.5, 1, 2, 5, 10)),
      labels = c("0.1", "0.5", "1", "2", "5", "10")
    ) +
    
    labs(
      x = paste(biome_name, "area proportion")
    ) +
    theme_minimal(base_size=14)
  
  p
}

savanna_area_pub_plot <- plot_log_area_vs_log_pubprop_with_top15(
  df = df_savanna,
  area_col = "savanna_prop",
  ratio_col  = "pub_prop",
  iso_col  = "ISO3",
  biome_name = "Savanna",  # used in title/legend
  top_n = 15               # highlight top 15
)

savanna_area_pub_plot

forest_area_pub_plot <- plot_log_area_vs_log_pubprop_with_top15(
  df = df_forest,
  area_col = "forest_prop",
  ratio_col  = "pub_prop",
  iso_col  = "ISO3",
  biome_name = "Forest",  # used in title/legend
  top_n = 15               # highlight top 15
)

forest_area_pub_plot

plot_ratio_map <- function(df_biome, biome_name="Savanna") {
  # We'll assume 'ratio' is the column
  world_joined <- world %>%
    left_join(df_biome, by=c("iso_a3"="ISO3"))
  
  ggplot(world_joined) +
    geom_sf(aes(fill = log(ratio)), color=NA) +
    scale_fill_gradient2(
      low = "red", mid="white", high="blue", midpoint=0,
      # breaks in log scale, if desired
      breaks = log(c(0.1,0.5,1,2,5,10)),
      labels = c("0.1","0.5","1","2","5","10"),
      na.value="grey80",
      name = paste0("Pub/Area\n(", biome_name, ")")
    ) +
    coord_sf() +
    theme_void() 
}

savanna_ratio_map <- plot_ratio_map(df_savanna, "Savanna")
forest_ratio_map  <- plot_ratio_map(df_forest,  "Forest")


vars_of_interest <- c("HDI","pop_est","gdp_md")

spearman_log_cor <- function(data, xvar, yvar="ratio") {
  # Filter out zero or negative
  df_sub <- data %>% filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]]),
                            .data[[xvar]]>0, .data[[yvar]]>0)
  if(nrow(df_sub)<5) return(list(rho=NA, p=NA, n=nrow(df_sub)))
  
  # cor.test on log-log
  if(xvar == "HDI") {cor_out <- cor.test((df_sub[[xvar]]), log(df_sub[[yvar]]), method="spearman")}
  else {
    cor_out <- cor.test(log(df_sub[[xvar]]), log(df_sub[[yvar]]), method="spearman")
  }
  
  list(rho=cor_out$estimate, p=cor_out$p.value, n=nrow(df_sub))
}

# For savanna
sav_results <- lapply(vars_of_interest, function(var){
  spc <- spearman_log_cor(df_savanna, var, "ratio")
  data.frame(variable=var, rho=spc$rho, p.value=spc$p, n_obs=spc$n)
})
sav_results <- do.call(rbind, sav_results)
sav_results$biome <- "Savanna"

# For forest
forest_results <- lapply(vars_of_interest, function(var){
  spc <- spearman_log_cor(df_forest, var, "ratio")
  data.frame(variable=var, rho=spc$rho, p.value=spc$p, n_obs=spc$n)
})
forest_results <- do.call(rbind, forest_results)
forest_results$biome <- "Forest"

all_results <- rbind(sav_results, forest_results)
all_results

library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)  # for breaks/labels if needed

plot_area_vs_logratio_colored <- function(
    df,
    xvar        = "HDI",         # "HDI", or a numeric var like "gdp_md" or "pop_est"
    area_col    = "savanna_prop",# used for identifying top 15 by area
    ratio_col   = "ratio",       # ratio must be positive
    iso_col     = "ISO3",
    biome_name  = "Savanna",
    top_n       = 15
) {
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  
  # 1) Basic filtering
  df_sub <- df %>%
    filter(
      !is.na(.data[[xvar]]),
      !is.na(.data[[ratio_col]]),
      .data[[ratio_col]] > 0,
      !is.na(.data[[area_col]])
    )
  
  if(nrow(df_sub) < 5) {
    message("Not enough data to plot.")
    return(NULL)
  }
  
  # 2) Identify top_n countries by area_col
  df_sub <- df_sub %>%
    arrange(desc(.data[[area_col]])) %>%
    mutate(is_top = row_number() <= top_n)
  
  # =========== CORRELATION: ALL COUNTRIES ===========
  # We log-transform x if xvar != "HDI"
  if (xvar == "HDI") {
    cor_data_x <- df_sub[[xvar]]
  } else {
    cor_data_x <- log(df_sub[[xvar]])
  }
  cor_data_y <- log(df_sub[[ratio_col]])
  
  cor_out  <- cor.test(cor_data_x, cor_data_y, method="spearman")
  rho_val  <- round(unname(cor_out$estimate), 3)
  p_val    <- formatC(unname(cor_out$p.value), format="f", digits=3)
  cor_label  <- paste0("All: ρ=", rho_val, ", p=", p_val)
  
  # =========== CORRELATION: TOP 15 ONLY ===========
  df_top15 <- df_sub %>% filter(is_top == TRUE)
  
  if(nrow(df_top15) >= 3) {
    if (xvar == "HDI") {
      cor_top_x <- df_top15[[xvar]]
    } else {
      cor_top_x <- log(df_top15[[xvar]])
    }
    cor_top_y <- log(df_top15[[ratio_col]])
    
    cor_top_out <- cor.test(cor_top_x, cor_top_y, method="spearman")
    rho_top  <- round(unname(cor_top_out$estimate), 3)
    p_top    <- formatC(unname(cor_top_out$p.value), format="f", digits=3)
    cor_label_top <- paste0("Top ", top_n, ": ρ=", rho_top, ", p=", p_top)
  } else {
    cor_label_top <- paste0("Top ", top_n, ": not enough data")
  }
  
  # 3) Prepare plotting columns
  #    plot_x = log(x) if xvar != "HDI", else x
  #    plot_y = log(ratio_col)
  df_sub <- df_sub %>%
    mutate(
      plot_x = if (xvar=="HDI") .data[[xvar]] else log(.data[[xvar]]),
      plot_y = log(.data[[ratio_col]])
    )
  
  # 4) Axis labels
  x_label <- if (xvar=="HDI") {
    "HDI"
  } else if (xvar=="gdp_md") {
    "GDP (million USD)"
  } else if (xvar=="pop_est") {
    "Population"
} else{
    "GDP per capita (USD)"
  }
  y_label <- "Pub/Area Ratio"
  
  # 5) Determine annotation coords
  x_min <- min(df_sub$plot_x, na.rm=TRUE)
  x_max <- max(df_sub$plot_x, na.rm=TRUE)
  y_min <- min(df_sub$plot_y, na.rm=TRUE)
  y_max <- max(df_sub$plot_y, na.rm=TRUE)
  
  ann_x <- x_min + 0.05*(x_max - x_min)
  ann_y1 <- y_max - 0.05*(y_max - y_min)  # correlation for all
  ann_y2 <- y_max - 0.15*(y_max - y_min)  # correlation for top 15
  
  # 6) Build the ggplot
  p <- ggplot(df_sub, aes(x=plot_x, y=plot_y)) +
    # (1) Smoothing line for all countries (dashed black)
    geom_smooth(method="lm", se=FALSE, color="black", linetype="dashed") +
    
    # (2) Smoothing line for top 15 only (darkcyan)
    geom_smooth(
      data = df_top15,
      aes(x=if (xvar=="HDI") .data[[xvar]] else log(.data[[xvar]]),
          y=log(.data[[ratio_col]])),
      method="lm", se=FALSE, color="darkcyan"
    ) +
    
    # (3) Points colored by ratio only for top 15
    geom_point(
      aes(color = ifelse(is_top, log(.data[[ratio_col]]), NA),
          size  = .data[[area_col]]),
      alpha=0.8
    ) +
    
    # (4) Diverging color scale around ratio=1 => log(1)=0
    scale_color_gradient2(
      low="red", mid="white", high="blue", midpoint=0,
      breaks = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)),
      labels = c("0.1","0.5","1","2","5","10"),
      name = paste0("Ratio\n(pub / ", biome_name, " area)"),
      na.value = "grey50"
    ) +
    
    scale_size_continuous(
      range=c(2,8),
      name=paste0("Area (", biome_name, ")")
    ) +
    
    # (5) Y-axis with normal ratio ticks
    scale_y_continuous(
      breaks=log(c(0.1,0.5,1,2,5,10)),
      labels=c("0.1","0.5","1","2","5","10"),
      name=y_label
    ) +
    
    # (6) X-axis logic:
    {
      if (xvar=="HDI") {
        # normal HDI scale
        scale_x_continuous(name=x_label)
      } else if (xvar=="gdp_per_capita") {
        scale_x_continuous(
        name = x_label,
        breaks = c(log(500), log(1000), log(2000),log(10000), log(50000)),
        labels = c("5e2", "1e3","2e3" ,"1e4", "5e4"))
      }else {
        # for population or GDP, we assume typical range from e.g. 1e5 to 1e9
        # You can adapt these breaks to your actual data.
        scale_x_continuous(
          name = x_label,
          breaks = c(log(1e5), log(1e6), log(1e7), log(1e8), log(1e9)),
          labels = c("1e5", "1e6", "1e7", "1e8", "1e9")
        )
      }
    } +
    
    # (7) correlation annotations
    annotate("text", x=ann_x, y=ann_y1, label=cor_label, hjust=0, size=4, color="black") +
    annotate("text", x=ann_x, y=ann_y2, label=cor_label_top, hjust=0, size=4, color="darkcyan") +
    
    # (8) Label top 15 countries
    geom_text_repel(
      data=df_sub %>% filter(is_top==TRUE),
      aes(label=.data[[iso_col]]),
      size=3, max.overlaps=30
    ) +
    
    theme_minimal(base_size=14)
  
  p
}



# For HDI
p_hdi <- plot_area_vs_logratio_colored(
  df         = df_savanna,
  xvar       = "HDI",          # normal scale on x
  area_col   = "savanna_prop",  # top 15 based on 'grassy_prop'
  biome_name = "Savanna",
  top_n      = 15
)

# For population
p_pop <- plot_area_vs_logratio_colored(
  df         = df_savanna,
  xvar       = "pop_est",      # log scale on x
  area_col   = "savanna_prop",
  biome_name = "Savanna",
  top_n      = 15
)

# For GDP
p_gdp <- plot_area_vs_logratio_colored(
  df         = df_savanna,
  xvar       = "gdp_md",
  area_col   = "savanna_prop",
  biome_name = "Savanna",
  top_n      = 15
)

p_gdp_per_capita <- plot_area_vs_logratio_colored(
  df         = df_savanna,
  xvar       = "gdp_per_capita",
  area_col   = "savanna_prop",
  biome_name = "Savanna",
  top_n      = 15
)
# Then view or arrange them
library(ggpubr)

savannah_cor_plot <- ggarrange(plotlist = c(list(p_hdi), list(p_pop), list(p_gdp), list(p_gdp_per_capita)), nrow=1, ncol = 4,common.legend = TRUE, legend = "bottom",labels = c("e","f","g","h"))

ggsave("savannah_cor_plot.png", plot =savannah_cor_plot,width = 12, height = 6, dpi = 600)

# For HDI
p_hdi <- plot_area_vs_logratio_colored(
  df         = df_forest,
  xvar       = "HDI",          # normal scale on x
  area_col   = "forest_prop",  # top 15 based on 'grassy_prop'
  biome_name = "Forest",
  top_n      = 15
)

# For population
p_pop <- plot_area_vs_logratio_colored(
  df         = df_forest,
  xvar       = "pop_est",      # log scale on x
  area_col   = "forest_prop",
  biome_name = "Forest",
  top_n      = 15
)

# For GDP
p_gdp <- plot_area_vs_logratio_colored(
  df         = df_forest,
  xvar       = "gdp_md",
  area_col   = "forest_prop",
  biome_name = "Forest",
  top_n      = 15
)

p_gdp_per_capita <- plot_area_vs_logratio_colored(
  df         = df_forest,
  xvar       = "gdp_per_capita",
  area_col   = "forest_prop",
  biome_name = "Forest",
  top_n      = 15
)

forest_cor_plot <- ggarrange(plotlist = c(list(p_hdi), list(p_pop), list(p_gdp),list(p_gdp_per_capita)), nrow=1, ncol = 4,common.legend = TRUE, legend = "bottom",labels = c("a","b","c", "d"))
ggsave("forest_pub_ratio.png", plot =forest_cor_plot,width = 12, height = 6, dpi = 600)

for_sav_comb_corr_plot <- ggarrange(forest_cor_plot, savannah_cor_plot, nrow = 2, ncol = 1)
ggsave("for_sav_comb_corr_plot.png", plot =for_sav_comb_corr_plot,width = 12, height = 10, dpi = 600)

# Then view or arrange them
library(ggpubr)

top_savanna <- df_savanna %>%
  arrange(desc(savanna_prop)) %>%
  slice_head(n = 15)

top_savanna <- top_savanna %>%
  # Sort by HDI (ascending)
  arrange(HDI) %>%
  
  # Factor the country column so ggplot uses that sorted order
  mutate(country = factor(name, levels = name)) %>%
  
  # Scale grassy share so the max grassy holder => width=1
  mutate(width_scaled = log(savanna_prop) / max(log(savanna_prop)))

top_savanna_hdi_corr <- cor.test(top_savanna$HDI, log(top_savanna$ratio), method="spearman")

top_forest <- df_forest %>%
  arrange(desc(forest_prop)) %>%
  slice_head(n = 15)

top_forest <- top_forest %>%
  # Sort by HDI (ascending)
  arrange(HDI) %>%
  
  # Factor the country column so ggplot uses that sorted order
  mutate(country = factor(name, levels = name)) %>%
  
  # Scale grassy share so the max grassy holder => width=1
  mutate(width_scaled = log(forest_prop) / max(log(forest_prop)))

top_forest_hdi_corr <- cor.test(top_forest$HDI, log(top_forest$ratio), method="spearman")


plot_biome_bar_chart <- function(
    df,
    biome_name      = "Savanna",
    country_col     = "country",     # x-axis factor
    hdi_col         = "HDI",         # bar height
    ratio_col       = "ratio",       # fill color
    width_col       = "width_scaled" # scaled bar width
) {
  # Basic checks
  if(!all(c(country_col, hdi_col, ratio_col, width_col) %in% colnames(df))) {
    stop("Data frame does not contain all required columns:", 
         country_col, hdi_col, ratio_col, width_col)
  }
  
  # Build the plot
  p <- ggplot(df, aes(x=.data[[country_col]], y=.data[[hdi_col]])) +
    geom_col(
      aes(
        fill = .data[[ratio_col]],   # color by ratio
        width= .data[[width_col]]    # bar width
      ),
      position = position_identity()
    ) +
    coord_flip() +  
    # Use a log transform for ratio fill, diverging at ratio=1 => log(1)=0
    scale_fill_gradient2(
      trans = "log",
      breaks = c(0.2, 1, 4),  # example breaks
      labels = c("0.2", "1", "4"),
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = 0, # log(1)=0 => ratio=1 is white
      name = paste("Ratio\n(Publication /\nGlobal", biome_name,"area)")
    ) +
    labs(
      y = "HDI",
      x = "Country",
      subtitle = paste("Bar thickness = proportion of global", biome_name, "area")
    ) +
    theme_minimal() +
    theme(
      legend.text   = element_text(size = 14),
      axis.title.x  = element_text(size = 14),
      axis.title.y  = element_text(size = 14),
      axis.text.x   = element_text(size = 12),
      axis.text.y   = element_text(size = 12)
    )
  
  p
}

savanna_bar_plot <- plot_biome_bar_chart(
  df           = top_savanna,
  biome_name   = "Savanna",
  country_col  = "country",
  hdi_col      = "HDI",
  ratio_col    = "ratio",
  width_col    = "width_scaled"
)

savanna_bar_plot

forest_bar_plot <- plot_biome_bar_chart(
  df           = top_forest,
  biome_name   = "Forest",
  country_col  = "country",
  hdi_col      = "HDI",
  ratio_col    = "ratio",
  width_col    = "width_scaled"
)

forest_bar_plot

#################### Combine Interesting Plots ####################

area_pub_hdi_plot <- ggarrange(forest_area_pub_plot, savanna_area_pub_plot, forest_ratio_map, savanna_ratio_map, nrow=2, ncol=2, labels = c("a","b","c","d"))
ggsave("area_pub_hdi_plot.png", plot =area_pub_hdi_plot,width = 15, height = 10, dpi = 1200)

