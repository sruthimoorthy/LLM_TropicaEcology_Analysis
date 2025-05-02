library(tidyr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(maps)
library(mapdata)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)
library(viridis)
library(ggpubr)
library(patchwork)
library(hrbrthemes)
library(cowplot)
# Set the working directory to where your data files are located
setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets")

publications <- read.csv("Tropical_filtered_publications.csv")

author_aff <- read.csv("AuthorAffiliationResults.csv")

author_aff <- author_aff %>% filter(UT..Unique.WOS.ID. %in% publications$ID)

author_aff_data <- author_aff %>% distinct(UT..Unique.WOS.ID.,.keep_all = TRUE) 

tropical_studies <- data.frame(ID = author_aff_data$UT..Unique.WOS.ID.,
                               year = author_aff_data$Publication.Year,
                               first_author_countries = author_aff_data$First.Author.ISO3,
                               all_author_countries = author_aff_data$All.Author.ISO3,
                               studySite = author_aff_data$ISO3_std)

rm(publications)

# Clean the author country lists by removing unwanted characters (e.g., brackets)
tropical_studies$first_author_countries <- gsub("\\[|\\]|\\{|\\}|\\'", "", tropical_studies$first_author_countries)

# Filter out studies with empty or irrelevant study country information
tropical_studies <- tropical_studies %>%
  filter(!(grepl("not found", first_author_countries) | first_author_countries == "" | is.na(first_author_countries)))

publications_site <- tropical_studies %>%
  separate_rows(first_author_countries, sep = ",") %>%
  mutate(sepISO3 = trimws(first_author_countries))

summary <- publications_site %>%
  mutate(yearCategory = case_when(
    year < 1980 ~ "pre 1980s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010  ~ "post 2010s"
  )) 

summary$yearCategory <- as.factor(summary$yearCategory)

summary_no_other <- summary %>% filter(!(sepISO3 == "" | is.na(sepISO3)))

site_counts <- summary_no_other %>%
  group_by(yearCategory, sepISO3) %>%
  summarise(unique_publications = n_distinct(ID), .groups = 'drop')

site_totals <- summary_no_other %>%
  group_by(yearCategory) %>%
  summarise(total_publications = n_distinct(ID), .groups = 'drop') %>%
  arrange(desc(total_publications)) 

# Recalculate the proportions with "Other" category
site_proportions <- site_counts %>%
  left_join(site_totals, by = "yearCategory") %>%
  mutate(proportion = (unique_publications / total_publications) * 100) %>%
  arrange(desc(proportion))


site_proportions <- site_proportions %>%
  group_by(yearCategory) %>%
  mutate(rank = rank(-proportion, ties.method = "min"))

#combined_ranks <- site_proportions %>%
#  group_by(sepISO3) %>%
#  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop')


# Step 2: Count how many decades each country is present in
country_decade_count <- site_proportions %>%
  group_by(sepISO3) %>%
  summarise(decade_count = n_distinct(yearCategory), .groups = 'drop')

# Step 3: Sum ranks across all decades for each country within each biome,
# but only include those with more than one decade
combined_ranks <- site_proportions %>%
  group_by(sepISO3) %>%
  summarise(combined_rank = sum(rank, na.rm = TRUE), .groups = 'drop') %>%
  inner_join(country_decade_count %>% filter(decade_count > 3), by = c("sepISO3")) 

# Step 4: Select the top 10 countries based on the combined rank
researchLeads2010s <- site_proportions %>%
  filter(yearCategory == "post 2010s")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- st_make_valid(world)
world_equal_area <- st_transform(world, crs = "+proj=eck4")
world_equal_area$land_area_km2 <- as.numeric(st_area(world_equal_area) / 1e6)

world_equal_area$gdp_per_capita <- (world_equal_area$gdp_md*1000000)/world_equal_area$pop_est

# pop_est, gdp_md_est might be in 'world'
df_world <- world_equal_area %>%
  st_drop_geometry() %>%
  dplyr::select(name, iso_a3, land_area_km2, pop_est, gdp_md, gdp_per_capita) %>%
  rename(ISO3 = iso_a3)

df_hdi <- read.csv("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles/Datasets/UNHDIValuesWithISO3.csv")   # or however you have it
# Must have columns "ISO3" and "HDI"
df_hdi$ISO3 <- df_hdi$iso3

df_forest <- 
df_2010 <- researchLeads2010s %>%
  dplyr::select(sepISO3, proportion) %>%
  mutate(pub_prop = proportion / 100) %>%  # if proportion was % out of 100
  rename(ISO3 = sepISO3)

df_merged_2010 <- df_2010 %>%
  left_join(df_world, by="ISO3") %>%
  left_join(df_hdi, by="ISO3")

# We now have columns:
#  ISO3, pub_prop, pop_est, gdp_md_est, HDI
df_merged_2010 <- df_merged_2010 %>%
  filter(!is.na(pub_prop),
         !is.na(pop_est),
         pop_est > 0,
         !is.na(gdp_md),
         gdp_md > 0,
         !is.na(HDI),
         HDI > 0)

all_land_threshold_km2 <- 1000

df_merged_2010 <- df_merged_2010 %>%
  filter(land_area_km2 >= all_land_threshold_km2)

publication_threshold <- 0.01

df_merged_2010 <- df_merged_2010 %>%
  filter(pub_prop >= publication_threshold)

library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

plot_pubprop_log_axes <- function(
    df,
    xvar = "pop_est",      # or "gdp_md_est", or "HDI"
    pub_col = "pub_prop",  # publication proportion in [0..1]
    iso_col = "ISO3",
    top_n   = 10
) {
  # 1) Filter valid rows
  df_sub <- df %>%
    filter(!is.na(.data[[xvar]]),
           !is.na(.data[[pub_col]]),
           .data[[pub_col]] > 0)
  if(xvar != "HDI") {
    df_sub <- df_sub %>% filter(.data[[xvar]]>0)
  }
  if(nrow(df_sub) < 5) {
    message("Not enough data to plot.")
    return(NULL)
  }
  
  # 2) Identify top N by pub_col
  df_sub <- df_sub %>%
    mutate(is_top = TRUE)
  
  # 3) Create columns for *plotting* in log space
  #    If xvar=="HDI", keep x in normal scale => no log
  #    else, x= log(xvar)
  #    always do y= log(pub_prop)
  
  if(xvar == "HDI") {
    df_sub <- df_sub %>%
      mutate(
        plot_x = .data[[xvar]],      # normal scale
        plot_y = log(.data[[pub_col]])
      )
  } else {
    df_sub <- df_sub %>%
      mutate(
        plot_x = log(.data[[xvar]]), # log scale
        plot_y = log(.data[[pub_col]])
      )
  }
  
  # 4) Spearman correlation on the same transformations
  #    if xvar=="HDI", cor(HDI, log(pub_prop)), else cor(log(xvar), log(pub_prop))
  cor_x <- if(xvar=="HDI") df_sub[[xvar]] else log(df_sub[[xvar]])
  cor_y <- log(df_sub[[pub_col]])
  
  cor_out <- cor.test(cor_x, cor_y, method="spearman")
  rho_val <- round(unname(cor_out$estimate), 3)
  p_val   <- round(unname(cor_out$p.value),  3)
  cor_label <- paste0("rho=", rho_val, ", p=", p_val)
  
  # 5) Setup axis breaks & labels for log scale
  #    We'll guess some typical ranges, but you can adapt them
  #    (A) x-axis
  if(xvar=="HDI") {
    # normal scale => no special breaks
    x_scale <- scale_x_continuous()
    x_label <- "HDI"
  } else if(xvar=="pop_est") {
    # for population or GDP, let's guess
    # find min and max in original space
    x_original_min <- min(df_sub[[xvar]], na.rm=TRUE)
    x_original_max <- max(df_sub[[xvar]], na.rm=TRUE)
    
    # We'll define some breaks. E.g. if pop ranges from 1e5 to 1e8
    # you might do breaks = log(c(1e5,1e6,1e7,1e8)) with labels c("1e5","1e6","1e7","1e8")
    # In a real script, you might dynamically generate them, but let's pick a typical set:
    # for example, let's handle pop 1e4 to 1e9
    x_breaks <- c(log(1e4), log(1e5), log(1e6), log(1e7), log(1e8), log(1e9))
    x_labels <- c("1e4","1e5","1e6","1e7","1e8","1e9")
    
    x_scale <- scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
    )
    x_label <- "Population"
  } else if (xvar=="gdp_per_capita") {
    x_scale <- scale_x_continuous(
      breaks = c(log(500), log(1000), log(2000),log(10000), log(50000)),
      labels = c("5e2", "1e3","2e3" ,"1e4", "5e4")
      )
    x_label <- "GDP per capita (USD)"
  }else{ 
    x_original_min <- min(df_sub[[xvar]], na.rm=TRUE)
    x_original_max <- max(df_sub[[xvar]], na.rm=TRUE)
    
    # We'll define some breaks. E.g. if pop ranges from 1e5 to 1e8
    # you might do breaks = log(c(1e5,1e6,1e7,1e8)) with labels c("1e5","1e6","1e7","1e8")
    # In a real script, you might dynamically generate them, but let's pick a typical set:
    # for example, let's handle pop 1e4 to 1e9
    x_breaks <- c(log(1e4), log(1e5), log(1e6), log(1e7), log(1e8), log(1e9))
    x_labels <- c("1e4","1e5","1e6","1e7","1e8","1e9")
    
    x_scale <- scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
    )
    x_label <- "GDP (million USD)"
    }
  
  #    (B) y-axis for pub_prop in [0..1], let's do e.g. 0.001 -> 0.1
  # compute min and max in the original pub_prop space
  y_original_min <- min(df_sub[[pub_col]], na.rm=TRUE) # e.g. 0.0005
  y_original_max <- max(df_sub[[pub_col]], na.rm=TRUE) # e.g. 0.5
  
  # We'll pick typical breaks e.g. 0.001, 0.01, 0.1, 1
  y_breaks <- c(log(0.001), log(0.01), log(0.1), log(1))
  y_labels <- c("0.001","0.01","0.1","1")
  
  y_scale <- scale_y_continuous(
    breaks = y_breaks,
    labels = y_labels,
    name = "Publication proportion"
  )
  
  # 6) Build the plot
  p <- ggplot(df_sub, aes(x=plot_x, y=plot_y)) +
    geom_point(color = "#F5C710",size=3, alpha=0.7) +
    # optional linear fit in log-log space
    geom_smooth(method="lm", color="black", se=FALSE) +
 
    
    # label top 15
    geom_text_repel(
      data = df_sub %>% filter(is_top==TRUE),
      aes(label=.data[[iso_col]]),
      size=3, max.overlaps=30
    ) +
    
    x_scale + y_scale +
    
    labs(
      x = x_label
    ) +
    
    # correlation annotation
    annotate("text",
             x = min(df_sub$plot_x) + 0.1*(max(df_sub$plot_x)-min(df_sub$plot_x)),
             y = max(df_sub$plot_y) - 0.1*(max(df_sub$plot_y)-min(df_sub$plot_y)),
             label = cor_label,
             hjust=0,
             size=4
    ) +
    theme_minimal(base_size=14)
  
  p
}

p_hdi <- plot_pubprop_log_axes(df_merged_2010, "HDI", top_n=17)
p_pop <- plot_pubprop_log_axes(df_merged_2010, "pop_est", top_n=17)
p_gdp <- plot_pubprop_log_axes(df_merged_2010, "gdp_md", top_n=17)
p_gdp_per_capita <- plot_pubprop_log_axes(df_merged_2010, "gdp_per_capita", top_n=17)

social_lead_author_corr_plot <- ggarrange(p_hdi,p_gdp_per_capita, p_pop, p_gdp, nrow=1, ncol = 4,labels = c("a","b","c","d"))
ggsave("social_lead_author_corr_plot.png", plot =social_lead_author_corr_plot,width = 14, height = 6, dpi = 600)


