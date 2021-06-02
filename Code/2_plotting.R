rm(list=ls())
options(scipen = 10^6)

library(tidyverse)
library(urbnmapr)
library(sf)
library(viridis)
library(ggmap)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project"

# Loading 2020 Elections Results Data by County
election_2020 <- read_csv(file.path("Intermediate", "election_2020_county.csv"))

# Loading Spatial information by County
county_sf <- get_urbn_map("counties", sf = TRUE)

# Merging datasets
spatial_20 <-
  county_sf %>%
  left_join(election_2020, by = c("county_fips" = "GEOID")) 


##### Plotting 

# Function to create Maps

making_maps <- function(df, plot_var, breaks, transformation = 1, fill_label, Title, Subtitle, Caption, set_color){
  
  Min = min(as.data.frame(df)[,plot_var], na.rm = TRUE)
  Max = max(as.data.frame(df)[,plot_var], na.rm = TRUE)
  
  Min = Min * transformation     # Transformation
  Max = Max * transformation
  
  if (length(breaks) > 1) {
    pretty_breaks <- c(Min, breaks,  Max)   # Defining breaks for partitioning plotting variable
  } else {
    pretty_breaks <- breaks
  }
    
  created_map <-
    df %>% 
    select(plot_var, geometry) %>%
    mutate(labeled_var =  cut(get(plot_var) * transformation,       # Transforming relevant variable into categories
                              breaks = pretty_breaks,    # By specific breaks
                              include.lowest = TRUE)) %>%
    ggplot() +
    geom_sf(aes(fill = labeled_var), size = 0.01, color = "grey22") +
    labs(fill = fill_label, title = Title, subtitle = Subtitle,
         caption = paste0("Note: Alaska provides electoral results by Electoral Disctrict.\n",
                          Caption))+
    scale_fill_manual(
      values = set_color,
      drop = FALSE,
      guide = guide_legend(
        reverse = FALSE,
        keyheight = unit(2, units = "mm"),
        keywidth = unit(20, units = "mm"),
        direction = "horizontal",
        byrow = T,
        nrow = 1,
        title.position = 'top',
        title.hjust = 0.5,
        label.position = "bottom"
      )) +
    theme_nothing(legend = TRUE) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = -0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5, vjust = -0.5))
    
  
  return(created_map)
}

##### 1. Republican Votes Share Margin (pp.) by County
margin_2020_map <- 
  making_maps(spatial_20,
              "gop_margin",
              c( -45, -25, -5, 0, 5, 25, 45),
              100,
              "2020 Republican Votes Share Margin (pp.)",
              "2020 Presidential Election Results by County",
              "Republican Votes Share Margin (pp.)",
              "Source: Data compiled by T. McGovern using data from The Guardian, townhall.com, Fox News, Politico, and the New York Times.",
              c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"))

# Saving plot
ggsave(file.path("Output","Map_2020_Results_County.png"), margin_2020_map, width = 20, height = 8.7)


##### 2. Change in Unemployment Rate Jan-Oct 2020 vs Jan-Oct 2019 by County
deltaUn_2020_map <-
  making_maps(spatial_20,
              "delta_unemployment",
              c( 0, 1, 1.5, 2.5, 3.5, 4.5, 5.5),
              100,
              "Change in Unemployment Rate (pp.)",
              "Increase of Unemployment Rate by Counties pre- 2020 Elections",
              "Change in Unemployment Rate Jan-Oct 2020 vs. Jan-Oct 2019 (pp.)",
              "Source: Local Area Unemployment Statistics - BLS",
              c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84'))
  
# Saving plot
ggsave(file.path("Output","Map_2020_Delta_Unemployment_county.png"), deltaUn_2020_map, width = 20, height = 8.7)

##### 3. Cumulative COVID cases per 10k people as 31-Oct-2019 by County
covid_2020_map <-
  making_maps(spatial_20,
              "cases_per10k",
              c(1, 2, 3, 4, 5, 6, 7),
              0.01,
              "Cumulative COVID cases per 100 people",
              "Cumulative COVID-19 Cases as of Oct 31st 2020 by County",
              "Number of COVID-19 Cases per 100 people",
              "Source: CSSE at Johns Hopkins University",
              c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#6e016b'))

# Saving plot
ggsave(file.path("Output","Map_2020_covid_county.png"), covid_2020_map, width = 20, height = 8.7)

##### 4. Change in Community Mobility to Work by County
mobility_2020_map <-
  making_maps(spatial_20,
              "mobility_deaths_peak",
              c(-50, -45, -40, -35,-30, -25, 0),
              1,
              "Change in Community Mobility to Work",
              "Difference in Mobility to Work Index between Mid-April and February",
              "Difference in Community Mobility Index",
              "Additional: There is no information for some Central counties due to very low activity preventing from achieving anonymity threshold set by Google.\nSource: Google",
              c("#005824", "#238b45", "#41ae76", "#66c2a4", "#99d8c9", "#ccece6", "#e5f5f9", "#f7fcfd"))

# Saving plot
ggsave(file.path("Output","Map_2020_mobility_county.png"), mobility_2020_map, width = 20, height = 8.7)



##### 5. Republican Votes Share Margin (pp.) vs.
#####    Change in Unemployment Rate Jan-Oct 2020 vs Jan-Oct 2019
#####    by County

# Calculating Medians
med_delta_unemp <- median(spatial_20$delta_unemployment, na.rm = T) * 100
med_gop_margin  <- median(spatial_20$gop_margin, na.rm = T) * 100

gop_margin_vs_delta_unemployment_scatter <-
  spatial_20 %>%
  select(delta_unemployment, gop_margin, total_votes) %>%
  ggplot(aes(x = delta_unemployment * 100, y = gop_margin * 100, size = total_votes / 10^6)) +
  geom_point(
    shape = 21,
    color = "gray30",
    fill = "hotpink", 
    stroke = 0.5,
    alpha = 0.4) +
  scale_size(range = c(0, 10))+
  geom_vline(xintercept = med_delta_unemp, size = 0.25, linetype = 2) +
  annotate("text", x = med_delta_unemp, y = 100, label = paste("Median = ", round(med_delta_unemp, 1)), hjust = -0.05) +
  geom_hline(yintercept = med_gop_margin, size = 0.25, linetype = 2) + 
  annotate("text", y = med_gop_margin, x = 14.5, label = paste("Median = ", round(med_gop_margin, 1)), vjust = -0.25) +
  labs(title = "2020 Republican Votes Share Margin vs.\nChange in Unemployment Rate 2020-2019*",
       x = "Change in Unemployment Rate: Jan-Oct 2020 vs. Jan-Oct 2019 (pp.)",
       y = " Republican Votes Share Margin (pp.)",
       size = "Million of Votes",
       caption = "*Average January to October for both years.") +
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = -0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, vjust = -0.5))

# Saving plot
ggsave(file.path("Output","gop_margin_vs_delta_unemployment_scatter.png"), gop_margin_vs_delta_unemployment_scatter, width = 8, height = 8)


## Saving objects
saveRDS(list(margin = margin_2020_map,
             delta = deltaUn_2020_map), file = file.path("Intermediate","PlotsToShiny.rds"))
