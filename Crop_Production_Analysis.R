
### Loading libraries
  library(data.world)
  library(dplyr)
  library(ggplot2)
  library(gganimate)
  library(RColorBrewer)

### Loading Data
  crop_production <- read.csv("https://query.data.world/s/c7o6v7fgxuxutd6xjyj34e77psxoy3", header=TRUE, 
                              stringsAsFactors=FALSE)
  summary(crop_production)
  str(crop_production)
  View(crop_production)
  
### Generating colors
  n <- 50
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  

### Exploratory Data Analysis
  
  ## Checking for missing values
    sapply(crop_production, function(x) sum(is.na(x)))
    
  ## Converting columns to factor
    crop_production$Crop <- as.factor(crop_production$Crop)
    
    
  ## Calculating total production of "Rice" between 1997-2015
    total_rice_production <- crop_production %>% subset(Crop == "Rice") %>% group_by(Crop_Year) %>% 
                          summarise(Total_Production = sum(Production, na.rm = T)) %>% as.data.frame()
    
    rice_production_by_state <- crop_production %>% subset(Crop == "Rice") %>% group_by(Crop_Year, State_Name) %>% 
                                summarise(Total_Production = sum(Production, na.rm = T)) %>% as.data.frame()
    
    # Plotting the Qunatity of "Rice" production in different states between 1997-2015
      ggplot(rice_production_by_state, aes(State_Name,Total_Production,fill = State_Name)) + geom_bar(stat = "identity", width = 0.5) + 
      theme_classic() + coord_flip() + labs(title = 'Year: {frame_time}', x = 'State', y = 'Total Production') + transition_time(Crop_Year) + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
      scale_fill_manual(values = sample(col_vector, n))

    
  ## Calculating "Rice" production in "Uttar Pradesh" in between 1997-2015
    rice_production_up <- crop_production %>% subset(Crop == "Rice" & State_Name == "Uttar Pradesh") %>% group_by(Crop_Year) %>% 
                          summarise(Total_Production = sum(Production, na.rm = T)) %>% as.data.frame()
    
    # Plotting the Qunatity of "Rice" production in "Uttar Pradesh" between 1997-2015
      p <- ggplot(rice_production_up, aes(as.factor(Crop_Year), Total_Production, fill = Total_Production)) + geom_col() + 
           scale_fill_distiller(palette = "Greens", direction = 1) + ggtitle("Production of Rice in Uttar Pradesh") 
           labs(x="Crop Year",y="Total Rice Production") + theme_minimal() +
           theme(
              panel.grid = element_blank(),
              panel.grid.major.y = element_line(color = "white"),
              panel.ontop = TRUE, plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))
    
      p + transition_states(Crop_Year, wrap = FALSE) + shadow_mark()
      
      
  ## Calculating crops production in "Uttar Pradesh" in between 1997-2015
      crops_production_up <- crop_production %>% subset(State_Name == "Uttar Pradesh") %>% group_by(Crop_Year,Crop) %>% 
                             summarise(Total_Production = sum(Production, na.rm = T)) %>% as.data.frame()
      
    # Plotting the Qunatity of "Rice" production in different states between 1997-2015
      ggplot(crops_production_up, aes(Crop,Total_Production,fill = Crop)) + geom_bar(stat = "identity", width = 0.5) + 
      theme_classic() + coord_flip() + labs(title = 'Year: {frame_time}', x = 'Crop', y = 'Total Production') + transition_time(Crop_Year) + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
      scale_fill_manual(values = sample(col_vector, n))
    
      sort(unique(crops_production_up$Rank))
      
      crops_production_up$Rank <- ave( -crops_production_up$Total_Production, crops_production_up$Crop_Year, FUN=rank )
      crops_production_up$Rank <- ave( -crops_production_up$Total_Production, crops_production_up[,1:2], FUN=rank )
      
      crops_production_up$Rank <- unlist(lapply(split(crops_production_up$Total_Production, 
                                  list(crops_production_up$Crop_Year, crops_production_up$Crop), drop = T), rank))
      
      crops_production_up <- crops_production_up %>% group_by(Crop_Year, Crop) %>% 
                              mutate(Rank=replace(min_rank(Total_Production), Crop=='b',0)) %>% as.data.frame()
      
      
      crops_production_up$Rank <- unlist(with(crops_production_up,tapply(Total_Production,Crop_Year, rank)))
      

      data <- read.csv('https://gist.githubusercontent.com/johnburnmurdoch/2e5712cce1e2a9407bf081a952b85bac/raw/08cf82f5e03c619f7da2700d1777da0b5247df18/INTERBRAND_brand_values_2000_2018_decimalised.csv')
      View(data)
 
      library(tidyverse)
     
      data %>%
        filter(rank<=10) %>% 
        ggplot(aes(x=-rank,y=value,fill=name, group=name)) +
        geom_tile(aes(y=value/2,height=value),width=0.9,show.legend = F)+
        geom_text(aes(label=name),
                  hjust="right",
                  colour="black",
                  fontface="bold",
                  nudge_y=-1000)+
        geom_text(aes(label=scales::comma(value)),
                  hjust="left",
                  nudge_y=2000,
                  colour="grey30")+
        theme_minimal() +
        coord_flip(clip="off") +
        scale_x_discrete("") +
        scale_y_continuous("",labels=scales::comma)+
        theme(panel.grid.major.y=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.text.y=element_blank()) +
        transition_time(year)-> p
      
      
      animate(p, nframes = 300, fps = 5, end_pause = 20)
      
      
      
      Sys.getenv("RSTUDIO_PANDOC")
      Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
      
      rmarkdown:::find_pandoc()
      as.list(rmarkdown:::.pandoc)
      
require(installr)
      installr::install.pandoc()      
      