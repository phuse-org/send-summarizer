#Code recycled from makeradar
#Change made 02/08/2024 "Gender" Parts all commented out/removed as it was not necessary for the bar plot

#Output Radar plot for Cross-Study Analysis for organSystem or all defined 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Gender must be restricted for each plot to either M or F or Combined

#Current organSystem options: ALL, SUMMARY, REPRODUCTIVE, LIVER, KIDNEY, HEMATOPOIETIC,
#ENDOCRINE and BW

makeBarPlot <- function(summaryData, organSystem, testvar) {
  
  ##library(fmsb)
  ##library(tidyr)
  ##library(ggplot2)
  ##library(plotly)
  organSystem <- toupper(organSystem)
  #Limit summaryData to desired Organ System
  if (organSystem %in% c('ALL', 'SUMMARY'))
  {
    #Takes Summary Values which do not have '.'
    Data <- summaryData[!grepl("\\.", rownames(summaryData)),]
    Title <- "Summary"
    a <- 0
  } else {
    #Limits summaryData to desired organSystem for individual radar plots
    Data <- summaryData[which(grepl(paste0(organSystem,"."),rownames(summaryData))),]
    Title <- organSystem
    a <- 1
  }
  
  #Limit to Gender Desired
  #Gender <- toupper(Gender)
  #Data <- Data[ ,which(grepl(Gender, colnames(Data)))]
  
  #Create better labeps
  if (a == 0){
    
  } else {
    rownames(Data) <- gsub(paste0(organSystem,"."),"",rownames(Data))
  }
  
  #Format Data for RadarChart by adding min and max to dataset
  Data <- t(Data)
  Data <- rbind(rep(3,ncol(Data)),rep(0,ncol(Data)), Data)
  Data <- as.data.frame(Data)
  
  #Create Group Names
  GroupNames <-str_replace(rownames(Data)[3:nrow(Data)],"\\.", " ")
  #Grab Num Species/Compounds from GroupNames
  Species <- word(GroupNames,1)
  Compounds <- substr(word(GroupNames,2),1, nchar(word(GroupNames,2))-2)
  
  #Set Colors for Radar Plot using Compounds
  ColorOptions <- c(rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.3,0.7,0.9,0.9), rgb(0.1,0.2,0.3,0.9), rgb(0.1,0.8,0.5,0.9))
  i <- 1
  for (Compound in  unique(Compounds)){
    Compounds <- str_replace_all(Compounds,Compound, ColorOptions[i])
    i <- i+1
  }
  colors_border <- Compounds
  
  #Set Shape for Radar using Species
  shapes_line_options <- c('1','2','3','4')
  shapes_point_options <- c('16','17','15','18')
  legend_point_options <- c('20','17','15','18')
  SpeciesFormat <- data.frame(shapes = Species,
                              points = Species,
                              legends = Species)
  j <- 1
  for (animal in unique(Species)){
    SpeciesFormat$shapes <- str_replace_all(SpeciesFormat$shapes, animal, shapes_line_options[j])
    SpeciesFormat$points <- str_replace_all(SpeciesFormat$points, animal, shapes_line_options[j])
    SpeciesFormat$legends <- str_replace_all(SpeciesFormat$legends, animal, shapes_line_options[j])
    j <- j+1
  }
  shapes_line <- as.numeric(SpeciesFormat$shapes)
  shapes_point <- as.numeric(SpeciesFormat$points)
  legend_point <- as.numeric(SpeciesFormat$legends)
  
  Data<-Data[-c(1,2),]
  Data2<-Data
  Data2["Species"]<-rownames(Data2)
  Data2<-Data2 %>% pivot_longer(cols=1:ncol(Data), names_to = "Indicators", values_to = "Values") 
  
  testvar<-testvar
  #print("makeBarPlot")
  #print(testvar)
  
  Data2$Indicators <- factor(Data2$Indicators, 
                        levels= testvar)
  
  Data <- Data2 %>%
    dplyr::arrange(Indicators) %>% na.omit()
  
  #saveRDS(Data, "intermediate.RDS")
  
  #Generate Bar plot
  par(xpd= TRUE,mar = c(1,12,2,12), oma = c(1,12,2,12))
  #saveRDS(Data2,"Data2.RDS")
   #b <- ggplot(Data2, aes( x = reorder(Indicators,-Values), y = Values, fill = Species ))
  b <- ggplot(Data, aes( x = Indicators, y = Values, fill = Species ))
    b<-b + geom_bar( position = "dodge", stat = "identity", alpha = .5 )
   b<-b + theme(axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0.5))
   barplot<- b + labs(title=NULL,x="Indicators")
  barplotly<-ggplotly(barplot)
   return(barplotly)
}

displayIndic <- function(summaryData, organSystem) {
  
  ##library(fmsb)
  ##library(tidyr)
  ##library(ggplot2)
  ##library(plotly)
  organSystem <- toupper(organSystem)
  #Limit summaryData to desired Organ System
  if (organSystem %in% c('ALL', 'SUMMARY'))
  {
    #Takes Summary Values which do not have '.'
    Data <- summaryData[!grepl("\\.", rownames(summaryData)),]
    Title <- "Summary"
    a <- 0
  } else {
    #Limits summaryData to desired organSystem for individual radar plots
    Data <- summaryData[which(grepl(paste0(organSystem,"."),rownames(summaryData))),]
    Title <- organSystem
    a <- 1
  }
  
  #Limit to Gender Desired
  #Gender <- toupper(Gender)
  #Data <- Data[ ,which(grepl(Gender, colnames(Data)))]
  
  #Create better labeps
  if (a == 0){
    
  } else {
    rownames(Data) <- gsub(paste0(organSystem,"."),"",rownames(Data))
  }
  
  #Format Data for RadarChart by adding min and max to dataset
  Data <- t(Data)
  Data <- rbind(rep(3,ncol(Data)),rep(0,ncol(Data)), Data)
  Data <- as.data.frame(Data)
  
  #Create Group Names
  GroupNames <-str_replace(rownames(Data)[3:nrow(Data)],"\\.", " ")
  #Grab Num Species/Compounds from GroupNames
  Species <- word(GroupNames,1)
  Compounds <- substr(word(GroupNames,2),1, nchar(word(GroupNames,2))-2)
  
  #Set Colors for Radar Plot using Compounds
  ColorOptions <- c(rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.3,0.7,0.9,0.9), rgb(0.1,0.2,0.3,0.9), rgb(0.1,0.8,0.5,0.9))
  i <- 1
  for (Compound in  unique(Compounds)){
    Compounds <- str_replace_all(Compounds,Compound, ColorOptions[i])
    i <- i+1
  }
  colors_border <- Compounds
  
  #Set Shape for Radar using Species
  shapes_line_options <- c('1','2','3','4')
  shapes_point_options <- c('16','17','15','18')
  legend_point_options <- c('20','17','15','18')
  SpeciesFormat <- data.frame(shapes = Species,
                              points = Species,
                              legends = Species)
  j <- 1
  for (animal in unique(Species)){
    SpeciesFormat$shapes <- str_replace_all(SpeciesFormat$shapes, animal, shapes_line_options[j])
    SpeciesFormat$points <- str_replace_all(SpeciesFormat$points, animal, shapes_line_options[j])
    SpeciesFormat$legends <- str_replace_all(SpeciesFormat$legends, animal, shapes_line_options[j])
    j <- j+1
  }
  shapes_line <- as.numeric(SpeciesFormat$shapes)
  shapes_point <- as.numeric(SpeciesFormat$points)
  legend_point <- as.numeric(SpeciesFormat$legends)
  
  Data<-Data[-c(1,2),]
  Data2<-Data
  Data2["Species"]<-rownames(Data2)
  Data2<-Data2 %>% pivot_longer(cols=1:ncol(Data), names_to = "Indicators", values_to = "Values") 
  Data2  

}
