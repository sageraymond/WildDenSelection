#title: "EINP Den Selection"
# author: "Juno Montgomery, Sage Raymond"
# date: "January - May 2025"

#Set WD

#Load relevant libraries
library(dplyr)
library(car)
library(tidyr)
library(DescTools)
library(MuMIn)
library(performance)
library(pROC)
library(jtools)
library(ggplot2)
library(sjPlot)
library(ggpubr)
library(ggeffects)

#Read in Third order Data
third <- read.csv("ThirdOrder.csv")

#Complete some data cleaning on Third order dataframe
#Removing unnecessary columns
third <- third  %>% 
  dplyr::select(-(c(FID, DEN_ID, Moisture, X)))


#Creating aspect indexes
#Recall that you can get radians from degrees by mutlipling by pi / 180
third <- third %>%
     mutate(AspRad = 0.0174532925 * Aspect) %>%
     mutate(East = sin(AspRad)) 

third <- third %>% mutate(third, Interim = cos(AspRad)) %>%
         mutate(South = Interim * -1)
         
third <- third %>%
  dplyr::select(-(c(Aspect, AspRad, Interim)))
         
#Next step is to generate mean values (and t test) for all third order covariates
#Generate summary statistics for covariate values
third_summary <- third %>% 
     dplyr::group_by(Use) %>% 
     dplyr::summarise_if(is.numeric, .funs=list(mean, sd, min, max)) %>%
     tidyr::pivot_longer(cols = -Use, 
                               names_to = c('.value', 'variable'), 
                               names_sep = '_fn')
         
third_summary$variable[third_summary$variable==1] <- "mean"
third_summary$variable[third_summary$variable==2] <- "sd"
third_summary$variable[third_summary$variable==3] <- "min"
third_summary$variable[third_summary$variable==4] <- "max"
         
         
third_summary <- third_summary%>%
   dplyr::select("Use",
                         "variable",
                         "Dist_Den",
                         "Dist_Water", 
                         "Dist_Perimeter", 
                         "Dist_Trees", 
                         "Dist_Shrub", 
                         "Dist_Open_Plus_Shrub", 
                         "Dist_Open_No_Shrub", 
                         "Slope", 
                         "Elevation", 
                         "Edge_Density_Open_Plus_Shrub", 
                         "Edge_Density_Open_No_Shrub", 
                         "Edge_Density_Shrubs", 
                         "Edge_Density_Trees", 
                         "Perc_Shrub_12.5", 
                         "Perc_Shrub_25",
                         "Perc_Shrub_50", 
                         "Perc_Shrub_100", 
                         "Perc_Shrub_250", 
                         "Perc_Shrub_500", 
                         "Perc_Tree_12.5", 
                         "Perc_Tree_25",
                         "Perc_Tree_50", 
                         "Perc_Tree_100", 
                         "Perc_Tree_250", 
                         "Perc_Tree_500",
                         "Perc_Water_12.5", 
                         "Perc_Water_25",
                         "Perc_Water_50", 
                         "Perc_Water_100", 
                         "Perc_Water_250", 
                         "Perc_Water_500",
                         "Perc_Open_12.5", 
                         "Perc_Open_25",
                         "Perc_Open_50", 
                         "Perc_Open_100", 
                         "Perc_Open_250", 
                         "Perc_Open_500", 
                         "Dist_Road", 
                         "EINP_rd_de", 
                         "South", 
                         "East")
         
#Summarise t-test results for covariate values
YYY <- third %>%
     dplyr::select(Use,Dist_Den, Dist_Water, Dist_Perimeter, Dist_Trees, Dist_Shrub, Dist_Open_Plus_Shrub, Dist_Open_No_Shrub, Slope, Elevation, Edge_Density_Open_Plus_Shrub, Edge_Density_Open_No_Shrub, Edge_Density_Shrubs, Edge_Density_Trees, Perc_Shrub_12.5, Perc_Shrub_25, Perc_Shrub_50, Perc_Shrub_100, Perc_Shrub_250, Perc_Shrub_500, Perc_Tree_12.5, Perc_Tree_25, Perc_Tree_50, Perc_Tree_100, Perc_Tree_250, Perc_Tree_500, Perc_Water_12.5, Perc_Water_25, Perc_Water_50, Perc_Water_100, Perc_Water_250, Perc_Water_500, Perc_Open_12.5, Perc_Open_25, Perc_Open_50, Perc_Open_100, Perc_Open_250, Perc_Open_500, Dist_Road, EINP_rd_de, South, East) %>%
     gather(key = variable, value = value, -Use) %>% 
     group_by(Use, variable) %>% 
     summarise(value = list(value)) %>%
     spread(Use, value) %>% 
     group_by(variable) %>%
     mutate(p_value = t.test(unlist(AVAIL), unlist(USED))$p.value,
                  t_value = t.test(unlist(AVAIL), unlist(USED))$statistic)

         
#write this to a table
third_ttesttable <- YYY[,-c(2:3)]
head(third_ttesttable)

##Now combine t-test results table and summary table
third_summary_Use <- data.frame(t(dplyr::filter(third_summary, Use == "USED")))
         
colnames(third_summary_Use) <- as.character(third_summary_Use[2,])
         
third_summary_Use <- third_summary_Use[-c(1:2),]
         
third_summary_Use <- tibble::rownames_to_column(third_summary_Use, var = "variable")
         
third_summary_Avail <- data.frame(t(dplyr::filter(third_summary, Use == "AVAIL")))
         
colnames(third_summary_Avail) <- as.character(third_summary_Avail[2,])
         
third_summary_Avail <- third_summary_Avail[-c(1:2),]
         
third_summary_Avail <- tibble::rownames_to_column(third_summary_Avail, var = "variable")
         
#Join three tables (i.e., Use, Avail, T-test results)
final_table_third <- third_ttesttable %>%
     left_join(third_summary_Avail, by = "variable") %>%
     left_join(third_summary_Use, by = "variable")
         
         
#reorder columns
final_table_third <- final_table_third %>%
     dplyr::select(mean.y, sd.y, min.y, max.y, mean.x, sd.x, min.x, max.x, t_value, p_value)
         
#rename columns
final_table_third <- final_table_third %>%
     rename("Variable" = variable, "Mean" = mean.y, "SD" = sd.y, "Min" = min.y, "Max" = max.y, "MeanA" = mean.x, "SDA" = sd.x, "MinA" = min.x, "MaxA" = max.x, "t-stat" = t_value, "p-value" = p_value)
         
#reorder from smallest to largest p values
final_table_third <- final_table_third %>%
       arrange(`p-value`)
         
#replace p values < 0.001 with "<0.001"
final_table_third$`p-value`[(final_table_third$`p-value` <= 0.001)] <- "<0.001"
         
#There are a few variables that are not relevant (shrubs, open no shrubs). Remove them:
final_table_third <- final_table_third %>%
           dplyr::filter(Variable != "Edge_Density_Open_No_Shrub",
                         Variable != "Perc_Shrub_50",
                         Variable != "Perc_Shrub_500",
                         Variable != "Perc_Shrub_100",
                         Variable != "Dist_Open_No_Shrub",
                         Variable != "Perc_Shrub_250",
                         Variable != "Edge_Density_Shrubs",
                         Variable != "Dist_Shrub",
                         Variable != "Perc_Shrub_25",
                         Variable != "Perc_Shrub_12.5")
         
#Final list is the usual suspects, dist water, dist trees, dist open (incl. shrub)
#Also density of tree edges and density of open (incl. shrub) edges
# also perc cover of WATER, TREES, OPEN (incl, shrubs)
         
#Write to CSV
#write.csv(final_table_third, file = "T.Test.Summary.Table.Third.csv")
         
#remove unecessary stuff
remove(final_table_third)
remove(third_summary)
remove(third_summary_Avail)
remove(third_summary_Use)
remove(third_ttesttable)
remove(YYY)

         
         
#Read in Fourth order Data
fourth <- read.csv("FourthOrderData.csv")
         
#Read in fourth order missing piece
Missing <- read.csv("Missing_Worm_Diameter.csv")
         
         
#Replace missing word diameter values:
head(Missing)
         
Missing <- Missing %>% 
   select(Worm_diameter_mm, Location, U_or_A)
         
Missing$Unique_ID <- paste(Missing$Location, Missing$U_or_A, sep="") 
         
Missing <- Missing %>% 
   select(Worm_diameter_mm, Unique_ID)
         
#Join missing data with remaining fourth order data:
fourth1 <- full_join(fourth, Missing, by="Unique_ID") 
         
#Correct diameter is now in worm_diameter_mm.x. Remove other column
fourth1 <- fourth1 %>% dplyr::select(-(Worm_diameter_mm.y))
         
fourth1 <- fourth1 %>% dplyr::rename("Diam" = Worm_diameter_mm.x)
         
#Write this to a csv
#write.csv(fourth1, "FourthOrderData_Clean.csv")
         
         
#Remove files you don't need
remove(fourth)
remove(fourth1)
remove(Missing)
         
#Read in clean fourth order data
fourth <- read.csv("FourthOrderData_Clean.csv")
         
#Remove unecessary columns:
fourth <- fourth %>%
   select(-(c(Location, Unique_ID, Slope_position, Soil_Type, Organic, Beaver_sticks, AorB_count,SStructure, Garbage, Dir_P, X1_Entrance_Length, X1_Entrance_Width, X2_Entrance_Length, X2_Entrance_Width, X3_Entrance_Length,
            X3_Entrance_Width, No_Entrances)))
         
#Create aspect indices.
fourth <- fourth %>%
     mutate(AspRad = 0.0174532925 * Aspect) %>% 
     mutate(East = sin(AspRad))
         
fourth <- mutate(fourth, Interim = cos(AspRad)) %>%
     mutate(South = Interim * -1)
         
fourth <- fourth %>%
     select(-(c(Aspect, AspRad, Interim)))
         
# Develop some metrics that use multiple columns of data
#First, average slope
fourth <- fourth %>%
     dplyr::mutate(AvSlope = rowMeans(select(., Slope_Upper, Slope_Lower), na.rm = TRUE))
         
fourth <- fourth %>% dplyr::select(-(c(Slope_Upper, Slope_Lower)))
         
#Second, find mean Canopy Cover and variance
fourth <- fourth %>%
   dplyr::mutate(AvCC = rowMeans(select(., Canopy_C, Canopy_U, Canopy_R, Canopy_D,
                                                Canopy_L), na.rm = TRUE)) %>%
   dplyr::mutate(VarCC = apply(select(., Canopy_C, Canopy_U, Canopy_R, Canopy_D,
                                              Canopy_L), 1, var))
         
#Third mean hiding cover and variance
fourth <- fourth %>%
     dplyr::mutate(AvHC = rowMeans(select(., Hiding_U, Hiding_R, Hiding_D,
                                                Hiding_L), na.rm = TRUE)) %>%
     dplyr::mutate(VarHC = apply(select(., Hiding_U, Hiding_R, Hiding_D,
                                              Hiding_L), 1, var))
         
#Fourth, get difference between max and min CC
fourth <- fourth %>%
       mutate(MAXCC = pmax(Canopy_C,Canopy_U, Canopy_R, Canopy_D, Canopy_L)) %>%
       mutate(MINCC = pmin(Canopy_C,Canopy_U, Canopy_R, Canopy_D, Canopy_L)) %>%
       mutate(CCDiff = MAXCC - MINCC) %>%
           dplyr::select(-(c(MAXCC, MINCC)))
         
#Finally, diff between max and min HC
fourth <- fourth %>%
       mutate(MAXHC = pmax(Hiding_U, Hiding_R, Hiding_D,
                               Hiding_L)) %>%
       mutate(MINHC = pmin(Hiding_U, Hiding_R, Hiding_D,
                               Hiding_L)) %>%
       mutate(HCDiff = MAXHC - MINHC) %>%
           dplyr::select(-(c(MAXHC, MINHC)))
         
#Remove unecessary columns
fourth <- fourth %>%
       dplyr::select(-(c(X.1, X,
                             Canopy_C,Canopy_U, Canopy_R, Canopy_D, Canopy_L,
                             Hiding_U, Hiding_R, Hiding_D, Hiding_L)))
         
         
#Next step is to generate mean values (and t test) for all fourth order covariates
#Generate summary statistics for covariate values
fourth_summary <- fourth %>% 
   dplyr::group_by(U_or_A) %>% 
   dplyr::summarise_if(is.numeric, .funs=list(mean, sd, min, max)) %>%
           tidyr::pivot_longer(cols = -U_or_A, 
                               names_to = c('.value', 'variable'), 
                               names_sep = '_fn')
         
fourth_summary$variable[fourth_summary$variable==1] <- "mean"
fourth_summary$variable[fourth_summary$variable==2] <- "sd"
fourth_summary$variable[fourth_summary$variable==3] <- "min"
fourth_summary$variable[fourth_summary$variable==4] <- "max"
         
         
fourth_summary <- fourth_summary%>%
   dplyr::select("U_or_A",
                         "variable",
                         "Diam",
                         "Tree_perc", 
                         "Shrub_perc", 
                         "Herb_perc", 
                         "Ground_perc", 
                         "Water_perc", 
                         "East", 
                         "South", 
                         "AvSlope", 
                         "AvCC", 
                         "AvHC",
                         "VarCC",
                         "VarHC", 
                         "CCDiff", 
                         "HCDiff")

#Summarise t-test results for covariate values
YYY <- fourth %>%
 dplyr::select(U_or_A,
                 Diam,
                 Tree_perc, 
                         Shrub_perc, 
                         Herb_perc, 
                         Ground_perc, 
                         Water_perc, 
                         East, 
                         South, 
                         AvSlope, 
                         AvCC, 
                         AvHC,
                         VarCC,
                         VarHC, 
                         CCDiff, 
                         HCDiff) %>%
           gather(key = variable, value = value, -U_or_A) %>% 
           group_by(U_or_A, variable) %>% 
           summarise(value = list(value)) %>%
           spread(U_or_A, value) %>% 
           group_by(variable) %>%
           mutate(p_value = t.test(unlist(A), unlist(U))$p.value,
                  t_value = t.test(unlist(A), unlist(U))$statistic)
         
         
#write this to a table
fourth_ttesttable <- YYY[,-c(2:3)]
head(fourth_ttesttable)
         
##Now combine t-test results table and summary table
fourth_summary_Use <- data.frame(t(dplyr::filter(fourth_summary, U_or_A == "U")))
         
colnames(fourth_summary_Use) <- as.character(fourth_summary_Use[2,])
         
fourth_summary_Use <- fourth_summary_Use[-c(1:2),]
         
fourth_summary_Use <- tibble::rownames_to_column(fourth_summary_Use, var = "variable")
         
fourth_summary_Avail <- data.frame(t(dplyr::filter(fourth_summary, U_or_A == "A")))
         
colnames(fourth_summary_Avail) <- as.character(fourth_summary_Avail[2,])
         
fourth_summary_Avail <- fourth_summary_Avail[-c(1:2),]
         
fourth_summary_Avail <- tibble::rownames_to_column(fourth_summary_Avail, var = "variable")
         
#Join three tables (i.e., Use, Avail, T-test results)
final_table_fourth <- fourth_ttesttable %>%
  left_join(fourth_summary_Avail, by = "variable") %>%
  left_join(fourth_summary_Use, by = "variable")
         
         
#reorder columns
final_table_fourth <- final_table_fourth %>%
   dplyr::select(mean.y, sd.y, min.y, max.y, mean.x, sd.x, min.x, max.x, t_value, p_value)
         
#rename columns
final_table_fourth <- final_table_fourth %>%
   rename("Variable" = variable, "Mean" = mean.y, "SD" = sd.y, "Min" = min.y, "Max" = max.y, "MeanA" = mean.x, "SDA" = sd.x, "MinA" = min.x, "MaxA" = max.x, "t-stat" = t_value, "p-value" = p_value)
         
#reorder from smallest to largest p values
final_table_fourth <- final_table_fourth %>%
       arrange(`p-value`)
         
#replace p values < 0.001 with "<0.001"
final_table_fourth$`p-value`[(final_table_fourth$`p-value` <= 0.001)] <- "<0.001"
         
#Write to CSV
#write.csv(final_table_fourth, file = "T.Test.Summary.Table.Fourth.csv")
         
#remove unecessary stuff
remove(final_table_fourth)
remove(fourth_summary)
remove(fourth_summary_Avail)
remove(fourth_summary_Use)
remove(fourth_ttesttable)
remove(YYY)
         
         
#Begin univariate modelling for fourth order habitat covariates. 
fourth <- fourth %>%
   dplyr::select(U_or_A, Diam, Tree_perc, Shrub_perc, Herb_perc, Ground_perc, Water_perc, East, South, AvSlope, AvCC, VarCC, AvHC, VarHC, CCDiff, HCDiff)
         
#Code factor variable
fourth$U_or_A <- ifelse(fourth$U_or_A == "U", 1, 0)
fourth$U_or_A <- factor(fourth$U_or_A, levels=c(0,1))
         
#confirm all are numeric
str(fourth)
fourth$Tree_perc <- as.numeric(fourth$Tree_perc)
fourth$Shrub_perc <- as.numeric(fourth$Shrub_perc)
fourth$Herb_perc <- as.numeric(fourth$Herb_perc)
fourth$Ground_perc <- as.numeric(fourth$Ground_perc)
fourth$Water_perc <- as.numeric(fourth$Water_perc)
fourth$CCDiff <- as.numeric(fourth$CCDiff)
         
#Next we need to scale and center variables
#Save an unscaled copy for later use
fourth_unscaled <- fourth
         
# Now scale and center
fourth[, sapply(fourth, is.numeric)] <- scale(fourth[, sapply(fourth, is.numeric)])
         
#Check for covariates with limited variation:
iqr_values <- apply(fourth, 2, function(x) IQR(x, na.rm = TRUE))
iqr_values #REMOVE WATER


#Complete univariate modelling
Mods_Lin <- list()
         for(i in c(1:14)) {
           if(i==1){var <- ("Diam")}
           if(i==2){var <- ("Tree_perc")}
           if(i==3){var <- ("Shrub_perc")}
           if(i==4){var <- ("Herb_perc")}
           if(i==5){var <- ("Ground_perc")}
           if(i==6){var <- ("East")}
           if(i==7){var <- ("South")}
           if(i==8){var <- ("AvSlope")}
           if(i==9){var <- ("AvCC")}
           if(i==10){var <- ("VarCC")}
           if(i==11){var <- ("CCDiff")}
           if(i==12){var <- ("AvHC")}
           if(i==13){var <- ("VarHC")}
           if(i==14){var <- ("HCDiff")}
           
           
           Mods_Lin[[i]] <- data.frame()
           
           # For every variable you want to test:
           for(j in c(var)){
             
             model <- glm(U_or_A ~ fourth[,j],
                          data=fourth, family = binomial)
             
             Mods_Lin[[i]][j,1] <- j # First column is the predictor
             Mods_Lin[[i]][j,2] <- AIC(model) # Second column is AIC
             Mods_Lin[[i]][j,3:4] <- model$coefficients
             Mods_Lin[[i]][j,5:6] <- coef(summary(model))[,4] 
             Mods_Lin[[i]][j,7:10] <- confint(model)
             Mods_Lin[[i]][j,11] <- PseudoR2(model, which = "McFadden") 
             Mods_Lin[[i]][j,12] <- PseudoR2(model, which = "Nagelkerke")
             Mods_Lin[[i]][j,13] <- nobs(model)
             
           }
           
           colnames(Mods_Lin[[i]]) <- c("predictor", "AIC", "Intercept_Beta", "Predictor_beta",
                                        "Intercept_pval", "Predictor_pval", "Intercept_LowerCI", 
                                        "Predictor_LowerCI", "Intercept_UpperCI", "Predictor_UpperCI",
                                        "McFaddenR2", "NagelkerkeR2", "sample_size")
         }
         Mods_Lin_Univariates <- dplyr::bind_rows(Mods_Lin)
         
         Mods_Lin_Univariates <- Mods_Lin_Univariates %>%
           dplyr::select(predictor, AIC, Predictor_beta, Predictor_pval, Predictor_LowerCI,
                         Predictor_UpperCI, McFaddenR2, NagelkerkeR2, sample_size) %>%
           dplyr::rename("Beta" = Predictor_beta,
                         "pvalue" = Predictor_pval,
                         "LowCI" = Predictor_LowerCI,
                         "UppCI" = Predictor_UpperCI)
         
         
#Test variables using a quadratic distribution:
Mods_quad <- list()
         for(i in c(1:14)) {
           if(i==1){var <- ("Diam")}
           if(i==2){var <- ("Tree_perc")}
           if(i==3){var <- ("Shrub_perc")}
           if(i==4){var <- ("Herb_perc")}
           if(i==5){var <- ("Ground_perc")}
           if(i==6){var <- ("East")}
           if(i==7){var <- ("South")}
           if(i==8){var <- ("AvSlope")}
           if(i==9){var <- ("AvCC")}
           if(i==10){var <- ("VarCC")}
           if(i==11){var <- ("CCDiff")}
           if(i==12){var <- ("AvHC")}
           if(i==13){var <- ("VarHC")}
           if(i==14){var <- ("HCDiff")}

           
           # Create a data frame to store the results from all the models.
           Mods_quad[[i]] <- data.frame()
           
           # For every variable you want to test:
           for(j in c(var)){
             
             model <- glm(U_or_A ~ fourth[,j] + I(fourth[,j]^2),
                          data=fourth, family = binomial)
             
             Mods_quad[[i]][j,1] <- j
             Mods_quad[[i]][j,2] <- AIC(model)
             Mods_quad[[i]][j,3:5] <- model$coefficients
             Mods_quad[[i]][j,6:8] <- coef(summary(model))[,4]
             Mods_quad[[i]][j,9:14] <- confint(model)
             Mods_quad[[i]][j,15] <- PseudoR2(model, which = "McFadden")
             Mods_quad[[i]][j,16] <- PseudoR2(model, which = "Nagelkerke")
           }
           
           colnames(Mods_quad[[i]]) <- c("predictor", "AIC", "Intercept_Beta", 
                                         "Predictor_beta", "Predictor_beta2", 
                                         "Intercept_pval", "Predictor_pval", 
                                         "Predictor_pval2", "Intercept_LowerCI", 
                                         "Predictor_LowerCI", "Predictor_LowerCI2",
                                         "Intercept_UpperCI", "Predictor_UpperCI",
                                         "Predictor_UpperCI2", "McFR2", "NagR2")
         }
         Mods_quad_Univariates <- dplyr::bind_rows(Mods_quad)
         
         Mods_quad_Univariates <- Mods_quad_Univariates %>%
           dplyr::select(predictor, AIC, Predictor_beta, Predictor_beta2,
           Predictor_pval, Predictor_pval2, Predictor_LowerCI, Predictor_UpperCI,
         Predictor_LowerCI2, Predictor_UpperCI2, McFR2, NagR2) %>%
  dplyr::rename("Q_Beta(lin)" = Predictor_beta,
                "Q_Beta(quad)" = Predictor_beta2,
                "Q_pvalue(lin)" = Predictor_pval,
                "Q_pvalue(quad)" = Predictor_pval2,
                "Q_LowCI(lin)" = Predictor_LowerCI,
                "Q_UppCI(lin)" = Predictor_UpperCI,
                "Q_LowCI(quad)" = Predictor_LowerCI2,
                "Q_UppCI(quad)" = Predictor_UpperCI2,
                "AIC_quad" = AIC)


#Print tables:
#write.csv(Mods_Lin_Univariates, file = "LinearUnivariates_FourthOrder.csv")
#write.csv(Mods_quad_Univariates, file = "QuadraticUnivariates_FourthOrder1.csv")
     
#RETAIN: worm diameter, tree, water, avg slope, hiding cover Var, HC difference, CC Diff as LINEAR Terms
#RETAIN: perc shrub, herb, and ground, AvgHC as quadratic

#Create a new dataframe with only the predictors we want to keep.
fourth <- fourth %>%
  dplyr::select(U_or_A, Diam, Tree_perc, Shrub_perc, Herb_perc, Ground_perc, 
                Water_perc, AvSlope, HCDiff, AvHC, VarHC)

#Test for correlations among the predictors;If a pair of variables has r > 0.6, that’s a problem!
cor(fourth [sapply(fourth , is.numeric)], method = c("pearson"), use = "pairwise.complete.obs")

#Correlated variables: shrub+herb, HCDiff and HCVar


#Addressing NAs in the data by replacing with mean for that group (VarHC (USE), HCDiff(use)):
fourth <- fourth %>%
  group_by(U_or_A) %>%
  mutate(VarHC = ifelse(is.na(VarHC) & U_or_A == 1, mean(VarHC, na.rm = TRUE), VarHC)) %>%
  ungroup()

fourth <- fourth %>%
  group_by(U_or_A) %>%
  mutate(HCDiff = ifelse(is.na(HCDiff) & U_or_A == 1, mean(HCDiff, na.rm = TRUE), HCDiff)) %>%
  ungroup()

fourth <- as.data.frame(fourth)

#Build global model:
#List ALL predictors we want to include
AllSubsMod <- glm(U_or_A~ Diam + Tree_perc + Water_perc + AvSlope + VarHC + HCDiff +
                    Shrub_perc + I(Shrub_perc^2) + Herb_perc + I(Herb_perc^2) + 
                    Ground_perc + I(Ground_perc^2) + AvHC +
                    I(AvHC^2),
                  data = fourth,
                  family = "binomial", na.action = "na.fail")

#create subset
subsetfourth <- expression(dc(`Shrub_perc`, `I(Shrub_perc^2)`) &
                         dc(`Herb_perc`, `I(Herb_perc^2)`) &
                           dc(`Ground_perc`, `I(Ground_perc^2)`) &
                         dc(`AvHC`, `I(AvHC^2)`))

AllSubsMod_Results <-dredge(AllSubsMod , subset= subsetfourth , trace = 2)
AllSubsMod_Results 

#Exmaine model results. Recall #Correlated variables: shrub+herb, HCDiff and HCVar
#There ARE 4 models within 2AIC
GLM1 <- get.models(AllSubsMod_Results, 1)[[1]]
GLM2 <- get.models(AllSubsMod_Results, 2)[[1]]
GLM3 <- get.models(AllSubsMod_Results, 3)[[1]]
GLM4 <- get.models(AllSubsMod_Results, 4)[[1]]

GLM1 <- glm(formula = U_or_A ~ AvSlope + Diam + Shrub_perc + Tree_perc + 
                       1, family = "binomial", data = fourth, na.action = "na.fail")
GLM2 <- glm(formula = U_or_A ~ AvSlope + Diam + Ground_perc + Shrub_perc + 
                       Tree_perc + 1, family = "binomial", data = fourth, na.action = "na.fail")
GLM3 <- glm(formula = U_or_A ~ AvSlope + Diam + Herb_perc + Tree_perc + 
                       1, family = "binomial", data = fourth, na.action = "na.fail")
GLM4 <- glm(formula = U_or_A ~ AvSlope + Diam + Shrub_perc + 1, family = "binomial", 
                     data = fourth, na.action = "na.fail")


summary(GLM1)
summary(GLM2)
summary(GLM3)
summary(GLM4)

#Model averaging
GLMavg <- model.avg(GLM1, GLM2, GLM3, GLM4)
summary(GLMavg)
confint(GLMavg) 

#Preparing FOW_unscaled for modeling.
fourth_unscaled <- fourth_unscaled %>%
  dplyr::select(U_or_A, AvSlope, Diam, Shrub_perc, Tree_perc, Ground_perc, Herb_perc)

fourth_unscaled <- fourth_unscaled %>%
  group_by(U_or_A) %>%
  mutate(AvSlope = ifelse(is.na(AvSlope) & U_or_A == 0, mean(AvSlope, na.rm = TRUE), AvSlope)) %>%
  ungroup()

#Effect sizes
GLM1$call
GLM1_unscaled <- glm(formula = U_or_A ~ AvSlope + Diam + Shrub_perc + Tree_perc + 
                       1, family = "binomial", data = fourth_unscaled, na.action = "na.fail")
summary(GLM1_unscaled)

GLM2$call
GLM2_unscaled <- glm(formula = U_or_A ~ AvSlope + Diam + Ground_perc + Shrub_perc + 
                       Tree_perc + 1, family = "binomial", data = fourth_unscaled, na.action = "na.fail")
summary(GLM2_unscaled)

GLM3$call
GLM3_unscaled <- glm(formula = U_or_A ~ AvSlope + Diam + Herb_perc + Tree_perc + 
                       1, family = "binomial", data = fourth_unscaled, na.action = "na.fail")
summary(GLM3_unscaled)

GLM4$call
GLM4_unscaled <- glm(formula = U_or_A ~ AvSlope + Diam + Shrub_perc + 1, family = "binomial", 
                     data = fourth_unscaled, na.action = "na.fail")
summary(GLM4_unscaled)



#Model average
GLMavg_unscaled <- model.avg(GLM1_unscaled, GLM2_unscaled, GLM3_unscaled, GLM4_unscaled)
exp(GLMavg_unscaled$coefficients) #this gives you the odds ratio
exp(confint(GLMavg_unscaled)) #this gives you the CI for the odds ratio

#Check our model performance
#First, collinearity using variance inflation factors (VIF), and record the highest VIF.
check_collinearity(GLM1) #1.29
check_collinearity(GLM2) #1.62
check_collinearity(GLM3) #1.37
check_collinearity(GLM4) #1.23

#Determine McFadden and Nagelkerke Pseudo R2, using a hastage to report the value.
PseudoR2(GLM1, which = "Nagelkerke") #0.7680382 
PseudoR2(GLM1, which = "McFadden") #0.6189806 

PseudoR2(GLM2, which = "Nagelkerke") #0.7791387 
PseudoR2(GLM2, which = "McFadden") #0.6332864 

PseudoR2(GLM3, which = "Nagelkerke") #0.7589369 
PseudoR2(GLM3, which = "McFadden") #0.6074594 

PseudoR2(GLM4, which = "Nagelkerke") #0.7412174 
PseudoR2(GLM4, which = "McFadden") #0.585543 

#Mean Nag: 0.7618328
#Mean McFadd: 0.61131735


#Receiver operator characteristic area under the curve
roc.GLM1 <- roc(fourth$U_or_A~fitted(GLM1))
auc(roc.GLM1) #0.9518

roc.GLM2 <- roc(fourth$U_or_A~fitted(GLM2))
auc(roc.GLM2) #0.9541

roc.GLM3 <- roc(fourth$U_or_A~fitted(GLM3))
auc(roc.GLM3) #0.9507

roc.GLM4 <- roc(fourth$U_or_A~fitted(GLM4))
auc(roc.GLM4) #0.949
#Av ROC is 0.9514

#Plot coefficients
plot_summs(GLM1, GLM2, GLM3, GLM4)

#Improve the plot
GLMPlotSumm <- plot_summs(GLM1, GLM2, GLM3, GLM4,
                          colors = c("#009688", "#009688", "#009688", "#009688"),
                          point.size = 5,
                          point.shape = FALSE,
                          coefs = c("Slope (%)" = "AvSlope", #name your variables
                                    "Soil Worm Diameter (mm)" = "Diam",
                                    "Shrub Cover (%)" = "Shrub_perc",
                                    "Tree Cover (%)" = "Tree_perc",
                                    "Herb Cover (%)" = "Herb_perc",
                                    "Ground Cover (%)" = "Ground_perc")) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  ggtitle("(A) Coefficient Estimates")

#Plotting our effect sizes
Pa <- plot_model(GLMavg_unscaled, type = "pred", terms = "AvSlope", show.values = TRUE, axis.lim = c(0, 1))
Pb <- plot_model(GLMavg_unscaled, type = "pred", terms = "Diam", show.values = TRUE, axis.lim = c(0, 1))

#Make prettier
Pa1<- Pa + theme_classic() + ggtitle("(B) Effect of Slope on Probability of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Slope (%)", y = "Probability of Den")
Pa1

Pb1<- Pb + theme_classic() + ggtitle("(C) Effect of Soil Worm Diameter on Likelihood of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Soil Worm Diameter (mm)", y = "Probability of Den")
Pb1

#Make a combined plot:
AA <- ggarrange(Pa1, Pb1, ncol = 1, nrow =2)
BB <- ggarrange(GLMPlotSumm, AA, ncol = 2)
#ggsave("FourthOrderCharts.png", BB, width = 12, height = 8, dpi = 700,  bg = "white") 

#remove relevant things
remove(AA)
remove(AllSubsMod)
remove(AllSubsMod_Results)
remove(BB)
remove(fourth)
remove(fourth_unscaled)
remove(GLM1)
remove(GLM2)
remove(GLM3)
remove(GLM4)
remove(GLM1_unscaled)
remove(GLM2_unscaled)
remove(GLM3_unscaled)
remove(GLM4_unscaled)
remove(GLMPlotSumm)
remove(Pa)
remove(Pa1)
remove(Pb)
remove(Pb1)
remove(roc.GLM1)
remove(roc.GLM2)
remove(roc.GLM3)
remove(roc.GLM4)
remove(subsetfourth)
remove(GLMavg)
remove(GLMavg_unscaled)

#Modelling for third order characteristics
third$Use <- ifelse(third$Use== "USED", 0, 1)
third$Use <- factor(third$Use, levels=c(0,1))

#remove terms that we do not plan on using:
third <- third %>% 
  dplyr::select(-(c(Dist_Shrub, Dist_Open_No_Shrub, Edge_Density_Open_No_Shrub,
                    Edge_Density_Shrubs, Perc_Shrub_12.5, Perc_Shrub_25,
                    Perc_Shrub_50, Perc_Shrub_100, Perc_Shrub_250, Perc_Shrub_500)))
                
                
#Transform road density from km/km2 to m/m2
third$EINP_rd_de <- third$EINP_rd_de*1000
third$Edge_Density_Open_Plus_Shrub <- third$Edge_Density_Open_Plus_Shrub*1000
third$Edge_Density_Trees <- third$Edge_Density_Trees*1000

#Make decay terms, several, for each distance metric. Then choose the best one
third <- third %>%
  mutate(Dec_Den0.002 = 1-(exp(-0.002*Dist_Den))) %>%
  mutate(Dec_Den0.004 = 1-(exp(-0.004*Dist_Den))) %>%
  mutate(Dec_Den0.006 = 1-(exp(-0.006*Dist_Den))) %>%
  mutate(Dec_Den0.012 = 1-(exp(-0.012*Dist_Den))) %>%
  mutate(Dec_Den0.03 = 1-(exp(-0.03*Dist_Den))) %>%
  mutate(Dec_Den0.06 = 1-(exp(-0.06*Dist_Den))) %>%
  mutate(Dec_Den0.2 = 1-(exp(-0.2*Dist_Den))) %>%
  mutate(Dec_Water0.002 = 1-(exp(-0.002*Dist_Water))) %>%
  mutate(Dec_Water0.004 = 1-(exp(-0.004*Dist_Water))) %>%
  mutate(Dec_Water0.006 = 1-(exp(-0.006*Dist_Water))) %>%
  mutate(Dec_Water0.012 = 1-(exp(-0.012*Dist_Water))) %>%
  mutate(Dec_Water0.03 = 1-(exp(-0.03*Dist_Water))) %>%
  mutate(Dec_Water0.06 = 1-(exp(-0.06*Dist_Water))) %>%
  mutate(Dec_Water0.2 = 1-(exp(-0.2*Dist_Water))) %>%
  mutate(Dec_Perimeter0.002 = 1-(exp(-0.002*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.004 = 1-(exp(-0.004*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.006 = 1-(exp(-0.006*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.012 = 1-(exp(-0.012*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.03 = 1-(exp(-0.03*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.06 = 1-(exp(-0.06*Dist_Perimeter))) %>%
  mutate(Dec_Perimeter0.2 = 1-(exp(-0.2*Dist_Perimeter))) %>%
  mutate(Dec_Trees0.002 = 1-(exp(-0.002*Dist_Trees))) %>%
  mutate(Dec_Trees0.004 = 1-(exp(-0.004*Dist_Trees))) %>%
  mutate(Dec_Trees0.006 = 1-(exp(-0.006*Dist_Trees))) %>%
  mutate(Dec_Trees0.012 = 1-(exp(-0.012*Dist_Trees))) %>%
  mutate(Dec_Trees0.03 = 1-(exp(-0.03*Dist_Trees))) %>%
  mutate(Dec_Trees0.06 = 1-(exp(-0.06*Dist_Trees))) %>%
  mutate(Dec_Trees0.2 = 1-(exp(-0.2*Dist_Trees))) %>%
  mutate(Dec_Open_Plus_Shrub0.002 = 1-(exp(-0.002*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.004 = 1-(exp(-0.004*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.006 = 1-(exp(-0.006*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.012 = 1-(exp(-0.012*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.03 = 1-(exp(-0.03*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.06 = 1-(exp(-0.06*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Open_Plus_Shrub0.2 = 1-(exp(-0.2*Dist_Open_Plus_Shrub))) %>%
  mutate(Dec_Road0.002 = 1-(exp(-0.002*Dist_Road))) %>%
  mutate(Dec_Road0.004 = 1-(exp(-0.004*Dist_Road))) %>%
  mutate(Dec_Road0.006 = 1-(exp(-0.006*Dist_Road))) %>%
  mutate(Dec_Road0.012 = 1-(exp(-0.012*Dist_Road))) %>%
  mutate(Dec_Road0.03 = 1-(exp(-0.03*Dist_Road))) %>%
  mutate(Dec_Road0.06 = 1-(exp(-0.06*Dist_Road))) %>%
  mutate(Dec_Road0.2 = 1-(exp(-0.2*Dist_Road)))

Decmods <- list()
for(i in c(1:6)) {
  if(i==1){decs <- c("Dist_Den", "Dec_Den0.002", "Dec_Den0.004", "Dec_Den0.006", "Dec_Den0.012", "Dec_Den0.03", "Dec_Den0.06", "Dec_Den0.2")}
  if(i==2){decs <- c("Dist_Water", "Dec_Water0.002", "Dec_Water0.004", "Dec_Water0.006", "Dec_Water0.012", "Dec_Water0.03", "Dec_Water0.06", "Dec_Water0.2")}
  if(i==3){decs <- c("Dist_Perimeter", "Dec_Perimeter0.002", "Dec_Perimeter0.004", "Dec_Perimeter0.006", "Dec_Perimeter0.012", "Dec_Perimeter0.03", "Dec_Perimeter0.06", "Dec_Perimeter0.2")}
  if(i==4){decs <- c("Dist_Trees", "Dec_Trees0.002", "Dec_Trees0.004", "Dec_Trees0.006", "Dec_Trees0.012", "Dec_Trees0.03", "Dec_Trees0.06", "Dec_Trees0.2")}
  if(i==5){decs <- c("Dist_Open_Plus_Shrub", "Dec_Open_Plus_Shrub0.002", "Dec_Open_Plus_Shrub0.004", "Dec_Open_Plus_Shrub0.006", "Dec_Open_Plus_Shrub0.012", "Dec_Open_Plus_Shrub0.03", "Dec_Open_Plus_Shrub0.06", "Dec_Open_Plus_Shrub0.2")}
  if(i==6){decs <- c("Dist_Road", "Dec_Road0.002", "Dec_Road0.004", "Dec_Road0.006", "Dec_Road0.012", "Dec_Road0.03", "Dec_Road0.06", "Dec_Road0.2")}
  # Create a data frame to store the results from all the models.
  Decmods[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(decs)){
    
    model <- glm(Use ~ third[,j], # Create the predictive model
                 data=third, family = binomial)
    
    null <- glm(Use ~1, # Create the null model
                data=third, family = binomial)
    
    
    Decmods[[i]][j,1] <- j # First column is the predictor
    Decmods[[i]][j,2] <- AIC(model) # Second column is AIC
    Decmods[[i]][j,3] <- AIC(null)
  }
  
  colnames(Decmods[[i]]) <- c("predictor", "AIC", "AIC_null")
}
DecDecayResults <- dplyr::bind_rows(Decmods)

#Retain most explanatory decay term for each distance metric
third <- third %>%
  dplyr::select(Use, Dist_Trees, Dist_Open_Plus_Shrub, Slope, Elevation, 
                Edge_Density_Open_Plus_Shrub, Edge_Density_Trees, 
                Perc_Tree_12.5, Perc_Tree_25, Perc_Tree_50, Perc_Tree_100, 
                Perc_Tree_250, Perc_Tree_500, Perc_Water_12.5, Perc_Water_25, 
                Perc_Water_50, Perc_Water_100, Perc_Water_250, Perc_Water_500, 
                Perc_Open_12.5, Perc_Open_25, Perc_Open_50, Perc_Open_100, 
                Perc_Open_250, Perc_Open_500, Dist_Road, EINP_rd_de, East, 
                South, Dec_Den0.004, Dec_Water0.06, Dec_Perimeter0.006, Dec_Road0.06)

third <- third %>%
  rename("Dec_Den" = Dec_Den0.004, 
         "Dec_Water" = Dec_Water0.06, 
         "Dec_Perimeter" = Dec_Perimeter0.006, 
         "Dec_Road" = Dec_Road0.06)

#Remove decay dataframes
remove(DecDecayResults)
remove(Decmods)
remove(decs)


#Scale and center our data
third_unscaled <- third
third[, sapply(third, is.numeric)] <- scale(third[, sapply(third, is.numeric)])

#Check for low variation
iqr_values <- apply(third, 2, function(x) IQR(x, na.rm = TRUE))
iqr_values #remove dec_road

#Develop univariate  models as quadratic and linear
Mods_Lin <- list()
for(i in c(1:30)) {
  if(i==1){var <- ("Dist_Trees")}
  if(i==2){var <- ("Dist_Open_Plus_Shrub")}
  if(i==3){var <- ("Slope")}
  if(i==4){var <- ("Elevation")}
  if(i==5){var <- ("Edge_Density_Open_Plus_Shrub")}
  if(i==6){var <- ("Edge_Density_Trees")}
  if(i==7){var <- ("Perc_Tree_12.5")}
  if(i==8){var <- ("Perc_Tree_25")}
  if(i==9){var <- ("Perc_Tree_50")}
  if(i==10){var <- ("Perc_Tree_100")}
  if(i==11){var <- ("Perc_Tree_250")}
  if(i==12){var <- ("Perc_Tree_500")}
  if(i==13){var <- ("Perc_Water_12.5")}
  if(i==14){var <- ("Perc_Water_25")}
  if(i==15){var <- ("Perc_Water_50")}
  if(i==16){var <- ("Perc_Water_100")}
  if(i==17){var <- ("Perc_Water_250")}
  if(i==18){var <- ("Perc_Water_500")}
  if(i==19){var <- ("Perc_Open_12.5")}
  if(i==20){var <- ("Perc_Open_25")}
  if(i==21){var <- ("Perc_Open_50")}
  if(i==22){var <- ("Perc_Open_100")}
  if(i==23){var <- ("Perc_Open_250")}
  if(i==24){var <- ("Perc_Open_500")}
  if(i==25){var <- ("EINP_rd_de")}
  if(i==26){var <- ("East")}
  if(i==27){var <- ("South")}
  if(i==28){var <- ("Dec_Den")}
  if(i==29){var <- ("Dec_Water")}
  if(i==30){var <- ("Dec_Perimeter")}

  
  Mods_Lin[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(var)){
    
    model <- glm(Use ~ third[,j],
                 data=third, family = binomial)
    
    Mods_Lin[[i]][j,1] <- j # First column is the predictor
    Mods_Lin[[i]][j,2] <- AIC(model) # Second column is AIC
    Mods_Lin[[i]][j,3:4] <- model$coefficients
    Mods_Lin[[i]][j,5:6] <- coef(summary(model))[,4] 
    Mods_Lin[[i]][j,7:10] <- confint(model)
    Mods_Lin[[i]][j,11] <- PseudoR2(model, which = "McFadden") 
    Mods_Lin[[i]][j,12] <- PseudoR2(model, which = "Nagelkerke")
    Mods_Lin[[i]][j,13] <- nobs(model)
    
  }
  
  colnames(Mods_Lin[[i]]) <- c("predictor", "AIC", "Intercept_Beta", "Predictor_beta",
                               "Intercept_pval", "Predictor_pval", "Intercept_LowerCI", 
                               "Predictor_LowerCI", "Intercept_UpperCI", "Predictor_UpperCI",
                               "McFaddenR2", "NagelkerkeR2", "sample_size")
}
Mods_Lin_Univariates <- dplyr::bind_rows(Mods_Lin)

Mods_Lin_Univariates <- Mods_Lin_Univariates %>%
  dplyr::select(predictor, AIC, Predictor_beta, Predictor_pval, Predictor_LowerCI,
                Predictor_UpperCI, McFaddenR2, NagelkerkeR2, sample_size) %>%
  dplyr::rename("Beta" = Predictor_beta,
                "pvalue" = Predictor_pval,
                "LowCI" = Predictor_LowerCI,
                "UppCI" = Predictor_UpperCI)

#Repeat for quadratics
mods_quad <- list()
for(i in c(1:30)) {
  if(i==1){var <- ("Dist_Trees")}
  if(i==2){var <- ("Dist_Open_Plus_Shrub")}
  if(i==3){var <- ("Slope")}
  if(i==4){var <- ("Elevation")}
  if(i==5){var <- ("Edge_Density_Open_Plus_Shrub")}
  if(i==6){var <- ("Edge_Density_Trees")}
  if(i==7){var <- ("Perc_Tree_12.5")}
  if(i==8){var <- ("Perc_Tree_25")}
  if(i==9){var <- ("Perc_Tree_50")}
  if(i==10){var <- ("Perc_Tree_100")}
  if(i==11){var <- ("Perc_Tree_250")}
  if(i==12){var <- ("Perc_Tree_500")}
  if(i==13){var <- ("Perc_Water_12.5")}
  if(i==14){var <- ("Perc_Water_25")}
  if(i==15){var <- ("Perc_Water_50")}
  if(i==16){var <- ("Perc_Water_100")}
  if(i==17){var <- ("Perc_Water_250")}
  if(i==18){var <- ("Perc_Water_500")}
  if(i==19){var <- ("Perc_Open_12.5")}
  if(i==20){var <- ("Perc_Open_25")}
  if(i==21){var <- ("Perc_Open_50")}
  if(i==22){var <- ("Perc_Open_100")}
  if(i==23){var <- ("Perc_Open_250")}
  if(i==24){var <- ("Perc_Open_500")}
  if(i==25){var <- ("EINP_rd_de")}
  if(i==26){var <- ("East")}
  if(i==27){var <- ("South")}
  if(i==28){var <- ("Dec_Den")}
  if(i==29){var <- ("Dec_Water")}
  if(i==30){var <- ("Dec_Perimeter")}

  #Create a data frame to store the results from all the models.
  mods_quad[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(var)){
    
    model <- glm(Use ~ third[,j] + I(third[,j]^2), # Create the predictive model
                 data=third, family = binomial)
    
    mods_quad[[i]][j,1] <- j # First column is the predictor
    mods_quad[[i]][j,2] <- AIC(model) # Second column is AIC
    mods_quad[[i]][j,3:5] <- model$coefficients # third, fourth,fifth column are beta for intercept and predictor, predictor^2
    mods_quad[[i]][j,6:8] <- coef(summary(model))[,4] # fifth and sixth column are p values for intercept and predictor
    mods_quad[[i]][j,9:14] <- confint(model) # fifth and sixth column are p values for intercept and predictor
    mods_quad[[i]][j,15] <- PseudoR2(model, which = "McFadden")
    mods_quad[[i]][j,16] <- PseudoR2(model, which = "Nagelkerke")
    
    
    
  }
  
  colnames(mods_quad[[i]]) <- c("predictor", "AIC", "Intercept_Beta", 
                                    "Predictor_beta", "Predictore_beta2", 
                                    "Intercept_pval", "Predictor_pval", 
                                    "Predictor_pval2", "Intercept_LowerCI", 
                                    "Predictor_LowerCI", "Predictor_LowerCI2",
                                    "Intercept_UpperCI", "Predictor_UpperCI",
                                    "Predictor_UpperCI2", "McFaddR2", "NagR2")
}
Mods_quad_Univariates <- dplyr::bind_rows(mods_quad)

Mods_quad_Univariates <- Mods_quad_Univariates %>%
  dplyr::select(predictor, AIC, Predictor_beta, Predictore_beta2,
                Predictor_pval, Predictor_pval2, Predictor_LowerCI, Predictor_UpperCI,
                Predictor_LowerCI2, Predictor_UpperCI2, McFaddR2, NagR2) %>%
  dplyr::rename("Q_Beta(lin)" = Predictor_beta,
                "Q_Beta(quad)" = Predictore_beta2,
                "Q_pvalue(lin)" = Predictor_pval,
                "Q_pvalue(quad)" = Predictor_pval2,
                "Q_LowCI(lin)" = Predictor_LowerCI,
                "Q_UppCI(lin)" = Predictor_UpperCI,
                "Q_LowCI(quad)" = Predictor_LowerCI2,
                "Q_UppCI(quad)" = Predictor_UpperCI2,
                "AIC_quad" = AIC)

#Print results as CSV files
#write.csv(Mods_Lin_Univariates, file = "Third_Order_Lin_Univariates.csv")
#write.csv(Mods_quad_Univariates, file = "Third_Order_Quad_Univariates.csv")

#Remove dataframes
remove(model)
remove(Mods_Lin_Univariates)
remove(Mods_Lin)
remove(mods_quad)
remove(Mods_quad_Univariates)
remove(null)
remove(i)
remove(j)
remove(var)

#Remove all but variables to be retained
##final list is: linear dist_tree, Dist_open, Slope, Open edge density, water 50,
#east, south, dec den, dec water, dec boundary AND quad: elevation, tree cover, 
#open cover 250, road density
third <- third %>%
  dplyr::select(Use, Dist_Trees, Dist_Open_Plus_Shrub, Slope,
                Edge_Density_Open_Plus_Shrub, Perc_Water_50, East, South,
                Dec_Den, Dec_Water, Dec_Perimeter, Perc_Tree_100,
                Perc_Open_250, EINP_rd_de)

#Check for correlations:
cor(third [sapply(third , is.numeric)], method = c("pearson"), use = "pairwise.complete.obs")

#Correlated variables: dist_trees and dist_open_plus_shrub, perc_water_50 and dec_water

#Create subset for terms that can be quadratic (elevation, tree cover, open cover 250, road density)
subsetThird <- expression(dc(`EINP_rd_de`, `I(EINP_rd_de^2)`) &
                            dc(`Perc_Tree_100`, `I(Perc_Tree_100^2)`) &
                            dc(`Perc_Open_250`, `I(Perc_Open_250^2)`))


AllSubsetsModel <- glm(Use ~ Dist_Trees + Dist_Open_Plus_Shrub + Slope +
                         Edge_Density_Open_Plus_Shrub + Perc_Water_50 + East + South +
                         Dec_Den + Dec_Water + Dec_Perimeter + Perc_Tree_100 +
                         Perc_Open_250 + EINP_rd_de + I(EINP_rd_de^2) + I(Perc_Tree_100^2) +
                         I(Perc_Open_250^2),
                       data = third, 
                       family = "binomial", 
                       na.action = "na.fail")


AllSubsets_results <-dredge(AllSubsetsModel , subset= subsetThird , trace = 2, rank = BIC)

AllSubsets_results #7 models within 2 BIC

#Save each model so you don't have to use the dredge function again.
GLM1 <- get.models(AllSubsets_results, 1)[[1]]
GLM2 <- get.models(AllSubsets_results, 2)[[1]]
GLM3 <- get.models(AllSubsets_results, 3)[[1]]
GLM4 <- get.models(AllSubsets_results, 4)[[1]]
GLM5 <- get.models(AllSubsets_results, 5)[[1]]
GLM6 <- get.models(AllSubsets_results, 6)[[1]]
GLM7 <- get.models(AllSubsets_results, 7)[[1]]

#These are SCALED
GLM1$call #use this to get the formula for the model
GLM1 <- glm(formula = Use ~ Dec_Den + Dist_Trees + Perc_Water_50 + 1, 
            family = "binomial", data = third, na.action = "na.fail")

GLM1_unscaled <- glm(formula = Use ~ Dec_Den + Dist_Trees + Perc_Water_50 + 1, 
            family = "binomial", data = third_unscaled, na.action = "na.fail")


GLM2$call
GLM2 <- glm(formula = Use ~ Dec_Den + Perc_Tree_100 + I(Perc_Tree_100^2) + 
      Perc_Water_50 + 1, family = "binomial", data = third, na.action = "na.fail")

GLM2_unscaled <- glm(formula = Use ~ Dec_Den + Perc_Tree_100 + I(Perc_Tree_100^2) + 
              Perc_Water_50 + 1, family = "binomial", data = third_unscaled, na.action = "na.fail")


GLM3$call
GLM3 <- glm(formula = Use ~ Dec_Den + Perc_Water_50 + 1, family = "binomial", 
            data = third, na.action = "na.fail")
GLM3_unscaled <- glm(formula = Use ~ Dec_Den + Perc_Water_50 + 1, family = "binomial", 
            data = third_unscaled, na.action = "na.fail")

GLM4$call
GLM4 <- glm(formula = Use ~ Dec_Den + Dist_Open_Plus_Shrub + Perc_Water_50 + 
              1, family = "binomial", data = third, na.action = "na.fail")
GLM4_unscaled <- glm(formula = Use ~ Dec_Den + Dist_Open_Plus_Shrub + Perc_Water_50 + 
              1, family = "binomial", data = third_unscaled, na.action = "na.fail")

GLM5$call
GLM5 <- glm(formula = Use ~ Dec_Den + Dec_Water + Dist_Trees + 1, family = "binomial", 
            data = third, na.action = "na.fail")
GLM5_unscaled <- glm(formula = Use ~ Dec_Den + Dec_Water + Dist_Trees + 1, family = "binomial", 
            data = third_unscaled, na.action = "na.fail")


GLM6$call
GLM6 <- glm(formula = Use ~ Dec_Den + Dec_Water + Perc_Tree_100 + I(Perc_Tree_100^2) + 
              1, family = "binomial", data = third, na.action = "na.fail")
GLM6_unscaled <- glm(formula = Use ~ Dec_Den + Dec_Water + Perc_Tree_100 + I(Perc_Tree_100^2) + 
              1, family = "binomial", data = third_unscaled, na.action = "na.fail")

GLM7$call
GLM7 <- glm(formula = Use ~ Dec_Den + Dec_Water + 1, family = "binomial", 
           data = third, na.action = "na.fail")
GLM7_unscaled <- glm(formula = Use ~ Dec_Den + Dec_Water + 1, family = "binomial", 
           data = third_unscaled, na.action = "na.fail")

#Model averaging
GLMavg <- model.avg(GLM1, GLM2, GLM3, GLM4, GLM5, GLM6, GLM7)
summary(GLMavg)
confint(GLMavg) 

#Repeat with unscaled models
GLMavg_unscaled <- model.avg(GLM1_unscaled, GLM2_unscaled, GLM3_unscaled, GLM4_unscaled, 
                    GLM5_unscaled, GLM6_unscaled, GLM7_unscaled)

summary(GLMavg_unscaled)

#Odds Ratio
exp(GLMavg_unscaled$coefficients) 

#CIs for Odds Ratio
exp(confint(GLMavg_unscaled)) 

#Check our model performance
#First, collinearity using variance inflation factors (VIF), and record the highest VIF.
check_collinearity(GLM1) #1.01
check_collinearity(GLM2) #1.29
check_collinearity(GLM3) #1.00
check_collinearity(GLM4) #1.02
check_collinearity(GLM5) #1.02
check_collinearity(GLM6) #1.29
check_collinearity(GLM7) #1.00


#Determine McFadden and Nagelkerke Pseudo R2, using a hastage to report the value.
PseudoR2(GLM1, which = "Nagelkerke") #0.3909189 
PseudoR2(GLM1, which = "McFadden") #0.293163  

PseudoR2(GLM2, which = "Nagelkerke") #0.4174337  
PseudoR2(GLM2, which = "McFadden") #0.3161578  

PseudoR2(GLM3, which = "Nagelkerke") #0.3601861  
PseudoR2(GLM3, which = "McFadden") #0.267093  

PseudoR2(GLM4, which = "Nagelkerke") #0.3873221  
PseudoR2(GLM4, which = "McFadden") #0.2900801  

PseudoR2(GLM5, which = "Nagelkerke") #0.3829317  
PseudoR2(GLM5, which = "McFadden") #0.2863285  

PseudoR2(GLM6, which = "Nagelkerke") #0.4098785  
PseudoR2(GLM6, which = "McFadden") #0.3095569  

PseudoR2(GLM7, which = "Nagelkerke") #0.3522892  
PseudoR2(GLM7, which = "McFadden") #0.260492  

#Meam Nag: 0.386
#Mean McF: 0.289



#Receiver operator characteristic area under the curve
roc.GLM1 <- roc(third$Use~fitted(GLM1))
auc(roc.GLM1) #0.8467

roc.GLM2 <- roc(third$Use~fitted(GLM2))
auc(roc.GLM2) #0.8605

roc.GLM3 <- roc(third$Use~fitted(GLM3))
auc(roc.GLM3) #0.8348

roc.GLM4 <- roc(third$Use~fitted(GLM4))
auc(roc.GLM4) #0.8482

roc.GLM5 <- roc(third$Use~fitted(GLM5))
auc(roc.GLM5) #0.8451

roc.GLM6 <- roc(third$Use~fitted(GLM6))
auc(roc.GLM6) #0.8578

roc.GLM7 <- roc(third$Use~fitted(GLM7))
auc(roc.GLM7) #0.8324

#Mean:0.8465

#These include our beta coefficients that we can easily compare.
#Plot coefficients (ugly version)
plot_summs(GLM1, GLM2, GLM3, GLM4, GLM5, GLM6, GLM7)

#Improve the plot
GLMaPlotSumm <- plot_summs(GLM1, GLM2, GLM3, GLM4, GLM5, GLM6, GLM7,
                           colors = c("#009688", "#009688", "#009688", "#009688", 
                                      "#009688", "#009688", "#009688"),
                           point.size = 5,
                           point.shape = FALSE,
                           coefs = c("Dec. Dist. to Den" = "Dec_Den",
                                     "Water Cover\n(%; 50-m buffer)" = "Perc_Water_50",
                                     "Dec. Dist. to Water" = "Dec_Water",
                                     "Tree Cover Squared\n(%; 100-m buffer)" = "I(Perc_Tree_100^2)",
                                     "Tree Cover\n(%; 100-m buffer)" = "Perc_Tree_100",
                                     "Dist. to Trees (m)" = "Dist_Trees",
                                     "Dist. to Open (m)" = "Dist_Open_Plus_Shrub")) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  ggtitle("(A) Coefficient Estimates")

#Plotting our effect sizes
Pa <- plot_model(GLMavg_unscaled, type = "pred", terms = "Dec_Den[all]", show.values = TRUE, ci.lvl=0, axis.lim = c(0, 1))
Pb <- plot_model(GLMavg_unscaled, type = "pred", terms = "Perc_Water_50", show.values = TRUE, ci.lvl=0, axis.lim = c(0, 1))
Pc <- plot_model(GLMavg_unscaled, type = "pred", terms = "Dec_Water[all]", show.values = TRUE, ci.lvl=0, axis.lim = c(0, 1)) 
Pd <- plot_model(GLMavg_unscaled, type = "pred", terms = "Perc_Tree_100[all]", show.values = TRUE, ci.lvl=0, axis.lim = c(0, 1))

#Make prettier
Pb1<- Pb + theme_classic() + ggtitle("(C) Effect of Water Cover (50-m buffer) on\nProbability of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12)) +
  labs(x = "Water (%; 50-m buffer)", y = "Probability of Den")
Pb1

Pd1<- Pd + theme_classic() + ggtitle("(E) Effect of Tree Cover on\nProbability of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12)) +
  labs(x = "Tree Cover (%; 100-m buffer)", y = "Likelihood of Den")
Pd1


Pa1<- Pa + theme_classic() + ggtitle("(B) Effect of Distance to Den\n on Probability of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12)) +
  labs(x = "Distance to Den (m)", y = "Probability of Den")
Pa1


Pc1<- Pc + theme_classic() + ggtitle("(D) Effect of Distance to Water on\nProbability of Use") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12)) +
  labs(x = "Distance to Water (m)", y = "Probability of Den")
Pc1


#Fixing the decay term for decay distance to den
Pa2 <- Pa1 +
  scale_x_continuous(
    labels = function(Dec_Den) {
      rounded_dist <- -log(1 - Dec_Den) / 0.004
      round(rounded_dist)
    },
    limits = c(0, 0.95),
    breaks = scales::pretty_breaks(n = 5)
  )


#and for water
Pc2 <- Pc1 +
  scale_x_continuous(
    labels = function(Dec_Water) {
      rounded_dist <- -log(1 - Dec_Water) / 0.06
      round(rounded_dist)
    },
    limits = c(0, 0.95),
    breaks = scales::pretty_breaks(n = 5)
  )


AA <- ggarrange(Pa2, Pc2, nrow = 2)
BB <- ggarrange(Pb1, Pd1, nrow = 2)
FinalFig <- ggarrange(GLMaPlotSumm, AA, BB, ncol = 3, widths = c(5, 4, 4))
#ggsave("ThirdOrderCharts.png", FinalFig, width = 12, height = 8, dpi = 700,  bg = "white") 
