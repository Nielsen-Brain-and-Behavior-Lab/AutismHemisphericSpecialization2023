#-----------------------------------SETUP------------------------------
#Study 2 Figures
#HCPD, HBN, & UT datasets
#Written by M. Peterson under MIT License 2023
#Nielsen Brain and Behavior Lab

#load libraries
library(mosaic)
library(dplyr)
library(ggpubr)
library(broom)
library(tidyverse)
library(rstatix)
library(psych) #ICC
library(readxl) #import excel files
library(circlize) #chord diagrams
library(MatchIt) #matching subjects

#-----------------------------------ALL DEMOS------------------------------
#Load UTAH dataset
study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")

#Format wide for demos figures
study2 <- subset(study2, NewNetwork=="1")

#FIGURES
      #1. Age x Dataset Raincloud
        GroupPalettte <- c("#E69F00", "#0072B2")
        ggplot(study2, aes(x = dataset, y = Age_in_Yrs, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = NA
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="Age (Years)", x="")+
          scale_colour_manual(values=GroupPalettte, labels = c(""))+
          scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        #save the file
        ggsave(filename = paste("Study2_HCPD_UT_AgeRain_230606.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
      
      #2. Mean Framewise Displacement x Group Raincloud
        GroupPalettte <- c("#E69F00", "#0072B2")
        ggplot(study2, aes(x = dataset, y = FD_avg, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = NA
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="Mean FD (mm)", x=" ")+
          scale_colour_manual(values=GroupPalettte, labels = c(""))+
          scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        #save the file
        ggsave(filename = paste("Study2_HCPD_UT_AvgFD_230606.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
        
      #3. % Volumes Remaining x Group Raincloud
        GroupPalettte <- c("#E69F00", "#0072B2")
        ggplot(study2, aes(x = dataset, y = Percent_Vols, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = NA
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), (ylim=c(50, 100)), clip = "off")+
          labs(y="Volumes Available (%)", x=" ")+
          scale_colour_manual(values=GroupPalettte, labels = c(""))+
          scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        #save the file
        ggsave(filename = paste("Study2_HCP_UT_PercentVols_230606.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
       
        #4. Handedness (EHI)
        GroupPalettte <- c("#E69F00", "#0072B2")
        ggplot(study2, aes(x = dataset, y = Handedness, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = NA
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="Handedness (EHI)", x=" ")+
          scale_colour_manual(values=GroupPalettte, labels = c(""))+
          scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        #save the file
        ggsave(filename = paste("Study2_UT_Handedness_230802.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        

#--------------------------------FREQ. STATS----------------------------

#UT Demos:
 #Load overall file:
        #Load UTAH dataset
        study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        
        #Format wide for demos figures
        UU_data <- subset(study2, NewNetwork=="1")
        
           
        
    #Load data (IQ, ADI/ADOS, CELF) times 1-4
      #CELF <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/CELF_PreTime1-4_Clean_20190403.xlsx")
      IQ <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/IQ_allTimes_20Apr15.xlsx") #LONG
      ADI_ADOSCSS <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx") #file came directly from Molly Prigge, WIDE
        names(ADI_ADOSCSS)[1] <- "SUBJID"
        UU_data_A <- merge(ADI_ADOSCSS, UU_data, by=c("SUBJID"), all=FALSE)
        #Create ADOS var to include Time5 CSS scores when entry isn't available
        UU_data_A$ADOS_CSS_COMB <- ifelse(is.na(UU_data_A$ADOS_CSS.Entry), UU_data_A$ADOS_Total_CSS_T5, UU_data_A$ADOS_CSS.Entry)
        
      #Take mean scores across available timepoints
        #IQ
        MEAN_VIQ <-aggregate(x = IQ$VIQ,  # Specify  data column
                             by = list(IQ$LabID),              # Specify group indicator
                             FUN = mean, na.rm=TRUE)  
        names(MEAN_VIQ)[1] <- "SUBJID"
        names(MEAN_VIQ)[2] <- "MEAN_VIQ"
        
        MEAN_PIQ <-aggregate(x = IQ$PIQ,  # Specify  data column
                             by = list(IQ$LabID),              # Specify group indicator
                             FUN = mean, na.rm=TRUE)  
        names(MEAN_PIQ)[1] <- "SUBJID"
        names(MEAN_PIQ)[2] <- "MEAN_PIQ"
        
        MEAN_FIQ <-aggregate(x = as.numeric(IQ$FIQ),  # Specify  data column
                             by = list(IQ$LabID),              # Specify group indicator
                             FUN = mean, na.rm=TRUE)  
        names(MEAN_FIQ)[1] <- "SUBJID"
        names(MEAN_FIQ)[2] <- "MEAN_FIQ"
        
        #Merge with UT NSAR data
        UU_IQ <- merge(MEAN_VIQ, UU_data, by=c("SUBJID"), all=TRUE)
        UU_IQ <- merge(MEAN_PIQ, UU_IQ, by=c("SUBJID"), all=TRUE)
        UU_IQ <- merge(MEAN_FIQ, UU_IQ, by=c("SUBJID"), all=TRUE)
        
        #Filter to participants with parc data
        UU_IQ <- subset(UU_IQ, FD_avg!="NA")
        
        
        
  #In-text descriptions  
    #0. ASD/NT
      table(UU_data$dataset)
    #1. Age at Time5 scan
      favstats(data=UU_data, Age_in_Yrs~dataset)
      mean(UU_data$Age_in_Yrs)
    #2. Sex  
      table(UU_data$sex, UU_data$dataset)
    #3. Mean FD
      UU_data$FD_avg <- as.numeric(UU_data$FD_avg)
      favstats(data=UU_data, FD_avg~dataset)
    #4. % volumes remaining  
      favstats(data=UU_data, Percent_Vols~dataset)
    #5. Handedness
      favstats(data=UU_data, Handedness~dataset)
      
#UT ASD vs NT Demographics Table (Table 1)      
      #1. Age at scan
      favstats(data=UU_data, Age_in_Yrs~dataset)
        #Group comparison (t-test)
        t.test(Age_in_Yrs~dataset, data=UU_data)
      #2. Mean FD
      favstats(data=UU_data, FD_avg~dataset)
        #Group comparison (t-test)
        t.test(FD_avg~dataset, data=UU_data)
      #3. % Volumes Available
        favstats(data=UU_data, Percent_Vols~dataset)
        #Group comparison (t-test)
        t.test(Percent_Vols~dataset, data=UU_data)
      #4. Handedness
        favstats(data=UU_data, Handedness~dataset)
        #Group comparison (t-test)
        t.test(Handedness~dataset, data=UU_data)
      #5. PIQ  
        favstats(data=UU_IQ, MEAN_PIQ~dataset)
        t.test(MEAN_PIQ~dataset, data=UU_IQ)
      #6. VIQ  
        favstats(data=UU_IQ, MEAN_VIQ~dataset)
        t.test(MEAN_VIQ~dataset, data=UU_IQ) 
      #7. FIQ  
        favstats(data=UU_IQ, MEAN_FIQ~dataset)
        t.test(MEAN_FIQ~dataset, data=UU_IQ) 
      #8. ADOS CSS Entry/T5
        favstats(UU_data_A$ADOS_CSS_COMB)
      #9. ADI-R
        favstats(UU_data_A$ADI_revised)
    
        
#Low verbal/cog performance (IQ threshold FIQ <=79)  
      #Create threshold var
        UU_IQ$LVCP <- ifelse(UU_IQ$MEAN_FIQ <= 79, 1, 0)
        table(UU_IQ$LVCP, UU_IQ$dataset)
        #1=LVCP, 0=HVCP, NA=NA
        
#----------------------------------DISCOVERY DATASET-----------------------------------
#LOAD RETEST 1
        #Network surface area
        RETEST1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/network_sa/RETEST1/NETWORK_SA_UT_RETEST1_SUB_NET_LH_RH_230527.csv")
        
        #Load UT NSAR data
        RETEST1$SUBJID <- gsub("^.{0,4}", "", RETEST1$SUBJID) #remove "sub-" string
        RETEST1$Network <- gsub("^.{0,8}", "", RETEST1$NETWORK) #remove "NETWORK-" string
        RETEST1 <- subset(RETEST1, Network!=0) #drop network0
        #Switch network ordering to reflect CBIG legend ordering
        mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
        oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
        RETEST1$NewNetwork <- mapping[ match(RETEST1$Network, oldvalues) ]
        RETEST1 <- RETEST1[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
        #Create SA LAT variable
        RETEST1$SA_LAT <- (RETEST1$RH_SA - RETEST1$LH_SA) / (RETEST1$LH_SA + RETEST1$RH_SA)
        #Create % SA vars
        RETEST1$LH_SA_PERCENT <- (RETEST1$LH_SA/63103.74)*100
        RETEST1$RH_SA_PERCENT <- (RETEST1$RH_SA/63196.98)*100
        
        
        #Merge with Demos
          study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
          study2 <- subset(study2, NewNetwork==1)
          study2 <- study2[,c("SUBJID", "dataset", "Age_in_Yrs", "sex", "Handedness")]

          #subset RETEST1 to subjects that passed exclusion criteria
          UU_IDS <- study2$SUBJID
          RETEST1 <- RETEST1[RETEST1$SUBJID %in% UU_IDS, ]
          
          #merge
          RETEST1_ALL <- merge(RETEST1, study2, by=c("SUBJID"), all=FALSE)

      #Grab RETEST1-specific FD and DVARS
          FD_R1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_RETEST1/motion_metrics/FD_avg_UT_RETEST1_230802.csv")
          DVARS_R1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_RETEST1/motion_metrics/DVARS_avg_UT_RETEST1_230803.csv")
          
          #formatting
          names(FD_R1)[1] <- "SUBJID"
          FD_R1$SUBJID <- gsub("^.{0,4}", "", FD_R1$SUBJID) #remove "sub-" string
          
          names(DVARS_R1)[1] <- "SUBJID"
          DVARS_R1$SUBJID <- gsub("^.{0,4}", "", DVARS_R1$SUBJID) #remove "sub-" string
          
          #Merge in
          RETEST1_ALL <- merge(RETEST1_ALL, FD_R1, by=c("SUBJID"), all=FALSE)
          RETEST1_ALL <- merge(RETEST1_ALL, DVARS_R1, by=c("SUBJID"), all=FALSE)
          

#DEMOS
        #Wide format data
          RETEST1_WIDE <- subset(RETEST1_ALL, NewNetwork=="1")
        #ASD & NT
          table(RETEST1_WIDE$dataset)
         
        #Mean FD figure (DISC)
          GroupPalettte <- c("#E69F00", "#0072B2")
          ggplot(RETEST1_WIDE, aes(x = dataset, y = FD_avg, fill=dataset)) + 
            ggdist::stat_halfeye(
              adjust = .5, 
              width = .6, 
              .width = 0, 
              justification = -.3, 
              point_colour = "NA") + 
            geom_boxplot(
              width = .25, 
              outlier.shape = NA
            ) +
            geom_point(
              size = 1.3,
              alpha = .3,
              position = position_jitter(
                seed = 1, width = .1
              )
            ) + 
            coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
            labs(y="DISC: Mean FD (mm)", x=" ")+
            scale_colour_manual(values=GroupPalettte, labels = c(""))+
            scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
            scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
            theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
          #save the file
          ggsave(filename = paste("Study2_UT_DISC_AvgFD_230808.png"), width = 3.35, height = 3.35,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
           
#NT VALIDATION (DISC)
          #Center variables 
          RETEST1_ALL$Age_Center <- RETEST1_ALL$Age_in_Yrs - (mean(RETEST1_ALL$Age_in_Yrs))
          RETEST1_ALL$FD_Center <- RETEST1_ALL$FD_avg - (mean(RETEST1_ALL$FD_avg))
          
          #Subset to NT individuals
          RETEST1_NT <- subset(RETEST1_ALL, dataset=="UT-NT")
          
          #Hyp.: Spec networks will be the same as those reported in Aim 1       
          for (i in 1:17) {
            # Subset the data based on NewNetwork value
            subset_data <- subset(RETEST1_NT, NewNetwork == i)
            
            # Fit the linear regression model
            subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
            subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
            subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
            subset_data$Handedness <- as.numeric(subset_data$Handedness)
            
            model <- lm(SA_LAT ~ Age_Center + FD_Center + Handedness, data = subset_data)
            # Create a unique name for each model
            model_name <- paste("R1_NT_model", i, sep = "")
            
            # Assign the model to the unique name
            assign(model_name, model)
          }
          
          #Access model results through: summary(R1_NT_model1)
          
      
#GROUP DIFFERENCES (DISC)        
          #Bonferroni correction =.003
          #Center variables 
          RETEST1_ALL$Age_Center <- RETEST1_ALL$Age_in_Yrs - (mean(RETEST1_ALL$Age_in_Yrs))
          RETEST1_ALL$FD_Center <- RETEST1_ALL$FD_avg - (mean(RETEST1_ALL$FD_avg))
          
          for (i in 1:17) {
            # Subset the data based on NewNetwork value
            subset_data <- subset(RETEST1_ALL, NewNetwork == i)
            
            # Fit the linear regression model
            subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
            subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
            subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
            subset_data$dataset <- as.factor(subset_data$dataset)
            subset_data$handedness <- as.numeric(subset_data$Handedness)
            
            model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
            # Create a unique name for each model
            model_name <- paste("UT_R1_model", i, sep = "")
            
            # Assign the model to the unique name
            assign(model_name, model)
          }
          
          #Access model results through: summary(UT_R1_model1)
          
        
#TEST-RETEST FIG PANEL A (point and line)       
      #Create adjusted values
          RETEST1_ALL$SA_LAT_ADJ <- NA
          ci_df <- data.frame(dataset=factor(),
                              NewNetwork=factor(),
                              CI_MIN=integer(),
                              CI_MAX=integer(),
                              PERC97.5=integer(),
                              PERC2.5=integer(),
                              PERC50=integer(),
                              MEAN=integer())
          
          group_list <- c("UT-ASD", "UT-NT")
          for (group in group_list){
            for (i in 1:17) {
              # Subset the data based on NewNetwork value
              subset_data <- subset(RETEST1_ALL, NewNetwork == i)
              
              subset_data <- transform(subset_data,
                                       Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                                       FD_Center = FD_avg - mean(FD_avg),
                                       Group_Bin = factor(ifelse(dataset=="UT-ASD", 0,1)))
              
              # Fit the linear regression model
              model <- lm(SA_LAT ~ Group_Bin + Age_Center + FD_Center + Handedness, data = subset_data)
              
              #Grab lm coefficients
              BETA_AGE <- model[["coefficients"]][["Age_Center"]]
              BETA_FD <- model[["coefficients"]][["FD_Center"]]
              BETA_GROUP <- model[["coefficients"]][["Group_Bin1"]]
              BETA_HAND <- model[["coefficients"]][["Handedness"]]
              
              #Grab means
              MEAN_AGE <- mean(subset_data$Age_Center)
              MEAN_FD <- mean(subset_data$FD_Center)
              MEAN_GROUP <- nrow(subset_data[subset_data$dataset == "UT-ASD",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
              MEAN_HAND <- mean(subset_data$Handedness)
              
              #IDs of the subset
              subsetted_ids <- subset_data$SUBJID
              
              #Find matching rows
              matching_rows <- RETEST1_ALL$SUBJID %in% subsetted_ids &
                RETEST1_ALL$NewNetwork %in% i
              
              #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
              if (group == "UT-NT"){
                RETEST1_ALL$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              } else {
                RETEST1_ALL$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              }
              
              #Find confidence intervals for intercept based on the model
              #find mean
              MEAN <- mean(subset_data$SA_LAT_ADJ)
              n <- length(subset_data$SA_LAT_ADJ)
              std_dev <- sd(subset_data$SA_LAT_ADJ)
              std_err <- std_dev / sqrt(n)
              alpha = 0.05
              degrees_of_freedom <- n - 1
              t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
              margin_error <- t_score * std_err
              
              #lower bound
              CI_MIN <- MEAN - margin_error
              #upper bound
              CI_MAX <- MEAN + margin_error
              
              #calculate 97.5 and 2.5 percentiles
              PERC97.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.975)
              PERC2.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.025)
              PERC50 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.5)
              
              #Append CI data to dataframe
              row_df <- data.frame(group, i, CI_MIN, CI_MAX, PERC97.5, PERC2.5, PERC50, MEAN)
              names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "PERC97.5", "PERC2.5", "PERC50", "MEAN")
              ci_df <- rbind(ci_df, row_df)
              
            }
          }    
          
      # Create the point and line plot (VERTICAL, 17N)
          GroupPalette <- c("#0072B2", "#E69F00")
          #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
          dataset_order <- c("UT-NT", "UT-ASD")
          ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
          ggplot(ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
            geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .4), color="black", size=.5) +
            geom_point(position = position_dodge(width = .4), size=2, shape=21) +
            coord_cartesian(ylim= c(1.2, NA), x=c(-0.68,.72), clip = "off") +
            labs(y = "", x = "") +
            geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
            scale_y_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
              axis.text.x = element_text(colour = "black", vjust=1),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_RETEST1_GROUP_NSAR_PointLineAdjusted_230803.png"), width = 3.35, height = 6,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)

  # Create the point and line plot (VERTICAL, 3N)
          GroupPalette <- c("#0072B2", "#E69F00")
          ci_df_subset <- ci_df[ci_df$NewNetwork %in% c(5, 8, 11), ]
          network_order <- c('5','8','11')
          ci_df_subset$NewNetwork <- factor(ci_df_subset$NewNetwork, level = network_order)
          dataset_order <- c("UT-NT", "UT-ASD")
          ci_df_subset$dataset <- factor(ci_df_subset$dataset, level=dataset_order)
          ggplot(ci_df_subset, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
            geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .4), color="black", size=.5) +
            geom_point(position = position_dodge(width = .4), size=2, shape=21) +
            coord_cartesian(ylim= c(1.2, NA), x=c(-0.68,.72), clip = "off") +
            labs(y = "", x = "") +
            geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
            scale_y_discrete(labels=c('LANG','SAL-A','CTRL-B'))+
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
              axis.text.x = element_text(colour = "black", vjust=1),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_RETEST1_3N_GROUP_NSAR_PointLineAdjusted_230824.png"), width = 3.35, height = 3,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
 
#----------------------------------REPLICATION DATASET-----------------------------------
  #LOAD RETEST 2
      #Network surface area
      RETEST2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/network_sa/RETEST2/NETWORK_SA_UT_RETEST2_SUB_NET_LH_RH_230527.csv")
      
      #Load UT NSAR data
      RETEST2$SUBJID <- gsub("^.{0,4}", "", RETEST2$SUBJID) #remove "sub-" string
      RETEST2$Network <- gsub("^.{0,8}", "", RETEST2$NETWORK) #remove "NETWORK-" string
      RETEST2 <- subset(RETEST2, Network!=0) #drop network0
      #Switch network ordering to reflect CBIG legend ordering
        mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
        oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
        RETEST2$NewNetwork <- mapping[ match(RETEST2$Network, oldvalues) ]
        RETEST2 <- RETEST2[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
        #Create SA LAT variable
        RETEST2$SA_LAT <- (RETEST2$RH_SA - RETEST2$LH_SA) / (RETEST2$LH_SA + RETEST2$RH_SA)
        #Create % SA vars
        RETEST2$LH_SA_PERCENT <- (RETEST2$LH_SA/63103.74)*100
        RETEST2$RH_SA_PERCENT <- (RETEST2$RH_SA/63196.98)*100
        
        
        #Merge with Demos
        study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        study2 <- subset(study2, NewNetwork==1)
        study2 <- study2[,c("SUBJID", "dataset", "Age_in_Yrs", "sex", "Handedness")]
        
        #subset RETEST1 to subjects that passed exclusion criteria
        UU_IDS <- study2$SUBJID
        RETEST2 <- RETEST2[RETEST2$SUBJID %in% UU_IDS, ]
        
        #merge
        RETEST2_ALL <- merge(RETEST2, study2, by=c("SUBJID"), all=FALSE)
        
        #Grab RETEST2-specific FD and DVARS
        FD_R2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_RETEST2/motion_metrics/FD_avg_UT_RETEST2_230803.csv")
        DVARS_R2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_RETEST2/motion_metrics/DVARS_avg_UT_RETEST2_230803.csv")
        
        #formatting
        names(FD_R2)[1] <- "SUBJID"
        FD_R2$SUBJID <- gsub("^.{0,4}", "", FD_R2$SUBJID) #remove "sub-" string
        
        names(DVARS_R2)[1] <- "SUBJID"
        DVARS_R2$SUBJID <- gsub("^.{0,4}", "", DVARS_R2$SUBJID) #remove "sub-" string
        
        #Merge in
        RETEST2_ALL <- merge(RETEST2_ALL, FD_R2, by=c("SUBJID"), all=FALSE)
        RETEST2_ALL <- merge(RETEST2_ALL, DVARS_R2, by=c("SUBJID"), all=FALSE)
        
        
#DEMOS
        #Wide format data
        RETEST2_WIDE <- subset(RETEST2_ALL, NewNetwork=="1")
        #ASD & NT
        table(RETEST2_WIDE$dataset)
        
        #Mean FD figure (DISC)
        GroupPalettte <- c("#E69F00", "#0072B2")
        ggplot(RETEST2_WIDE, aes(x = dataset, y = FD_avg, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = NA
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="REP: Mean FD (mm)", x=" ")+
          scale_colour_manual(values=GroupPalettte, labels = c(""))+
          scale_fill_manual(values=GroupPalettte, labels = c("UT-ASD", "UT-NT"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("UT-ASD", "UT-NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        #save the file
        ggsave(filename = paste("Study2_UT_REP_AvgFD_230808.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
        
#NT VALIDATION (REP)
        #Center variables 
        RETEST2_ALL$Age_Center <- RETEST2_ALL$Age_in_Yrs - (mean(RETEST2_ALL$Age_in_Yrs))
        RETEST2_ALL$FD_Center <- RETEST2_ALL$FD_avg - (mean(RETEST2_ALL$FD_avg))
        
        #Subset to NT individuals
        RETEST2_NT <- subset(RETEST2_ALL, dataset=="UT-NT")
        
        #Hyp.: Spec networks will be the same as those reported in Aim 1       
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(RETEST2_NT, NewNetwork == i)
          
          # Fit the linear regression model
          subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
          subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
          subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
          subset_data$Handedness <- as.numeric(subset_data$Handedness)
          
          model <- lm(SA_LAT ~ Age_Center + FD_Center + Handedness, data = subset_data)
          # Create a unique name for each model
          model_name <- paste("R2_NT_model", i, sep = "")
          
          # Assign the model to the unique name
          assign(model_name, model)
        }
        
        #Access model results through: summary(R2_NT_model1)
        
        
#GROUP DIFFERENCES (REP)        
        #Bonferroni correction =.03
        #Center variables 
        RETEST2_ALL$Age_Center <- RETEST2_ALL$Age_in_Yrs - (mean(RETEST2_ALL$Age_in_Yrs))
        RETEST2_ALL$FD_Center <- RETEST2_ALL$FD_avg - (mean(RETEST2_ALL$FD_avg))
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(RETEST2_ALL, NewNetwork == i)
          
          # Fit the linear regression model
          subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
          subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
          subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
          subset_data$dataset <- as.factor(subset_data$dataset)
          subset_data$handedness <- as.numeric(subset_data$Handedness)
          
          model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
          # Create a unique name for each model
          model_name <- paste("UT_R2_model", i, sep = "")
          
          # Assign the model to the unique name
          assign(model_name, model)
        }
        
        #Access model results through: summary(UT_R2_model1)
        
        
#TEST-RETEST FIG PANEL A (point and line)       
    #Create adjusted values
        RETEST2_ALL$SA_LAT_ADJ <- NA
        ci_df <- data.frame(dataset=factor(),
                            NewNetwork=factor(),
                            CI_MIN=integer(),
                            CI_MAX=integer(),
                            PERC97.5=integer(),
                            PERC2.5=integer(),
                            PERC50=integer(),
                            MEAN=integer())
        
        group_list <- c("UT-ASD", "UT-NT")
        for (group in group_list){
          for (i in 1:17) {
            # Subset the data based on NewNetwork value
            subset_data <- subset(RETEST2_ALL, NewNetwork == i)
            
            subset_data <- transform(subset_data,
                                     Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                                     FD_Center = FD_avg - mean(FD_avg),
                                     Group_Bin = factor(ifelse(dataset=="UT-ASD", 0,1)))
            
            # Fit the linear regression model
            model <- lm(SA_LAT ~ Group_Bin + Age_Center + FD_Center + Handedness, data = subset_data)
            
            #Grab lm coefficients
            BETA_AGE <- model[["coefficients"]][["Age_Center"]]
            BETA_FD <- model[["coefficients"]][["FD_Center"]]
            BETA_GROUP <- model[["coefficients"]][["Group_Bin1"]]
            BETA_HAND <- model[["coefficients"]][["Handedness"]]
            
            #Grab means
            MEAN_AGE <- mean(subset_data$Age_Center)
            MEAN_FD <- mean(subset_data$FD_Center)
            MEAN_GROUP <- nrow(subset_data[subset_data$dataset == "UT-ASD",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
            MEAN_HAND <- mean(subset_data$Handedness)
            
            #IDs of the subset
            subsetted_ids <- subset_data$SUBJID
            
            #Find matching rows
            matching_rows <- RETEST2_ALL$SUBJID %in% subsetted_ids &
              RETEST2_ALL$NewNetwork %in% i
            
            #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
            if (group == "UT-NT"){
              RETEST2_ALL$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
            } else {
              RETEST2_ALL$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
            }
            
            #Find confidence intervals for intercept based on the model
            #find mean
            MEAN <- mean(subset_data$SA_LAT_ADJ)
            n <- length(subset_data$SA_LAT_ADJ)
            std_dev <- sd(subset_data$SA_LAT_ADJ)
            std_err <- std_dev / sqrt(n)
            alpha = 0.05
            degrees_of_freedom <- n - 1
            t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
            margin_error <- t_score * std_err
            
            #lower bound
            CI_MIN <- MEAN - margin_error
            #upper bound
            CI_MAX <- MEAN + margin_error
            
            #calculate 97.5 and 2.5 percentiles
            PERC97.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.975)
            PERC2.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.025)
            PERC50 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.5)
            
            #Append CI data to dataframe
            row_df <- data.frame(group, i, CI_MIN, CI_MAX, PERC97.5, PERC2.5, PERC50, MEAN)
            names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "PERC97.5", "PERC2.5", "PERC50", "MEAN")
            ci_df <- rbind(ci_df, row_df)
            
          }
        }    
        
  # Create the point and line plot (VERTICAL, 17N)
        GroupPalette <- c("#0072B2", "#E69F00")
        #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
        network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
        ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
        dataset_order <- c("UT-NT", "UT-ASD")
        ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
        ggplot(ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
        geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .4), color="black", size=.5) +
          geom_point(position = position_dodge(width = .4), size=2, shape=21) +
          coord_cartesian(ylim= c(1.2, NA), x=c(-0.68,.72), clip = "off") +
          labs(y = "", x = "") +
          geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
          scale_y_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
          scale_fill_manual(values = GroupPalette) + 
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none",
            axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
            axis.text.x = element_text(colour = "black", vjust=1),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          )
        ggsave(filename = paste("Study2_UT_RETEST2_GROUP_NSAR_PointLineAdjusted_230803.png"), width = 3.35, height = 6,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)

 
# Create the point and line plot (VERTICAL, 3N)
        GroupPalette <- c("#0072B2", "#E69F00")
        ci_df_subset <- ci_df[ci_df$NewNetwork %in% c(5, 8, 11), ]
        network_order <- c('5','8','11')
        ci_df_subset$NewNetwork <- factor(ci_df_subset$NewNetwork, level = network_order)
        dataset_order <- c("UT-NT", "UT-ASD")
        ci_df_subset$dataset <- factor(ci_df_subset$dataset, level=dataset_order)
        ggplot(ci_df_subset, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
          geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .4), color="black", size=.5) +
          geom_point(position = position_dodge(width = .4), size=2, shape=21) +
          coord_cartesian(ylim= c(1.2, NA), x=c(-0.68,.72), clip = "off") +
          labs(y = "", x = "") +
          geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
          scale_y_discrete(labels=c('LANG','SAL-A','CTRL-B'))+
          scale_fill_manual(values = GroupPalette) + 
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none",
            axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
            axis.text.x = element_text(colour = "black", vjust=1),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          )
        ggsave(filename = paste("Study2_UT_RETEST2_3N_GROUP_NSAR_PointLineAdjusted_230824.png"), width = 3.35, height = 3,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
               
#--------------------------------------TEST-RETEST NSAR-------------------------------------
#SETUP
        #RETEST1
        RETEST1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/network_sa/RETEST1/NETWORK_SA_UT_RETEST1_SUB_NET_LH_RH_230527.csv")
        
          #Load UT NSAR data
          RETEST1$SUBJID <- gsub("^.{0,4}", "", RETEST1$SUBJID) #remove "sub-" string
          RETEST1$Network <- gsub("^.{0,8}", "", RETEST1$NETWORK) #remove "NETWORK-" string
          RETEST1 <- subset(RETEST1, Network!=0) #drop network0
          #Switch network ordering to reflect CBIG legend ordering
          mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
          oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
          RETEST1$NewNetwork <- mapping[ match(RETEST1$Network, oldvalues) ]
          RETEST1 <- RETEST1[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
          #Create SA LAT variable
          RETEST1$SA_LAT_R1 <- (RETEST1$RH_SA - RETEST1$LH_SA) / (RETEST1$LH_SA + RETEST1$RH_SA)
          
        #RETEST2
        RETEST2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/network_sa/RETEST2/NETWORK_SA_UT_RETEST2_SUB_NET_LH_RH_230527.csv")
        
            #Load UT NSAR data
            RETEST2$SUBJID <- gsub("^.{0,4}", "", RETEST2$SUBJID) #remove "sub-" string
            RETEST2$Network <- gsub("^.{0,8}", "", RETEST2$NETWORK) #remove "NETWORK-" string
            RETEST2 <- subset(RETEST2, Network!=0) #drop network0
            #Switch network ordering to reflect CBIG legend ordering
            mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
            oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
            RETEST2$NewNetwork <- mapping[ match(RETEST2$Network, oldvalues) ]
            RETEST2 <- RETEST2[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
            #Create SA LAT variable
            RETEST2$SA_LAT_R2 <- (RETEST2$RH_SA - RETEST2$LH_SA) / (RETEST2$LH_SA + RETEST2$RH_SA)
            
        #Load demos
        study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        study2 <- subset(study2, NewNetwork==1)
        study2 <- study2[,c("SUBJID", "dataset", "Age_in_Yrs", "sex", "Handedness")]
        
        #subset RETEST1 to subjects that passed exclusion criteria
        UU_IDS <- study2$SUBJID
        RETEST1 <- RETEST1[RETEST1$SUBJID %in% UU_IDS, ]
        RETEST2 <- RETEST2[RETEST2$SUBJID %in% UU_IDS, ]
        
        #merge
        RETEST1_ALL <- merge(RETEST1, study2, by=c("SUBJID"), all=FALSE)
        RETEST <- merge(RETEST2, RETEST1_ALL, by=c("SUBJID", "NewNetwork"), all=TRUE)
        
        
#Calculate network-level ICC for NSAR        
        # Create an empty dataframe to store ICC3 values
        icc_net_df <- data.frame(NSAR_ICC = numeric(0))
        for (network in unique(RETEST$NewNetwork)) {
          # Create dataframe subset
          df_subset <- subset(RETEST, NewNetwork == network)[, c("SA_LAT_R1", "SA_LAT_R2")]
          
          # Rename dataframe
          df_name <- paste("nsar_wide_dataR.", network, sep = "")
          assign(df_name, df_subset)
          
          # Compute ICC and save ICC3 value to dataframe
          icc_result <- suppressWarnings(ICC(get(df_name)))
          icc3_value <- icc_result$results$ICC[3]
          icc_net_df <- rbind(icc_net_df, data.frame(NSAR_ICC = icc3_value))
        }
        
        #Add SUBJID variable: 
        icc_net_df$NewNetwork <- unique(RETEST$NewNetwork)
        
 

        
#NETWORK SCATTERPLOTS
        for (n in 1:17) {
          subsetted_data <- subset(RETEST, NewNetwork == n)  # Subsetting the dataset based on Network
          icc <- icc_net_df$NSAR_ICC
          icc_ordered <- icc[order(icc_net_df$NewNetwork)]
          Network_Names<- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
          
          min_value <- min(subsetted_data$SA_LAT_R1, subsetted_data$SA_LAT_R2)
          max_value <- max(subsetted_data$SA_LAT_R1, subsetted_data$SA_LAT_R2)
          
          CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
          subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
          subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
          
          filename <- paste("Study2_UT_NSAR_RETEST_Plots_Network", n, "_230810.png", sep = "")
          plot_title <- paste(Network_Names[n], " ICC: ", format(round(icc_ordered[n], 2), nsmall = 2), sep="")
          
          ggplot(subsetted_data, aes(x = SA_LAT_R2, y = SA_LAT_R1, color = NewNetwork)) +
            labs(x = "NSAR Session 2", y = "NSAR Session 1") +
            labs(fill = " ", color = " ") +
            ggtitle(plot_title)+
            geom_point(aes(fill = NewNetwork),color="black", pch = 21) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
            geom_smooth(aes(color = NewNetwork), method = "lm", size = 0.75, se = FALSE) +
            scale_y_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
            scale_x_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
            scale_color_manual(values = CBIG_Palette[n]) +
            scale_fill_manual(values = CBIG_Palette[n]) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0, size=10),
                  axis.title = element_text(colour = "black", size = 10),
                  axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=9),
                  axis.text.x = element_text(colour = "black", size=9),
                  legend.position = "none",
                  legend.title = element_text(colour = "black", size = 11),
                  legend.text = element_text(colour = "black", size = 11),
                  legend.background = element_rect(fill = "white", size = 0.5),
                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                  axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                  panel.border = element_blank(),
                  panel.background = element_blank())
          
          ggsave(filename = filename, width = 1.675, height = 1.675,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        }
        
                
               
#--------------------------------------TEST-RETEST TSNR---------------------------------
        
        
       
         
#---------------------------------------TEST-RETEST DICE-------------------------------
#SETUP: SINGLE DICE
        #Load Single Dice
        SINGLE_DICE <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/dice/RETEST/UT_RETEST_dice_singular_230810.csv")
        #Merge demos
        study2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        study2 <- subset(study2, NewNetwork==1)
        study2 <- study2[,c("SUBJID", "dataset", "Age_in_Yrs", "sex", "Handedness")]
        
        SINGLE_DICE <- merge(SINGLE_DICE, study2, by=c("SUBJID"), all=FALSE)
        
        
        #Fig. 1: Subject RAINCLOUD OF SINGLE DICE X TIME
        Palette <- c("#E69F00", "#0072B2")
        ggplot(SINGLE_DICE, aes(x = dataset, y = Dice, fill=dataset)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = 21, 
            outlier.fill = NULL
          ) +
          geom_point(
            size = 1.3,
            alpha = .3,
            position = position_jitter(
              seed = 1, width = .1
            )
          ) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="Subject Dice Coefficient", x="")+
          scale_colour_manual(values=Palette)+
          scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          scale_x_discrete(labels=c("ASD", "NT")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        ggsave(filename = paste("Study2_UT_SingleDice_230810.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures", dpi = 300)
        

        
        
#SETUP: NETWORK DICE
        
        
                
#-----------------------------------NT VALIDATION: COMPLETE DATASET-------------------------
#SETUP: 
        #Load UTAH dataset
        UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        
        #Center variables 
        UT_NSAR$Age_Center <- UT_NSAR$Age_in_Yrs - (mean(UT_NSAR$Age_in_Yrs))
        UT_NSAR$FD_Center <- UT_NSAR$FD_avg - (mean(UT_NSAR$FD_avg))
        
        #Subset to NT individuals
        UT_NSAR_NT <- subset(UT_NSAR, dataset=="UT-NT")
        
#Hyp.: Spec networks will be the same as those reported in Aim 1       
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(UT_NSAR_NT, NewNetwork == i)
          
          # Fit the linear regression model
          subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
          subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
          subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
          subset_data$Handedness <- as.numeric(subset_data$Handedness)
          
          model <- lm(SA_LAT ~ Age_Center + FD_Center + Handedness, data = subset_data)
          # Create a unique name for each model
          model_name <- paste("NT_NSAR_model", i, sep = "")
          
          # Assign the model to the unique name
          assign(model_name, model)
        }
        
        #Access model results through: summary(NT_NSAR_model1)
        
    
            
        
    #Step1: Model-adjusted values
        UT_NSAR_NT$SA_LAT_ADJ <- NA
        ci_df <- data.frame(sex=factor(),
                            NewNetwork=factor(),
                            CI_MIN=integer(),
                            CI_MAX=integer(),
                            MEAN=integer())
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(UT_NSAR_NT, NewNetwork == i)
          
          subset_data <- transform(subset_data,
                                   Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                                   FD_Center = FD_avg - mean(FD_avg))
          
          # Fit the linear regression model
          model <- lm(SA_LAT ~ Age_Center + FD_Center + Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Center"]]
          BETA_FD <- model[["coefficients"]][["FD_Center"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Center)
          MEAN_FD <- mean(subset_data$FD_Center)
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- UT_NSAR_NT$SUBJID %in% subsetted_ids &
            UT_NSAR_NT$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          UT_NSAR_NT$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
            #Find confidence intervals for intercept based on the model
            #find mean
            MEAN <- mean(subset_data$SA_LAT_ADJ)
            n <- length(subset_data$SA_LAT_ADJ)
            std_dev <- sd(subset_data$SA_LAT_ADJ)
            std_err <- std_dev / sqrt(n)
            alpha = 0.05
            degrees_of_freedom <- n - 1
            t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
            margin_error <- t_score * std_err
            
            #lower bound
            CI_MIN <- MEAN - margin_error
            #upper bound
            CI_MAX <- MEAN + margin_error
            #Append CI data to dataframe
            row_df <- data.frame(i, CI_MIN, CI_MAX, MEAN)
            names(row_df) <- c("NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
            ci_df <- rbind(ci_df, row_df) 
        }
        
        
        
        
        
        
      #Step2: Create the Boxplot
        # specify the order of the networks
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
        # use factor() to set the order of the factor levels
        UT_NSAR_NT$NewNetwork <- factor(UT_NSAR_NT$NewNetwork, level = network_order)
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        rearranged_palette <- CBIG_Palette[as.integer(network_order)]
        # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
        ggplot(UT_NSAR_NT, aes(x = SA_LAT_ADJ, y = NewNetwork, fill = NewNetwork)) + 
          geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
          coord_cartesian(xlim = c(-1,1), ylim = c(1.2, NA), clip = "off") +
          labs(y = "", x = "") +
          #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
          geom_vline(xintercept=0, linetype = "dotted", color = "black") +
          scale_y_discrete(labels=c('Dorsal Attn-A', 'Default-C', 'Language', 'Default-A', 'Control-A', 'Sal/VenAttn-B', 'Dorsal Attn-B', 'Somatomotor-B', 'Default-B', 'Limbic-A', 'Somatomotor-A', 'Visual-A', 'Sal/VenAttn-A', 'Control-C', 'Visual-B', 'Control-B', 'Limbic-B'))+
          scale_colour_manual(values = rearranged_palette) +
          scale_fill_manual(values = rearranged_palette) + 
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none",
            axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
            axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          )
        ggsave(filename = paste("Study2_UT_NT_NSAR_Boxplots_230609.png"), width = 3.5, height = 6,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
  
        
        
        #Horizontal boxplot for PPT
        # specify the order of the networks
        #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
        network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
        # use factor() to set the order of the factor levels
        UT_NSAR_NT$NewNetwork <- factor(UT_NSAR_NT$NewNetwork, level = network_order)
        CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
        rearranged_palette <- CBIG_Palette[as.integer(network_order)]
        # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
        ggplot(UT_NSAR_NT, aes(x = NewNetwork, y = SA_LAT_ADJ, fill = NewNetwork)) + 
          geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
          coord_cartesian(xlim = c(1.2,NA), ylim = c(-1, 1), clip = "off") +
          labs(y = "", x = "") +
          geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
          #geom_vline(xintercept=0, linetype = "dotted", color = "black") +
          scale_x_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
          scale_colour_manual(values = rearranged_palette) +
          scale_fill_manual(values = rearranged_palette) + 
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none",
            axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
            axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 25),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          )
        ggsave(filename = paste("Study2_UT_NT_NSAR_Boxplots_HORZ_230707.png"), width = 6, height = 2.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
        
#-----------------------------------GROUP ANALYSIS: UT HYP.2---------------------------
#SETUP
        #Load UTAH dataset
        UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
        
#Hyp2: Group differences in specialization will be localized to language network.
          #Bonferroni correction =.003
          
          #Center variables 
          UT_NSAR$Age_Center <- UT_NSAR$Age_in_Yrs - (mean(UT_NSAR$Age_in_Yrs))
          UT_NSAR$FD_Center <- UT_NSAR$FD_avg - (mean(UT_NSAR$FD_avg))

          for (i in 1:17) {
            # Subset the data based on NewNetwork value
            subset_data <- subset(UT_NSAR, NewNetwork == i)
            
            # Fit the linear regression model
            subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
            subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
            subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
            subset_data$dataset <- as.factor(subset_data$dataset)
            subset_data$handedness <- as.numeric(subset_data$Handedness)
            
            model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
            # Create a unique name for each model
            model_name <- paste("UT_NSAR_model", i, sep = "")
            
            # Assign the model to the unique name
            assign(model_name, model)
          }
          
          #Access model results through: summary(UT_NSAR_model1)


#Effect sizes for group differences          
          #Cohen's D for network 5 (LANG) on multiple regression contrasts using formula from: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1469-185X.2007.00027.x; see equation 10
          summary(UT_NSAR_model5)
          t_statistic <- -4.690 
          n1 <- 48 #participants in ASD group
          n2 <- 70 #participants in NT
          cohens_d <- t_statistic*(n1 + n2)/(sqrt(n1 * n2) * sqrt(116)) # 116 dof 
          cohens_d  
          
          #Cohen's D for network 8 (SAL-A) on multiple regression contrasts using formula from: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1469-185X.2007.00027.x
          summary(UT_NSAR_model8)
          t_statistic <- 2.896  
          n1 <- 48 #participants in ASD group
          n2 <- 70 #participants in NT
          cohens_d <- t_statistic*(n1 + n2)/(sqrt(n1 * n2) * sqrt(116)) # 116 dof 
          cohens_d  
          
          #Cohen's D for network 11 (CTRL-B) on multiple regression contrasts using formula from: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1469-185X.2007.00027.x
          summary(UT_NSAR_model11)
          t_statistic <- 2.706  
          n1 <- 48 #participants in ASD group
          n2 <- 70 #participants in NT
          cohens_d <- t_statistic*(n1 + n2)/(sqrt(n1 * n2) * sqrt(116)) # 116 dof 
          cohens_d  
          
        
#FIG.1: Adjusted values NSAR x Group 
          #Create adjusted values
          UT_NSAR$SA_LAT_ADJ <- NA
          ci_df <- data.frame(dataset=factor(),
                              NewNetwork=factor(),
                              CI_MIN=integer(),
                              CI_MAX=integer(),
                              PERC97.5=integer(),
                              PERC2.5=integer(),
                              PERC50=integer(),
                              MEAN=integer())
          
          group_list <- c("UT-ASD", "UT-NT")
          for (group in group_list){
            for (i in 1:17) {
              # Subset the data based on NewNetwork value
              subset_data <- subset(UT_NSAR, NewNetwork == i)
              
              subset_data <- transform(subset_data,
                                       Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                                       FD_Center = FD_avg - mean(FD_avg),
                                       Group_Bin = factor(ifelse(dataset=="UT-ASD", 0,1)))
              
              # Fit the linear regression model
              model <- lm(SA_LAT ~ Group_Bin + Age_Center + FD_Center + Handedness, data = subset_data)
              
              #Grab lm coefficients
              BETA_AGE <- model[["coefficients"]][["Age_Center"]]
              BETA_FD <- model[["coefficients"]][["FD_Center"]]
              BETA_GROUP <- model[["coefficients"]][["Group_Bin1"]]
              BETA_HAND <- model[["coefficients"]][["Handedness"]]
              
              #Grab means
              MEAN_AGE <- mean(subset_data$Age_Center)
              MEAN_FD <- mean(subset_data$FD_Center)
              MEAN_GROUP <- nrow(subset_data[subset_data$dataset == "UT-ASD",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
              MEAN_HAND <- mean(subset_data$Handedness)
              
              #IDs of the subset
              subsetted_ids <- subset_data$SUBJID
              
              #Find matching rows
              matching_rows <- UT_NSAR$SUBJID %in% subsetted_ids &
                UT_NSAR$NewNetwork %in% i
              
              #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
              if (group == "UT-NT"){
                UT_NSAR$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              } else {
                UT_NSAR$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              }

              #Find confidence intervals for intercept based on the model
              #find mean
              MEAN <- mean(subset_data$SA_LAT_ADJ)
              n <- length(subset_data$SA_LAT_ADJ)
              std_dev <- sd(subset_data$SA_LAT_ADJ)
              std_err <- std_dev / sqrt(n)
              alpha = 0.05
              degrees_of_freedom <- n - 1
              t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
              margin_error <- t_score * std_err
              
              #lower bound
              CI_MIN <- MEAN - margin_error
              #upper bound
              CI_MAX <- MEAN + margin_error
              
              #calculate 97.5 and 2.5 percentiles
              PERC97.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.975)
              PERC2.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.025)
              PERC50 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.5)
              
              #Append CI data to dataframe
              row_df <- data.frame(group, i, CI_MIN, CI_MAX, PERC97.5, PERC2.5, PERC50, MEAN)
              names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "PERC97.5", "PERC2.5", "PERC50", "MEAN")
              ci_df <- rbind(ci_df, row_df)
              
            }
          }    
          
          # Create the point and line plot (VERTICAL)
          GroupPalette <- c("#0072B2", "#E69F00")
          #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
          dataset_order <- c("UT-NT", "UT-ASD")
          ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
          ggplot(ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
            geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .4), color="black", size=.5) +
            geom_point(position = position_dodge(width = .4), size=2, shape=21) +
            coord_cartesian(ylim= c(1.2, NA), x=c(-0.68,.72), clip = "off") +
            labs(y = "", x = "") +
            geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
            scale_y_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
              axis.text.x = element_text(colour = "black", vjust=1),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_GROUP_NSAR_PointLineAdjusted_230606.png"), width = 3.35, height = 6,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
    
          # Create the point and line plot (HORIZONTAL)
          GroupPalette <- c("#0072B2", "#E69F00")
          #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
          dataset_order <- c("UT-NT", "UT-ASD")
          ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
          ggplot(ci_df, aes(x = NewNetwork, y = MEAN, group=interaction(dataset, NewNetwork), fill=as.factor(dataset))) +
            geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = 0,  position = position_dodge(width = .4), color="black", size=.5) +
            geom_point(position = position_dodge(width = .4), size=2, shape=21) +
            coord_cartesian(x=c(1.2, NA), clip = "off") +
            labs(y = "Adjusted NSAR Value", x = "") +
            geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
            scale_x_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
              axis.text.x = element_text(colour = "black", vjust=1, angle=25, hjust=1),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_GROUP_NSAR_PointLineAdjusted_HORZ_230707.png"), width = 6, height = 2.25,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
          
          
    #Adjust LH and RH SA for covariates: mean-centered age, mean-centered FD, group     
          #Create adjusted values
          UT_NSAR$RH_SA_ADJ <- NA
          UT_NSAR$LH_SA_ADJ <- NA
          
          # Create an empty dataframe to store the confidence interval data
          ci_df <- data.frame(matrix(ncol = 8, nrow = 0))
          names(ci_df) <- c("dataset", "NewNetwork", "HEMI", "CI_MIN", "CI_MAX", "MEAN")
          
          # Define a function to calculate confidence intervals
          calculate_ci <- function(data, column) {
            mean_val <- mean(data[[column]])
            n <- length(data[[column]])
            std_dev <- sd(data[[column]])
            std_err <- std_dev / sqrt(n)
            alpha <- 0.05
            degrees_of_freedom <- n - 1
            t_score <- qt(p = alpha/2, df = degrees_of_freedom, lower.tail = FALSE)
            margin_error <- t_score * std_err
            
            ci_min <- mean_val - margin_error
            ci_max <- mean_val + margin_error
            
            return(list(mean_val = mean_val, ci_min = ci_min, ci_max = ci_max))
          }
          
          group_list <- c("UT-ASD", "UT-NT")
          for (group in group_list){
            for (i in 1:17) {
              # Subset the data based on NewNetwork value
              subset_data <- subset(UT_NSAR, NewNetwork == i)
              
              subset_data <- transform(subset_data,
                                       Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                                       FD_Center = FD_avg - mean(FD_avg),
                                       Group_Bin = factor(ifelse(dataset=="UT-ASD", 0,1)))
              
              # Fit the linear regression model
              rh_model <- lm(RH_SA_PERCENT ~ Group_Bin + Age_Center + FD_Center + Handedness, data = subset_data)
              
              #Grab lm coefficients
              BETA_AGE <- rh_model[["coefficients"]][["Age_Center"]]
              BETA_FD <- rh_model[["coefficients"]][["FD_Center"]]
              BETA_GROUP <- rh_model[["coefficients"]][["Group_Bin1"]]
              BETA_HAND <- rh_model[["coefficients"]][["Handedness"]]
              
              #Grab means
              MEAN_AGE <- mean(subset_data$Age_Center)
              MEAN_FD <- mean(subset_data$FD_Center)
              MEAN_GROUP <- nrow(subset_data[subset_data$dataset == "UT-ASD",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
              MEAN_HAND <- mean(subset_data$Handedness)
              
              #IDs of the subset
              subsetted_ids <- subset_data$SUBJID
              
              #Find matching rows
              matching_rows <- UT_NSAR$SUBJID %in% subsetted_ids &
                UT_NSAR$NewNetwork %in% i
              
              #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
              if (group == "UT-NT"){
                UT_NSAR$RH_SA_ADJ[matching_rows] <- subset_data$RH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$RH_SA_ADJ <- subset_data$RH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              } else {
                UT_NSAR$RH_SA_ADJ[matching_rows] <- subset_data$RH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$RH_SA_ADJ <- subset_data$RH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              }
              
              
              #LH: Fit the linear regression model
              lh_model <- lm(LH_SA_PERCENT ~ Group_Bin + Age_Center + FD_Center + Handedness, data = subset_data)
              
              #Grab lm coefficients
              BETA_AGE <- lh_model[["coefficients"]][["Age_Center"]]
              BETA_FD <- lh_model[["coefficients"]][["FD_Center"]]
              BETA_GROUP <- lh_model[["coefficients"]][["Group_Bin1"]]
              BETA_HAND <- lh_model[["coefficients"]][["Handedness"]]
              
              #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
              if (group == "UT-NT"){
                UT_NSAR$LH_SA_ADJ[matching_rows] <- subset_data$LH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$LH_SA_ADJ <- subset_data$LH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              } else {
                UT_NSAR$LH_SA_ADJ[matching_rows] <- subset_data$LH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                subset_data$LH_SA_ADJ <- subset_data$LH_SA_PERCENT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_GROUP*(as.numeric(subset_data$Group_Bin) - MEAN_GROUP))+ (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              }
              
              hemi_list <- c("LH", "RH")
              for (hemi in hemi_list){
              #calculate confidence intervals for RH and LH

                  #pick var for CI
                  hemi_var <- paste0(hemi, "_SA_ADJ", sep="")
                  
                  # Calculate confidence intervals for RH_SA_ADJ
                  ci <- calculate_ci(subset_data, hemi_var)
                  
                  # Append CI data to dataframe
                  row_df <- data.frame(group, i, hemi, ci$ci_min, ci$ci_max, ci$mean_val)
                  names(row_df) <- c("dataset", "NewNetwork","HEMI", "CI_MIN", "CI_MAX", "MEAN")
                  ci_df <- rbind(ci_df, row_df)
              }
            }
          }    
          
          
      #Fig. 2A: % LH SA Boxplot in UT
          #Just include specialized networks
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          # use factor() to set the order of the factor levels
          UT_NSAR$NewNetwork <- factor(UT_NSAR$NewNetwork, level = network_order)
          GroupPalette <- c("#E69F00", "#0072B2")
          # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
          ggplot(UT_NSAR, aes(x = NewNetwork, y = LH_SA_ADJ, group=interaction(dataset, NewNetwork), fill = dataset)) + 
            geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
            coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 15), clip = "off") +
            labs(y = "Adj % LH Surface Area", x = "") +
            scale_y_continuous(expand=c(0,0))+
            scale_x_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            scale_colour_manual(values = GroupPalette) +
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
              axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_SA_LH_ADJ_Boxplots_230609.png"), width = 6.9, height = 2.35,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
          
      #Fig. 2B: % RH SA Boxplot in UT
          #Just include specialized networks
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          # use factor() to set the order of the factor levels
          UT_NSAR$NewNetwork <- factor(UT_NSAR$NewNetwork, level = network_order)
          GroupPalette <- c("#E69F00", "#0072B2")
          ggplot(UT_NSAR, aes(x = NewNetwork, y = RH_SA_ADJ, group=interaction(dataset, NewNetwork), fill = dataset)) + 
            geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
            coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 13.5), clip = "off") +
            labs(y = "Adj % RH Surface Area", x = "") +
            scale_y_continuous(expand=c(0,0))+
            scale_x_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            scale_colour_manual(values = GroupPalette) +
            scale_fill_manual(values = GroupPalette) + 
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
              axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_SA_RH_ADJ_Boxplots_230609.png"), width = 6.9, height = 2.35,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
          
      
              
      #Fig. 2C: MEAN % SA for LH and RH. With X-axis MSHBM Labels. Boxplots
          #Mean percent
          #LH_MEAN <- aggregate(LH_SA_ADJ ~ dataset + NewNetwork, data = UT_NSAR, FUN = mean)
          #RH_MEAN <- aggregate(RH_SA_ADJ ~ dataset + NewNetwork, data = UT_NSAR, FUN = mean)
          
          #names(LH_MEAN)[3] <- "MEAN_LH_PERCENT"
          #names(RH_MEAN)[3] <- "MEAN_RH_PERCENT"
          #mean_df <- merge(LH_MEAN, RH_MEAN, by=c("dataset", "NewNetwork"), all=TRUE)
          
          #MEAN_LONG <- mean_df %>%
          #  pivot_longer(cols = starts_with("MEAN_"),
          #               names_to = c("HEMI"),
          #               names_pattern = "MEAN_(.*)")
          #names(MEAN_LONG)[4] <- "MEAN_PERCENT"
          
          #std. error
          #std <- function(x) sd(x)/sqrt(length(x))
          #LH_STD <- aggregate(LH_SA_ADJ ~ dataset + NewNetwork, data = UT_NSAR, FUN = std)
          #RH_STD <- aggregate(RH_SA_ADJ ~ dataset + NewNetwork, data = UT_NSAR, FUN = std)
          
          #names(LH_STD)[3] <- "SE_LH_PERCENT"
          #names(RH_STD)[3] <- "SE_RH_PERCENT"
          #se_df <- merge(LH_STD, RH_STD, by=c("dataset", "NewNetwork"), all=TRUE)
          #SE_LONG <- se_df %>%
          #  pivot_longer(cols = starts_with("SE_"),
          #               names_to = c("HEMI"),
          #               names_pattern = "SE_(.*)")
          #names(SE_LONG)[4] <- "SE_PERCENT"
          
          #SA_df <- merge(SE_LONG, MEAN_LONG, by=c("dataset", "NewNetwork", "HEMI"), all=TRUE)
          #combine dataset and hemi into one var
          #SA_df$data_hemi = as.factor(paste0(SA_df$dataset,SA_df$HEMI))
          
          # specify the order of the networks
          #network_order <- c('1', '2', '3','4', '5', '6','7', '8','9', '10', '11', '12', '13','14', '15','16', '17')
          #SA_df <- SA_df[SA_df$NewNetwork %in% network_order, ]
          #Order networks numerically
          # use factor() to set the order of the factor levels
          #SA_df$NewNetwork <- factor(SA_df$NewNetwork, level = network_order)
          #Order groups manually
          #group_order <- c('UT-ASDLH_PERCENT', "UT-ASDRH_PERCENT", "UT-NTLH_PERCENT", "UT-NTRH_PERCENT")
          #SA_df$data_hemi <- factor(SA_df$data_hemi, levels=group_order)
          #GroupPalette <- c("#E69F00","#FFCC5B", "#0072B2", "#93D8FF")
          #ggplot(SA_df, aes(x = NewNetwork, y = MEAN_PERCENT, group=interaction(data_hemi, NewNetwork), fill = data_hemi)) +
          #  geom_bar(stat = "identity", position=position_dodge(width = .8)) +
          #  geom_errorbar(aes(ymin=MEAN_PERCENT-SE_PERCENT, ymax = MEAN_PERCENT+SE_PERCENT), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
          #  labs(x = "", y = "Adj Mean % Surface Area") +
          #  scale_colour_manual(values = GroupPalette) +
          #  scale_fill_manual(values = GroupPalette) + 
          #  scale_y_continuous(expand=c(0,0), limits=c(0,11.2))+
          #  scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")) +
            #theme_bw()
          #  theme(
          #    axis.text = element_text(size = 10),
          #    axis.title = element_text(size = 10),
          #    legend.position = "none",
          #    axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          #    axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
          #    panel.background = element_blank(),
          #    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
          #  )
          #ggsave(filename = paste("Study2_UT_LHRH_SA_ADJ_Percent_17N_Boxplots_230609.png"), width = 6.9, height = 2.35,
          #       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)

      #Fig. 2C: use CI_df
          ci_df$data_hemi = as.factor(paste0(ci_df$dataset,ci_df$HEMI))
          # specify the order of the networks
          network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
          ci_df <- ci_df[ci_df$NewNetwork %in% network_order, ]
          #Order networks numerically
          # use factor() to set the order of the factor levels
          ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
          #Order groups manually
          group_order <- c('UT-ASDLH', "UT-ASDRH", "UT-NTLH", "UT-NTRH")
          ci_df$data_hemi <- factor(ci_df$data_hemi, levels=group_order)
          GroupPalette <- c("#E69F00","#FFCC5B", "#0072B2", "#93D8FF")
          ggplot(ci_df, aes(x = NewNetwork, y = MEAN, group=interaction(data_hemi, NewNetwork), fill = data_hemi)) +
            geom_bar(stat = "identity", position=position_dodge(width = .8)) +
            geom_errorbar(aes(ymin=CI_MIN, ymax = CI_MAX), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
            labs(x = "", y = "Adj Mean % Surface Area") +
            scale_colour_manual(values = GroupPalette) +
            scale_fill_manual(values = GroupPalette) + 
            scale_y_continuous(expand=c(0,0), limits=c(0,11.2))+
            scale_x_discrete(labels=c('DAN-A', 'DEF-C', 'LANG', 'DEF-A', 'CTRL-A', 'SAL-B', 'DAN-B', 'SOM-B', 'DEF-B', 'LIM-A', 'SOM-A', 'VIS-A', 'SAL-A', 'CTRL-C', 'VIS-B', 'CTRL-B', 'LIM-B'))+
            #theme_bw()
            theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10),
              legend.position = "none",
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
              axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid")
            )
          ggsave(filename = paste("Study2_UT_LHRH_SA_ADJ_Percent_17N_Boxplots_230609.png"), width = 6.9, height = 2.35,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)

#-----------------------------------MATCHED GROUP COMPARISONS-------------------------------------
#Uses MatchIt package  
          
#1. MEAN FD
          #MATCH on MEAN FD
            #Load UT dataset
            UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
            #Center variables 
            UT_NSAR$Age_Center <- UT_NSAR$Age_in_Yrs - (mean(UT_NSAR$Age_in_Yrs))
            UT_NSAR$FD_Center <- UT_NSAR$FD_avg - (mean(UT_NSAR$FD_avg))
          
            #Binarize AutismControl
            UT_NSAR$Dx_bin <- ifelse(UT_NSAR$dataset == "UT-NT", 0, 1)
            set.seed(42) #seed is set for reproducibility
            m.out=matchit(Dx_bin~FD_avg, method="nearest", data=UT_NSAR, ratio=1)
            summary(m.out)
            #verify good match with qq-plots
            plot(m.out, type = "qq", interactive = FALSE, which.xs = c("FD_avg"))
            #extract matched dataset
            m.data1 <- match.data(m.out, drop.unmatched = TRUE)

            
        #Grab basic info about matched dataset
            #Convert matched dataset to wide format
            FDMATCH <- subset(m.data1, NewNetwork=="1")
            
            #Participants x group
            table(FDMATCH$dataset)
            
        #Run models using FD-matched participants
          for (i in 1:17) {
            # Subset the data based on NewNetwork value
            subset_data <- subset(m.data1, NewNetwork == i)
            
            # Fit the linear regression model
            subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
            subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
            subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
            subset_data$dataset <- as.factor(subset_data$dataset)
            subset_data$handedness <- as.numeric(subset_data$Handedness)
            
            model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
            # Create a unique name for each model
            model_name <- paste("UT_NSAR_FDMATCH_model", i, sep = "")
            
            # Assign the model to the unique name
            assign(model_name, model)
          }
          
          #Access model results through: summary(UT_NSAR_FDMATCH_model1)

#2. VOLUMES AVAILABLE
            #MATCH on % VOLUMES AVAILABLE
              #Load UT dataset
              UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
              #Center variables 
              UT_NSAR$Age_Center <- UT_NSAR$Age_in_Yrs - (mean(UT_NSAR$Age_in_Yrs))
              UT_NSAR$FD_Center <- UT_NSAR$FD_avg - (mean(UT_NSAR$FD_avg))
              
              #Binarize AutismControl
              UT_NSAR$Dx_bin <- ifelse(UT_NSAR$dataset == "UT-NT", 0, 1)
              set.seed(42) #seed is set for reproducibility
              m.out=matchit(Dx_bin~Percent_Vols, method="nearest", data=UT_NSAR, ratio=1)
              summary(m.out)
              #verify good match with qq-plots
              plot(m.out, type = "qq", interactive = FALSE, which.xs = c("Percent_Vols"))
              #extract matched dataset
              m.data2 <- match.data(m.out, drop.unmatched = TRUE)
              
              
            #Grab basic info about matched dataset
              #Convert matched dataset to wide format
              PVMATCH <- subset(m.data2, NewNetwork=="1")
              
              #Participants x group
              table(PVMATCH$dataset)
              
            #Run models using FD-matched participants
            for (i in 1:17) {
              # Subset the data based on NewNetwork value
              subset_data <- subset(m.data2, NewNetwork == i)
              
              # Fit the linear regression model
              subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
              subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
              subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
              subset_data$dataset <- as.factor(subset_data$dataset)
              subset_data$handedness <- as.numeric(subset_data$Handedness)
              
              model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
              # Create a unique name for each model
              model_name <- paste("UT_NSAR_PVMATCH_model", i, sep = "")
              
              # Assign the model to the unique name
              assign(model_name, model)
            }
            
            #Access model results through: summary(UT_NSAR_PVMATCH_model1)
            
 
              
#2. FIQ
              #MATCH on % VOLUMES AVAILABLE
                #Load UT dataset
                UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
                #Center variables 
                UT_NSAR$Age_Center <- UT_NSAR$Age_in_Yrs - (mean(UT_NSAR$Age_in_Yrs))
                UT_NSAR$FD_Center <- UT_NSAR$FD_avg - (mean(UT_NSAR$FD_avg))
                
                #Merge in FIQ
                IQ <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/IQ_allTimes_20Apr15.xlsx") #LONG
               
                #Take mean scores across available timepoints
                MEAN_FIQ <-aggregate(x = as.numeric(IQ$FIQ),  # Specify  data column
                                     by = list(IQ$LabID),              # Specify group indicator
                                     FUN = mean, na.rm=TRUE)  
                names(MEAN_FIQ)[1] <- "SUBJID"
                names(MEAN_FIQ)[2] <- "MEAN_FIQ"
                
                #Merge with UT NSAR data
                UU_IQ <- merge(MEAN_FIQ, UT_NSAR, by=c("SUBJID"), all=TRUE)
                
                #Filter to participants with parc data
                UU_IQ <- subset(UU_IQ, FD_avg!="NA")
                
                #Filter to participants with FIQ data
                UU_IQ <- subset(UU_IQ, MEAN_FIQ!="NA")
                
                #Binarize AutismControl
                UU_IQ$Dx_bin <- ifelse(UU_IQ$dataset == "UT-NT", 0, 1)
                set.seed(42) #seed is set for reproducibility
                m.out=matchit(Dx_bin~MEAN_FIQ, method="nearest", data=UU_IQ, ratio=1)
                summary(m.out)
                #verify good match with qq-plots
                plot(m.out, type = "qq", interactive = FALSE, which.xs = c("MEAN_FIQ"))
                #extract matched dataset
                m.data3 <- match.data(m.out, drop.unmatched = TRUE)
                
              
              #Grab basic info about matched dataset
                #Convert matched dataset to wide format
                FIQMATCH <- subset(m.data3, NewNetwork=="1")
                
                #Participants x group
                table(FIQMATCH$dataset)
                
              #Run models using FD-matched participants
              for (i in 1:17) {
                # Subset the data based on NewNetwork value
                subset_data <- subset(m.data3, NewNetwork == i)
                
                # Fit the linear regression model
                subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
                subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
                subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
                subset_data$dataset <- as.factor(subset_data$dataset)
                subset_data$handedness <- as.numeric(subset_data$Handedness)
                
                model <- lm(SA_LAT ~ dataset + Age_Center + FD_Center + Handedness, data = subset_data)
                # Create a unique name for each model
                model_name <- paste("UT_NSAR_FIQMATCH_model", i, sep = "")
                
                # Assign the model to the unique name
                assign(model_name, model)
              }
              
              #Access model results through: summary(UT_NSAR_FIQMATCH_model1)
              
                                 
#-----------------------------------GROUP ANALYSIS: UT HYP.3---------------------------
#SETUP
          #Load UTAH dataset
          UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
          
          #Load IQ (times 1-4)
          IQ <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/IQ_allTimes_20Apr15.xlsx")
  
          #Take mean scores across available timepoints
          #IQ
            MEAN_VIQ <-aggregate(x = IQ$VIQ,  # Specify  data column
                                   by = list(IQ$LabID),              # Specify group indicator
                                   FUN = mean, na.rm=TRUE)  
            names(MEAN_VIQ)[1] <- "SUBJID"
            names(MEAN_VIQ)[2] <- "MEAN_VIQ"
            
            MEAN_PIQ <-aggregate(x = IQ$PIQ,  # Specify  data column
                                 by = list(IQ$LabID),              # Specify group indicator
                                 FUN = mean, na.rm=TRUE)  
            names(MEAN_PIQ)[1] <- "SUBJID"
            names(MEAN_PIQ)[2] <- "MEAN_PIQ"
            
            MEAN_VIQ <-aggregate(x = IQ$VIQ,  # Specify  data column
                                 by = list(IQ$LabID),              # Specify group indicator
                                 FUN = mean, na.rm=TRUE)  
            names(MEAN_VIQ)[1] <- "SUBJID"
            names(MEAN_VIQ)[2] <- "MEAN_VIQ"
            
            #Merge with UT NSAR data
            UT_NSAR_IQ <- merge(MEAN_VIQ, UT_NSAR, by=c("SUBJID"), all=FALSE)
            
            #Filter to LANG network
            UT_NSAR_IQ <- subset(UT_NSAR_IQ, NewNetwork=="5")

            #How many subjects are missing IQ? 3. N=45
            table(UT_NSAR_IQ$dataset)
            
#Statistical analysis: Relationship between VIQ and LANG LAT (NSAR 5)
          #Model components
            UT_NSAR_IQ$Age_Center <- as.numeric(UT_NSAR_IQ$Age_in_Yrs - (mean(UT_NSAR_IQ$Age_in_Yrs)))
            UT_NSAR_IQ$FD_Center <- as.numeric(UT_NSAR_IQ$FD_avg - (mean(UT_NSAR_IQ$FD_avg)))
            UT_NSAR_IQ$SA_LAT <- as.numeric(UT_NSAR_IQ$SA_LAT)
            UT_NSAR_IQ$MEAN_VIQ <- as.numeric(UT_NSAR_IQ$MEAN_VIQ)    
            UT_NSAR_IQ$Handedness <- as.numeric(UT_NSAR_IQ$Handedness)
            
          #Fit model   
            model_IQ <- lm(MEAN_VIQ ~ dataset + SA_LAT + Age_Center + FD_Center + Handedness, data = UT_NSAR_IQ)
            summary(model_IQ)
            
            
          #Scatterplot using adjusted scores
            #Grab lm coefficients
            BETA_AGE <- model_IQ[["coefficients"]][["Age_Center"]]
            BETA_FD <- model_IQ[["coefficients"]][["FD_Center"]]
            BETA_GROUP <- model_IQ[["coefficients"]][["datasetUT-NT"]]
            BETA_HAND <- model_IQ[["coefficients"]][["Handedness"]]
            
            #Grab means
            MEAN_AGE <- mean(UT_NSAR_IQ$Age_Center)
            MEAN_FD <- mean(UT_NSAR_IQ$FD_Center)
            MEAN_GROUP <- nrow(UT_NSAR_IQ[UT_NSAR_IQ$dataset == "UT-ASD",])/nrow(UT_NSAR_IQ) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
            MEAN_HAND <- mean(UT_NSAR_IQ$Handedness)
            
            #Binarize group
            UT_NSAR_IQ$GROUP_BIN <- as.numeric(ifelse(UT_NSAR_IQ$dataset=="UT-ASD", 0, 1))
            
            #Apply formula from Qurechi 2014
            UT_NSAR_IQ$VIQ_ADJ <- "NA"
            UT_NSAR_IQ$VIQ_ADJ <- ifelse(UT_NSAR_IQ$dataset=="UT-ASD", (UT_NSAR_IQ$MEAN_VIQ - ( (BETA_AGE*(UT_NSAR_IQ$Age_Center - MEAN_AGE)) + (BETA_FD*(UT_NSAR_IQ$FD_Center - MEAN_FD)) + (BETA_HAND*(UT_NSAR_IQ$Handedness - MEAN_HAND)) )), UT_NSAR_IQ$VIQ_ADJ)
            UT_NSAR_IQ$VIQ_ADJ <- ifelse(UT_NSAR_IQ$dataset=="UT-NT", (UT_NSAR_IQ$MEAN_VIQ - ( (BETA_AGE*(UT_NSAR_IQ$Age_Center - MEAN_AGE)) + (BETA_GROUP*(UT_NSAR_IQ$GROUP_BIN - MEAN_GROUP)) + (BETA_FD*(UT_NSAR_IQ$FD_Center - MEAN_FD))+ (BETA_HAND*(UT_NSAR_IQ$Handedness - MEAN_HAND)) )), UT_NSAR_IQ$VIQ_ADJ )

            #Drop NAs
            UT_NSAR_IQ <- subset(UT_NSAR_IQ, VIQ_ADJ!="NaN")
            
            #As numeric adjusted values
            UT_NSAR_IQ$VIQ_ADJ <- as.numeric(UT_NSAR_IQ$VIQ_ADJ)
            
            Palette <- c("#E69F00", "#0072B2")  
            p <- ggplot(UT_NSAR_IQ, aes(x = SA_LAT, y = VIQ_ADJ, fill = dataset)) +
              labs(x = 'Language NSAR', y = "Adjusted Verbal IQ") +
              labs(fill = " ") +
              labs(color = " ") +
              scale_colour_manual(values = Palette) +
              scale_fill_manual(values = Palette, labels = c("")) +
              geom_point(aes(fill = dataset), colour = "black", pch = 21) +
              geom_smooth(method = lm, aes(color = dataset), se = TRUE) +
              scale_colour_manual(values = Palette) +
              scale_fill_manual(values = Palette, labels = c("")) +
              guides(color = guide_legend(override.aes = list(fill = NA))) +
              theme_bw() +
              theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                #plot.title = element_text(hjust = 0.5, vjust = -0.1),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_blank(),
                legend.text = element_text(colour = "black", size = 10),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank()
              )
            
            # Save the ggplot as an PNG file
            filename <- "Study2_UT_VIQ_ASD_NSAR5_Scatter_230608.png"
            ggsave(
              filename = filename,
              plot = p,
              width = 3.35,
              height = 3.35,
              path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/",
              dpi = 300)
            
#--------------------------------------GROUP ANALYSIS: UT HYP.4----------------------------------
#Question: How does specialization stratify language development, specifically language delay?            
       
#SETUP:
       UT_NSAR_LANG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_LANGDELAY_230803.csv")
       
#FREQ. STATS
        #subset to ASD
            UT_NSAR_LANG_ASD <- subset(UT_NSAR_LANG, dataset=="UT-ASD")
        #bring in IQ
            IQ <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/IQ_allTimes_20Apr15.xlsx") #LONG
            #IQ
            MEAN_VIQ <-aggregate(x = IQ$VIQ,  # Specify  data column
                                 by = list(IQ$LabID),              # Specify group indicator
                                 FUN = mean, na.rm=TRUE)  
            names(MEAN_VIQ)[1] <- "SUBJID"
            names(MEAN_VIQ)[2] <- "MEAN_VIQ"
            
            MEAN_PIQ <-aggregate(x = IQ$PIQ,  # Specify  data column
                                 by = list(IQ$LabID),              # Specify group indicator
                                 FUN = mean, na.rm=TRUE)  
            names(MEAN_PIQ)[1] <- "SUBJID"
            names(MEAN_PIQ)[2] <- "MEAN_PIQ"
            
            MEAN_FIQ <-aggregate(x = as.numeric(IQ$FIQ),  # Specify  data column
                                 by = list(IQ$LabID),              # Specify group indicator
                                 FUN = mean, na.rm=TRUE)  
            names(MEAN_FIQ)[1] <- "SUBJID"
            names(MEAN_FIQ)[2] <- "MEAN_FIQ"
            
            #Merge with UT NSAR data
            UT_NSAR_LANG_ASD <- merge(MEAN_VIQ, UT_NSAR_LANG_ASD, by=c("SUBJID"), all=TRUE)
            UT_NSAR_LANG_ASD <- merge(MEAN_PIQ, UT_NSAR_LANG_ASD, by=c("SUBJID"), all=TRUE)
            UT_NSAR_LANG_ASD <- merge(MEAN_FIQ, UT_NSAR_LANG_ASD, by=c("SUBJID"), all=TRUE)
            
            #Bring in ADOS CSS/ADI-R
            ADI_ADOSCSS <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx") #file came directly from Molly Prigge, WIDE
            names(ADI_ADOSCSS)[1] <- "SUBJID"
            #Create ADOS var to include Time5 CSS scores when entry isn't available
            ADI_ADOSCSS$ADOS_CSS_COMB <- ifelse(is.na(ADI_ADOSCSS$ADOS_CSS.Entry), ADI_ADOSCSS$ADOS_Total_CSS_T5, ADI_ADOSCSS$ADOS_CSS.Entry)
              #merge with UT NSAR data
              UT_NSAR_LANG_ASD <- merge(ADI_ADOSCSS, UT_NSAR_LANG_ASD, by=c("SUBJID"), all=TRUE)
            
            #Filter to participants with parc data
            UT_NSAR_LANG_ASD <- subset(UT_NSAR_LANG_ASD, FD_avg!="NA")
            
        #treat lang delay as a factor
            UT_NSAR_LANG_ASD$LANG_DELAY <- as.character(UT_NSAR_LANG_ASD$LANG_DELAY)
        
        #TABLE 2
            table(UT_NSAR_LANG_ASD$LANG_DELAY)
        #1. Age in years (no LD=2, LD=1)
            favstats(data=UT_NSAR_LANG_ASD, Age_in_Yrs~LANG_DELAY)
            #Group comparison (t-test)
            t.test(Age_in_Yrs~LANG_DELAY, data=UT_NSAR_LANG_ASD)    
        #2. Mean FD 
            favstats(data=UT_NSAR_LANG_ASD, FD_avg~LANG_DELAY)
            #Group comparison (t-test)
            t.test(FD_avg~LANG_DELAY, data=UT_NSAR_LANG_ASD)   
        #3. %Volumes available
            favstats(data=UT_NSAR_LANG_ASD, Percent_Vols~LANG_DELAY)
            #Group comparison (t-test)
            t.test(Percent_Vols~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
        #4. Handedness
            favstats(data=UT_NSAR_LANG_ASD, Handedness~LANG_DELAY)
            #Group comparison (t-test)
            t.test(Handedness~LANG_DELAY, data=UT_NSAR_LANG_ASD)
        #5. Mean PIQ 
            favstats(data=UT_NSAR_LANG_ASD, MEAN_PIQ~LANG_DELAY)
            #Group comparison (t-test)
            t.test(MEAN_PIQ~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
        #6. Mean VIQ 
            favstats(data=UT_NSAR_LANG_ASD, MEAN_VIQ~LANG_DELAY)
            #Group comparison (t-test)
            t.test(MEAN_VIQ~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
        #7. Mean FIQ 
            favstats(data=UT_NSAR_LANG_ASD, MEAN_FIQ~LANG_DELAY)
            #Group comparison (t-test)
            t.test(MEAN_FIQ~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
        #8. ADOS CSS
            favstats(data=UT_NSAR_LANG_ASD, ADOS_CSS_COMB~LANG_DELAY)
            #Group comparison (t-test)
            t.test(ADOS_CSS_COMB~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
        #9. ADI-R
            favstats(data=UT_NSAR_LANG_ASD, ADI_revised~LANG_DELAY)
            #Group comparison (t-test)
            t.test(ADI_revised~LANG_DELAY, data=UT_NSAR_LANG_ASD) 
            
#MODEL: 
          #Model components
            UT_NSAR_LANG$LANG_DELAY <- as.factor(UT_NSAR_LANG$LANG_DELAY)
            UT_NSAR_LANG$Age_Center <- as.numeric(UT_NSAR_LANG$Age_in_Yrs - (mean(UT_NSAR_LANG$Age_in_Yrs)))
            UT_NSAR_LANG$FD_Center <- as.numeric(UT_NSAR_LANG$FD_avg - (mean(UT_NSAR_LANG$FD_avg)))
            UT_NSAR_LANG$SA_LAT <- as.numeric(UT_NSAR_LANG$SA_LAT)
            UT_NSAR_LANG$Handedness <- as.numeric(UT_NSAR_LANG$Handedness)
            
          #Fit main model   
            model_LANG <- lm(SA_LAT~LANG_DELAY + Age_Center + FD_Center + Handedness, data=UT_NSAR_LANG)
            summary(model_LANG)
          
          #Fit model testing ASD-NoLD vs ASD-LD    (2=ASD No Delay, 1=ASD Delay, 0=TD) 
            UT_NSAR_LANG$LANG_DELAY2 <- factor(UT_NSAR_LANG$LANG_DELAY, levels=c("2", "0", "1"))
            model2_LANG <- lm(SA_LAT~LANG_DELAY2 + Age_Center + FD_Center + Handedness, data=UT_NSAR_LANG)
            summary(model2_LANG)
            
            
          #Adjusted scores
            #Create empty vars
            UT_NSAR_LANG$SA_LAT_ADJ <- NA
            ci_df <- data.frame(LANG_DELAY=factor(),
                                CI_MIN=integer(),
                                CI_MAX=integer(),
                                MEAN=integer())
            
            #Grab lm coefficients
            BETA_AGE <- model_LANG[["coefficients"]][["Age_Center"]]
            BETA_FD <- model_LANG[["coefficients"]][["FD_Center"]]
            BETA_HAND <- model_LANG[["coefficients"]][["Handedness"]]
            BETA_GROUP1 <- model_LANG[["coefficients"]][["LANG_DELAY1"]] #1=ASD delay
            BETA_GROUP2 <- model_LANG[["coefficients"]][["LANG_DELAY2"]] #2=ASD no delay
            
            #Grab means
            MEAN_AGE <- mean(UT_NSAR_LANG$Age_Center)
            MEAN_FD <- mean(UT_NSAR_LANG$FD_Center)
            MEAN_HAND <- mean(UT_NSAR_LANG$Handedness)
            MEAN_GROUP1 <- nrow(UT_NSAR_LANG[UT_NSAR_LANG$LANG_DELAY == "1",])/nrow(UT_NSAR_LANG) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category). Determine ref category with: levels(subset_data$datset)[1]
            MEAN_GROUP2 <- nrow(UT_NSAR_LANG[UT_NSAR_LANG$LANG_DELAY == "2",])/nrow(UT_NSAR_LANG)
            
            UT_NSAR_LANG$LANG_DELAY_bin <- ifelse(UT_NSAR_LANG$LANG_DELAY=="0", 0, 1) 
            
            #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
            #Run formula for Group 0 - NT
            UT_NSAR_LANG$SA_LAT_ADJ <- ifelse(UT_NSAR_LANG$LANG_DELAY=="0", (UT_NSAR_LANG$SA_LAT - ( BETA_AGE*(UT_NSAR_LANG$Age_Center - MEAN_AGE) + BETA_FD*(UT_NSAR_LANG$FD_Center - MEAN_FD) + BETA_HAND*(UT_NSAR_LANG$Handedness - MEAN_HAND) )), UT_NSAR_LANG$SA_LAT_ADJ)
            #Run formula for Group 1 - ASD Delay
            UT_NSAR_LANG$SA_LAT_ADJ <- ifelse(UT_NSAR_LANG$LANG_DELAY=="1", (UT_NSAR_LANG$SA_LAT - ( BETA_AGE*(UT_NSAR_LANG$Age_Center - MEAN_AGE) + BETA_GROUP1*(UT_NSAR_LANG$LANG_DELAY_bin - MEAN_GROUP1) + BETA_FD*(UT_NSAR_LANG$FD_Center - MEAN_FD) + BETA_HAND*(UT_NSAR_LANG$Handedness - MEAN_HAND) )), UT_NSAR_LANG$SA_LAT_ADJ)
            #Run formula for Group 2 - ASD No Delay
            UT_NSAR_LANG$SA_LAT_ADJ <- ifelse(UT_NSAR_LANG$LANG_DELAY=="2", (UT_NSAR_LANG$SA_LAT - ( BETA_AGE*(UT_NSAR_LANG$Age_Center - MEAN_AGE) + BETA_GROUP2*(UT_NSAR_LANG$LANG_DELAY_bin - MEAN_GROUP2) + BETA_FD*(UT_NSAR_LANG$FD_Center - MEAN_FD) + BETA_HAND*(UT_NSAR_LANG$Handedness - MEAN_HAND) )), UT_NSAR_LANG$SA_LAT_ADJ)
            
           
            
            #Find confidence intervals for intercept based on the model
            group_list <- c(0, 1, 2)
            for (group in group_list){
              subset_data <- subset(UT_NSAR_LANG, LANG_DELAY==group)
            #find mean
            MEAN <- mean(subset_data$SA_LAT_ADJ)
            n <- length(subset_data$SA_LAT_ADJ)
            std_dev <- sd(subset_data$SA_LAT_ADJ)
            std_err <- std_dev / sqrt(n)
            alpha = 0.05
            degrees_of_freedom <- n - 1
            #t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
            t_score=1.96
            margin_error <- t_score * std_err
            
            #lower bound
            CI_MIN <- MEAN - margin_error
            #upper bound
            CI_MAX <- MEAN + margin_error
            #Append CI data to dataframe
            row_df <- data.frame(group, CI_MIN, CI_MAX, MEAN)
            names(row_df) <- c("LANG_DELAY", "CI_MIN", "CI_MAX", "MEAN")
            ci_df <- rbind(ci_df, row_df)
            }


#Effect size
            #Cohen's D for network 5 (LANG) on multiple regression contrasts using formula from: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1469-185X.2007.00027.x
            summary(model_LANG)
            t_statistic <- 4.618
            n1 <- 29 #participants in ASD-LD group
            n2 <- 70 #participants in NT
            cohens_d <- t_statistic*(n1 + n2)/(sqrt(n1 * n2) * sqrt(94))  #94 dof
            cohens_d  
            
            #Cohen's D for group difference between ASD No LD and NT
            summary(model_LANG)
            t_statistic <- 2.439   
            n1 <- 16 #participants in ASD-No LD group
            n2 <- 70 #participants in NT
            cohens_d <- t_statistic*(n1 + n2)/(sqrt(n1 * n2) * sqrt(81)) #81 dof
            cohens_d
                        
#Adjusted plot: raincloud 
            #drop NA values
            UT_NSAR_LANG <- subset(UT_NSAR_LANG, LANG_DELAY!="NA")
            GroupPalettte <- c("#0072B2", "#D55E00", "#E69F00")
            ggplot(UT_NSAR_LANG, aes(x = LANG_DELAY, y = SA_LAT_ADJ, fill=LANG_DELAY)) + 
              ggdist::stat_halfeye(
                adjust = .5, 
                width = .6, 
                .width = 0, 
                justification = -.3, 
                point_colour = "NA") + 
              geom_boxplot(
                width = .25, 
                outlier.shape = NA
              ) +
              geom_point(
                size = 1.3,
                alpha = .3,
                position = position_jitter(
                  seed = 1, width = .1
                )
              ) + 
              coord_cartesian((xlim = c(1.2, NA)), (ylim=c(-0.64, .64)), clip = "off")+
              labs(y="Adjusted Language NSAR", x="")+
              scale_colour_manual(values=GroupPalettte, labels = c(""))+
              scale_fill_manual(values=GroupPalettte, labels = c("NT", "ASD-LD", "ASD-No LD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
              scale_x_discrete(labels=c("NT", "ASD-LD", "ASD-No LD")) +
              theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
              theme(panel.background = element_blank())+
              theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
            #save the file
            ggsave(filename = paste("Study2_UT_LDAdj_Rain_230609.png"), width = 3.35, height = 3.35,
                   path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
            
            
            
            
            #Just show the MEAN and SEM
            std <- function(x) sd(x)/sqrt(length(x))
            SEM <- aggregate(SA_LAT_ADJ ~ LANG_DELAY + NewNetwork, data = UT_NSAR_LANG, FUN = std)
            MEAN <- aggregate(SA_LAT_ADJ ~ LANG_DELAY + NewNetwork, data = UT_NSAR_LANG, FUN = mean)
            names(SEM)[3] <- "SEM"
            names(MEAN)[3] <- "MEAN"

            mean_sem <- merge(SEM, MEAN, by=c("NewNetwork", "LANG_DELAY"), all=TRUE)
            
            mean_sem$LANG_DELAY <- as.factor(mean_sem$LANG_DELAY)
            GroupPalettte <- c("#0072B2", "#D55E00", "#E69F00")
            ggplot(mean_sem, aes(x = LANG_DELAY, y = MEAN, fill=LANG_DELAY)) + 
              geom_errorbar(aes(ymin=(MEAN-SEM), ymax = (MEAN+SEM)), width = 0, color="black", size=.5) +
              geom_point(size = 3, fill=GroupPalettte, shape=21)+ 
              #coord_cartesian((xlim = c(1.2, NA)), (ylim=c(-0.3, 0)), clip = "off")+
              labs(y="Adjusted Language NSAR", x="")+
              scale_colour_manual(values=GroupPalettte, labels = c(""))+
              scale_fill_manual(values=GroupPalettte, labels = c("NT", "ASD-LD", "ASD-No LD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
              scale_x_discrete(labels=c("NT", "ASD-LD", "ASD-No LD")) +
              theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
              theme(panel.background = element_blank())+
              theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
            #save the file
            ggsave(filename = paste("Study2_UT_LDAdj_MEAN_230624.png"), width = 3.35, height = 3.35,
                   path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
            
            
#----------------------------------------------GROUP ANALYSIS: UT HYP. 5--------------------------
#Question: What is the relationship between ASD symptom severity and language specialization?
            #Load UTAH dataset
            UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
            
            #Subset to LANG NSAR
            UT_NSAR <- subset(UT_NSAR, NewNetwork=="5")
            
            #ADOS (selected because more participants have ADOS than ADI)
            ADI_ADOSCSS <- read_excel("C:/Users/maddy/Box/Autism_CSF/data/ADOSADI.xlsx") #file came directly from Molly Prigge, WIDE
            names(ADI_ADOSCSS)[1] <- "SUBJID"
            #Create ADOS var to include Time5 CSS scores when entry isn't available
            ADI_ADOSCSS$ADOS_CSS_COMB <- ifelse(is.na(ADI_ADOSCSS$ADOS_CSS.Entry), ADI_ADOSCSS$ADOS_Total_CSS_T5, ADI_ADOSCSS$ADOS_CSS.Entry)
            
            #merge with NSAR
            UT_NSAR_LANG <- merge(UT_NSAR, ADI_ADOSCSS, by=c("SUBJID"), all=TRUE)
            
            #Drop subjects missing NSAR
            UT_NSAR_LANG <- subset(UT_NSAR_LANG, SA_LAT!="NA")
            
            #Drop subjects missing ADOS_CSS_COMB
            UT_NSAR_LANG <- subset(UT_NSAR_LANG, ADOS_CSS_COMB!="NA")
            
            
#Create model
            #Model components
            UT_NSAR_LANG$ADOS_CSS_COMB <- as.numeric(UT_NSAR_LANG$ADOS_CSS_COMB)
            UT_NSAR_LANG$Age_Center <- as.numeric(UT_NSAR_LANG$Age_in_Yrs - (mean(UT_NSAR_LANG$Age_in_Yrs)))
            UT_NSAR_LANG$FD_Center <- as.numeric(UT_NSAR_LANG$FD_avg - (mean(UT_NSAR_LANG$FD_avg)))
            UT_NSAR_LANG$SA_LAT <- as.numeric(UT_NSAR_LANG$SA_LAT)
            UT_NSAR_LANG$Handedness <- as.numeric(UT_NSAR_LANG$Handedness)
            
            #Fit model   
            model_ADOS <- lm(ADOS_CSS_COMB~SA_LAT + Age_Center + FD_Center + Handedness, data=UT_NSAR_LANG)
            summary(model_ADOS)
            
            
            
#Model-adjusted NSAR
            #Adjusted scores
            #Create empty vars
            UT_NSAR_LANG$ADOS_ADJ <- NA
            ci_df <- data.frame(CI_MIN=integer(),
                                CI_MAX=integer(),
                                MEAN=integer())
            
            #Grab lm coefficients
            BETA_AGE <- model_ADOS[["coefficients"]][["Age_Center"]]
            BETA_FD <- model_ADOS[["coefficients"]][["FD_Center"]]
            BETA_HAND <- model_ADOS[["coefficients"]][["Handedness"]]
            
            #Grab means
            MEAN_AGE <- mean(UT_NSAR_LANG$Age_Center)
            MEAN_FD <- mean(UT_NSAR_LANG$FD_Center)
            MEAN_HAND <- mean(UT_NSAR_LANG$Handedness)
           
            #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
            UT_NSAR_LANG$ADOS_ADJ <- UT_NSAR_LANG$ADOS_CSS_COMB - ( BETA_AGE*(UT_NSAR_LANG$Age_Center - MEAN_AGE) + BETA_FD*(UT_NSAR_LANG$FD_Center - MEAN_FD) + BETA_HAND*(UT_NSAR_LANG$Handedness - MEAN_HAND))

            #Find confidence intervals for intercept based on the model
              #find mean
              MEAN <- mean(UT_NSAR_LANG$ADOS_ADJ)
              n <- length(UT_NSAR_LANG$ADOS_ADJ)
              std_dev <- sd(UT_NSAR_LANG$ADOS_ADJ)
              std_err <- std_dev / sqrt(n)
              alpha = 0.05
              degrees_of_freedom <- n - 1
              #t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
              t_score=1.96
              margin_error <- t_score * std_err
              
              #lower bound
              CI_MIN <- MEAN - margin_error
              #upper bound
              CI_MAX <- MEAN + margin_error
              #Append CI data to dataframe
              row_df <- data.frame(CI_MIN, CI_MAX, MEAN)
              names(row_df) <- c("CI_MIN", "CI_MAX", "MEAN")
              ci_df <- rbind(ci_df, row_df)
            
            
#Figure: Scatterplot. NSAR x ADOSCSS            
              Palette <- c("#E69F00", "#0072B2")  
              p <- ggplot(UT_NSAR_LANG, aes(x = SA_LAT, y = ADOS_ADJ, fill=dataset)) +
                labs(x = 'Language NSAR', y = "Adjusted ADOS CSS") +
                labs(fill = " ") +
                labs(color = " ") +
                scale_colour_manual(values = Palette) +
                scale_fill_manual(values = Palette, labels = c("")) +
                geom_point(aes(fill = dataset), colour = "black",fill="#E69F00", pch = 21) +
                geom_smooth(method = lm, aes(color = dataset), se = TRUE) +
                scale_colour_manual(values = Palette) +
                scale_fill_manual(values = Palette, labels = c("")) +
                guides(color = guide_legend(override.aes = list(fill = NA))) +
                theme_bw() +
                theme(
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  #plot.title = element_text(hjust = 0.5, vjust = -0.1),
                  axis.title = element_text(colour = "black", size = 12),
                  axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                  axis.text.x = element_text(colour = "black"),
                  legend.position = "none",
                  legend.title = element_blank(),
                  legend.text = element_text(colour = "black", size = 10),
                  legend.background = element_rect(fill = "white", size = 0.5),
                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                  axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                  panel.border = element_blank(),
                  panel.background = element_blank()
                )
              
              # Save the ggplot as an PNG file
              filename <- "Study2_UT_ADOS_ASD_NSAR5_Scatter_230623.png"
              ggsave(
                filename = filename,
                plot = p,
                width = 3.35,
                height = 3.35,
                path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/",
                dpi = 300)

#------------------------------------SETUP AND SAVE OUT UTAH DATASET---------------------

#Load the UT dataset
    #Load data descriptor (demos)
    UU_data <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/Utah_dataset/Participants/UofU_Master_Demographics_220524.xlsx")
    UU_data <- UU_data[,c("SUBJID", "AutismControl", "Group", "sex", "ScanDateT5", "AgeYrsT5", "TestingT5", "Notes_T5")]
              
    #Handedness scores (EHI)
      HAND <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/Handedness_Times1 and 3_clean_13June2014.xlsx")
      names(HAND)[1] <- "SUBJID"
      MEAN_HAND <-aggregate(x = as.numeric(HAND$HandIndexByTrial),  # Specify  data column
                            by = list(HAND$SUBJID),              # Specify group indicator
                            FUN = mean, na.rm=TRUE)  
      names(MEAN_HAND)[1] <- "SUBJID"
      names(MEAN_HAND)[2] <- "Handedness"
             
    #Additional handedness data from MBDP (EHI)
      HAND2 <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/Utah_dataset/Participants/AdditionalHandedness_230630_MBDP.xlsx")
      names(HAND2)[1] <- "SUBJID"
      names(HAND2)[2] <- "Handedness"
      
    #Merge both handedness datasets
    HAND_ALL <- merge(MEAN_HAND, HAND2, by=c("SUBJID", "Handedness"), all=TRUE)
    
    
    #Load subjects with completed preproc and parc
    PARC <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_ALL/subjids/subjids.csv")
        
    #Load avg FD
    avg_FD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_UT_ALL_230224.csv")
    names(avg_FD)[1] <- "SUBJID"
    avg_FD$SUBJID <- gsub("^.{0,4}", "", avg_FD$SUBJID) #remove "sub-" string
    avg_FD$FD_avg <- as.numeric(avg_FD$FD_avg)
              
    #Load avg DVARS
    avg_DVARS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_UT_ALL_230224.csv")
    names(avg_DVARS)[1] <- "SUBJID"
    avg_DVARS$SUBJID <- gsub("^.{0,4}", "", avg_DVARS$SUBJID) #remove "sub-" string
    avg_DVARS$DVARS_avg <- as.numeric(avg_DVARS$DVARS_avg)
              
    #Load volumes remaining
    VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_Utah_230520.csv")
    names(VOLS)[1] <- "SUBJID"
    VOLS$SUBJID <- gsub("^.{0,4}", "", VOLS$SUBJID) #remove "sub-" string
    VOLS$Sum_Volumes <- as.numeric(VOLS$Sum_Volumes)
    VOLS$Percent_Vols <- VOLS$Sum_Volumes / (586*2) #Number of volumes after skip4 = 586 x2 runs
    VOLS$Percent_Vols <- VOLS$Percent_Vols*100
            
    #Merge datasets
    UU_data <- merge(UU_data, PARC, by =c("SUBJID"), all=TRUE)
    UU_data <- merge(UU_data, avg_FD, by =c("SUBJID"), all=TRUE)
    UU_data <- merge(UU_data, avg_DVARS, by =c("SUBJID"), all=TRUE)
    UU_data <- merge(UU_data, VOLS, by = c("SUBJID"), all=TRUE)
    UU_data <- merge(UU_data, HAND_ALL, by = c("SUBJID"), all=TRUE)
              
    #Exclusion and filtering
        #1. Filter to Time5 only
        UU_data <- subset(UU_data, AgeYrsT5!="NA")
              
        #2. Filter to subjects with parcellation data (FD < .2, DVARS < 50)
        UU_data <- subset(UU_data, PARC!="NA")
              
        #3. Drop duplicates if any
        UU_data$duplicate <- !duplicated(UU_data$SUBJID)
        UU_data <- subset(UU_data, duplicate==TRUE)
              
        #4. Exclude participants with FD > .2
        UU_data <- subset(UU_data, FD_avg < 0.2)
              
        #5. Exclude participants with DVARS > 50
        UU_data <- subset(UU_data, DVARS_avg < 50)
              
        #6. Exclude age outlier
        UU_data <- subset(UU_data, AgeYrsT5 < 50)
              
        #7. Threshold to less than 50% volumes
        UU_data <- subset(UU_data, Percent_Vols>50)
              
        #8. Exclude females
        UU_data <- subset(UU_data, sex!=2)
        
        #9. Exclude participants with MRI incidental findings (ex: head injury, MS, doesn't qualify as control)
        #IDs: 59504, 57797, 70784,64004,67677, 1898003, 90503, 95576
        excluded_subjects <- c("59504", "57797", "70784","64004","67677", "1898003", "90503", "95576")
        UU_data <- UU_data %>%
          filter(!SUBJID %in% excluded_subjects)
        
        #10. Exclude participants missing handedness data
        UU_data <- subset(UU_data, Handedness!="NA")
        UU_data <- subset(UU_data, Handedness!="NaN")
        
      #Dataset marker and format variables
      UU_data$dataset <- ifelse(UU_data$AutismControl == "autism", "UT-ASD", "UT-NT")
      UU_data$Age_in_Yrs <- UU_data$AgeYrsT5
      UU_data <- UU_data[,c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness")]
              
      #Load UT NSAR data
      NET_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study2_Dissertation/Utah_analysis/network_sa/NETWORK_SA_SUB_NET_LH_RH_230214.csv")
      NET_SA$SUBJID <- gsub("^.{0,4}", "", NET_SA$SUBJID) #remove "sub-" string
      NET_SA$Network <- gsub("^.{0,8}", "", NET_SA$NETWORK) #remove "NETWORK-" string
      NET_SA <- subset(NET_SA, Network!=0) #drop network0
      #Switch network ordering to reflect CBIG legend ordering
      mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
      oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
      NET_SA$NewNetwork <- mapping[ match(NET_SA$Network, oldvalues) ]
      NET_SA <- NET_SA[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
      #Create SA LAT variable
      NET_SA$SA_LAT <- (NET_SA$RH_SA - NET_SA$LH_SA) / (NET_SA$LH_SA + NET_SA$RH_SA)
      #Create % SA vars
      NET_SA$LH_SA_PERCENT <- (NET_SA$LH_SA/63103.74)*100
      NET_SA$RH_SA_PERCENT <- (NET_SA$RH_SA/63196.98)*100
      #Merge with Demos
      UT_NSAR <- merge(NET_SA, UU_data, by=c("SUBJID"), all=FALSE)
      
      
#For replication and transparency purposes, write out entire datset used for stats
write.csv(UT_NSAR,"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv",row.names=FALSE, quote=FALSE)
      
              



#LANG DELAY
    #Load UTAH dataset
    UT_NSAR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_entirety_230802.csv")
    
    #Subset to LANG NSAR
    UT_NSAR <- subset(UT_NSAR, NewNetwork=="5")
    
    #Load ADI (Time1 only)
    ADI1990 <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/ADI_Time 1_cleaned_no formulas_16July2013.xlsx", sheet = "1990")
    ADI1999 <- read_excel("C:/Users/maddy/Box/Autism_Longitudinal_Neuroimaging/Jared_BYU/UU_Lainhart_Data_June_2015_Times1to3/ADI_Time 1_cleaned_no formulas_16July2013.xlsx", sheet = "1999")
    ADIOther <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/Utah_dataset/Participants/ADI_AdditionalSubjects_MBDP_230630.xlsx")
    
    #ADI1990: Create LANG_DELAY variable: Q12AgeFirstSingleWords and Q13AgeFirstPhrases
    #Originally coded in months
    #Other codes: 993=had some words, then lost and not yet regained. 994=milestone not reached. 996=N/K but apparently normal. 997=N/K but apparently delayed. 999=N/K or not asked
    #Threshold: onset of first words later than 24 months and/or having onset of first phrases later than 33 months. 
    ADI1990$FirstWordDelay <- ifelse(ADI1990$Q12AgeFirstSingleWords <= "24" | ADI1990$Q12AgeFirstSingleWords == "996", 0, 1) #1=Delay, 0=No delay
    ADI1990$FirstPhraseDelay <- ifelse(ADI1990$Q13AgeFirstPhrases <="33" | ADI1990$Q13AgeFirstPhrases =="996", 0, 1) #1=Delay, 0=No delay
    ADI1990$LANG_DELAY <- ifelse(ADI1990$FirstWordDelay =="1" | ADI1990$FirstPhraseDelay =="1", 1, 2) #1=Delay, 2=No delay
    
    
    #ADI1999: Create LANG_DELAY variable: Q9AgeFirstSingleWords and Q10AgeFirstPhrases
    #Originally coded in months
    #Other codes: 993=had some words, then lost and not yet regained. 994=milestone not reached. 996=N/K but apparently normal. 997=N/K but apparently delayed. 999=N/K or not asked
    #Threshold: onset of first words later than 24 months and/or having onset of first phrases later than 33 months. 
    ADI1999$FirstWordDelay <- ifelse(ADI1999$Q9AgeFirstSingleWords <= "24" | ADI1999$Q9AgeFirstSingleWords == "996", 0, 1) #1=Delay, 0=No delay
    ADI1999$FirstPhraseDelay <- ifelse(ADI1999$Q10AgeFirstPhrases <="33" | ADI1999$Q10AgeFirstPhrases =="996", 0, 1) #1=Delay, 0=No delay
    ADI1999$LANG_DELAY <- ifelse(ADI1999$FirstWordDelay =="1" | ADI1999$FirstPhraseDelay =="1", 1, 2) #1=Delay, 2=No delay
    
    #ADIOther: Additional data provided by MBDP on 230630 via email
    #Originally coded in months
    #Other codes: 993=had some words, then lost and not yet regained. 994=milestone not reached. 996=N/K but apparently normal. 997=N/K but apparently delayed. 999=N/K or not asked
    #Threshold: onset of first words later than 24 months and/or having onset of first phrases later than 33 months. 
    ADIOther$FirstWordDelay <- ifelse(ADIOther$AgeFirstSingleWords <= "24" | ADIOther$AgeFirstSingleWords == "996", 0, 1) #1=Delay, 0=No delay
    ADIOther$FirstPhraseDelay <- ifelse(ADIOther$AgeFirstPhrases <="33" | ADIOther$AgeFirstPhrases =="996", 0, 1) #1=Delay, 0=No delay
    ADIOther$LANG_DELAY <- ifelse(ADIOther$FirstWordDelay =="1" | ADIOther$FirstPhraseDelay =="1", 1, 2) #1=Delay, 2=No delay
    
    
    #Merge with SA_LAT    
    #Combine ADI data
    names(ADI1999)[1] <- "SUBJID"
    ADI1999 <- ADI1999[,c("SUBJID", "FirstWordDelay", "FirstPhraseDelay", "LANG_DELAY")]
    names(ADI1990)[1] <- "SUBJID"
    ADI1990 <- ADI1990[,c("SUBJID", "FirstWordDelay", "FirstPhraseDelay", "LANG_DELAY")]
    names(ADIOther)[1] <- "SUBJID"
    ADIOther <- ADIOther[,c("SUBJID", "FirstWordDelay", "FirstPhraseDelay", "LANG_DELAY")]
    
    ADI_COMB <- merge(ADI1990, ADI1999, by=c("SUBJID", "FirstWordDelay", "FirstPhraseDelay", "LANG_DELAY"), all=TRUE)
    ADI_COMB <- merge(ADI_COMB, ADIOther, by=c("SUBJID", "FirstWordDelay", "FirstPhraseDelay", "LANG_DELAY"), all=TRUE)
    #drop NAs
    ADI_COMB <- subset(ADI_COMB, LANG_DELAY!="NA")
    #merge with NSAR
    UT_NSAR_LANG <- merge(ADI_COMB, UT_NSAR, by=c("SUBJID"), all=TRUE)
    
    #Drop subjects that don't meet exclusion criteria
    UU_IDS <- UT_NSAR$SUBJID
    UT_NSAR_LANG <- UT_NSAR_LANG[UT_NSAR_LANG$SUBJID %in% UU_IDS, ]
    
    #Drop duplicates if any
    UT_NSAR_LANG$duplicate <- !duplicated(UT_NSAR_LANG$SUBJID)
    UT_NSAR_LANG <- subset(UT_NSAR_LANG, duplicate==TRUE)
    
    #Find ASD subjects missing ADI LANG_DELAY
    UT_NSAR_LANG$missing_LD_flag <- ifelse(UT_NSAR_LANG$dataset == "UT-ASD" & UT_NSAR_LANG$LANG_DELAY == "NA", 1, 0)
    # Create a vector of SUBJID values where dataset=="UT-ASD" and LANG_DELAY=="NA"
    subjids <- UT_NSAR_LANG$SUBJID[is.na(UT_NSAR_LANG$missing_LD_flag)]
    print(subjids)
    
    #Create add TD dimension to lang delay (0)
    UT_NSAR_LANG$LANG_DELAY <- ifelse(UT_NSAR_LANG$dataset=="UT-NT", 0, UT_NSAR_LANG$LANG_DELAY)
    #LANG_DELAY coded: 2=ASD No Delay, 1=ASD Delay, 0=TD
    table(UT_NSAR_LANG$LANG_DELAY)
    
    #For replication and transparency purposes, write out entire datset used for stats
    write.csv(UT_NSAR_LANG,"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/csv_files/study2_UU_NSAR_LANGDELAY_230803.csv",row.names=FALSE, quote=FALSE)
