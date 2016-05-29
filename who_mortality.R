## Run log
mylog = file("log_whoMort.txt", open = "wt")
sink(mylog)
Sys.time()

## DESCRIPTION
## R code for exploratory analysis of global causes of mortality.
## starting data is grouped by cause, sex, and age.
## original data obtained from http://www.who.int/healthinfo/global_burden_disease/estimates/en/index2.html
## some manual manipulation in excel prior to R


## Load libraries ####

  require(dplyr)
  require(ggplot2)
  require(tidyr)
  require(gridExtra)
  require(cowplot)



## Load data

  morta = read.table("who_mortality_2015.txt", header = T, sep = "\t")
  head(morta)

    
  
## Divide Disability Adjuseted Life Years by a million for each sex  ####

  morta$milmale = morta$male / 1e+06
  morta$milfemale = morta$female / 1e+06
  
  
  
### Format data to plot all mortality causes by age and sex. ####

  b = morta %>%
    select (myid, milmale, milfemale, years, age) %>%
    gather (key = sex, value = daly, -c(myid, years, age))

    head(b)  
  
  

  
## Make new table of selected phenotypes only.####

  c = morta %>%
    select (id, myid, milmale, milfemale, years, age) %>%
    gather (key = sex, value = milcounts, -c(id, myid, years, age)) %>%
    filter (myid == "C-Car" |
              myid == "P-Uni" |
              myid == "C-Isc" |
              myid == "C-Str" |
              myid == "D-Iod" |
              myid == "I-Lep" | myid == "J-Dro" |
              myid == "J-Roa" | myid == "M-Ost" | myid == "M-Rhe" |
              myid == "O-Liv" | myid == "O-Sto")
  

  
    
## Assign factor levels in order so that sex x effect can be visualised. ####

  c$id <- factor(c$id, 
                 levels = c( 
                   "Leprosy", "Liver_cancer", "Road_injury",  "Stroke",
                   "carditis", "Stomach_cancer", "Drowning", "Ischaemic_heart_disease",
                   "Iodine_deficiency", "Rheumatoid_arthritis", "Osteoarthritis", "Unipolar_depressive_disorders"))
    head(c)


## Order the age group levels by actual age order. ####

  morta$age = factor(morta$age, levels = c("0-27 days", "1-59 months",
                                           "5-14 years", "15-29 years",
                                           "30-49 years", "50-59 years",
                                           "60-69 years", "70+ years"))
    
  

## Assign human readable labels to plot facets. ####
## Factor assignment stuff tedious.
  
  mylabs = as_labeller(c (`carditis` = "Carditis",
                          `Drowning` = "Drowning",
                          `Iodine_deficiency` = "Iodine deficiency",
                          `Ischaemic_heart_disease` = "Ischaemic heart dis.",
                          `Leprosy` = "Leprosy",
                          `Liver_cancer` = "Liver cancer",
                          `Osteoarthritis` = "Osteoarthritis",
                          `Road_injury` = "Road injury",
                          `Stomach_cancer` = "Stomach cancer",
                          `Stroke` = "Stroke",
                          `Unipolar_depressive_disorders` = "Unipolar deperession",
                          `Rheumatoid_arthritis` = "Rheumatoid arthritis"))
  
  
  
## Make a decent ggplot theme for plots. ####

  theme_f1 <- function() {
    theme_bw(base_size = 8) +
      theme(
        plot.margin = unit(c(.25, .5, .25, .25), "cm"),
        strip.background = element_rect(fill = "white", colour="white"),
        axis.line.y = element_line(size = .3, colour = "black"),
        axis.line.x = element_line(size = .3, colour = "black")
      ) 
  }
  
  attr(theme_f1(), "complete")
  

  
## same theme but with larger base size for figure 1. ####

  theme_f2 <- function() {
    theme_bw(base_size = 10) +
      theme(
        plot.margin = unit(c(.25, .5, .25, .25), "cm"),
        strip.background = element_rect(fill = "white", colour="white"),
        axis.line.y = element_line(size = .3, colour = "black"),
        axis.line.x = element_line(size = .3, colour = "black")
      ) 
  }
  
  attr(theme_f1(), "complete")
  
  
  
  
## Plot all DALY by age, group by sex. ####

  all.pheno.time = 
    ggplot (b, aes(years, daly, group = sex, fill = sex)) +
      geom_area(alpha = .5, position = "identity") +
      facet_wrap("myid", scales="free_y") + 
        theme_f1() +
        labs(title = "Guide: B=Birth, C=Cardio, D=Digestive, G=Genetic, I=Infection, J=inJury, M=Metabolic,
             N=Neurological, O=Oncology, P=Psychiatric, S=Sensory, U=Uro-genital, Z=other", size = 6) +
        scale_y_continuous(name="Disasbility-adjusted life-years (millions)", labels = NULL) +
        scale_x_continuous(name = "Age group (years)") +
          theme(legend.position="none",
                panel.grid.major.x = element_line(size = 0),
                title = element_text(size = 5))
  

    
### Plot, for selected phenotypes, age x sex x DALY ###

  sel.pheno.plot =  
    ggplot (c, aes(years, milcounts, group = sex, linetype = sex, colour = sex)) +
      geom_line(size = 4, alpha = .3, linetype = 1) + 
      geom_line(size = .5, alpha = .8, colour = "black") + 
        facet_wrap("id", scales="free_y", labeller = mylabs) + 
        scale_colour_discrete(name = "Legend", labels=c("Female", "Male")) +
        scale_linetype_discrete(name = "Legend", labels = c("Female", "Male")) +
        theme_f2() +
        scale_y_continuous(name = "Disability-adjusted life years (millions)") +
        scale_x_continuous(name = "Age group (years)") +
        guides (colour = guide_legend(title = "Legend")) +
          theme(legend.position = "bottom")

  
  
  
### Scatter plots of male vs female DALY, grouped by age.
### warning, points are paired/duplicated due to age/years.

  scat.all.ages = 
    ggplot(morta, aes(milmale, milfemale)) + 
      geom_abline(intercept = 0, slope = 1, size=.5, linetype = 2, colour="black") +
      geom_point(size=2, shape=21) +
      geom_smooth(se=TRUE, size = 0) +
        scale_x_continuous(name = "male DALY (x10^6)") +
        scale_y_continuous(name = "female DALY (x10^6)") +
        facet_wrap("age", nrow = 2) +
        theme_f1() +
          theme(legend.position="none")
    
  
  
## Scatter plot of male vs female DALY for age to 15-29 only, points labelled with type.
  
  scat.mid =
    ggplot(filter(morta, years == 29.00), aes(milmale, milfemale, label = id)) + 
    geom_abline(intercept = 0, slope = 1, size=.5, linetype = 2, colour = "black") +
    geom_text(size = 2) +
    geom_smooth(se = TRUE, size = 0) +
      scale_x_continuous(name = "male DALY (x10^6)") +
      scale_y_continuous(name = "female DALY (x10^6)") +
      facet_wrap("age", nrow = 2) +
      theme_f1() +
        theme(legend.position = "none")
    
  
  
## Combine four plots into one page ###
  
  all.plots = plot_grid(nrow = 2, 
                        all.pheno.time, sel.pheno.plot, 
                        scat.all.ages, scat.mid, 
                        labels = "AUTO",  label_size = 12, scale = 1)
  

  
## Save combined plots
  
  save_plot("suppfig1_all_sexMortality.pdf", all.plots, base_height = 10)
  save_plot("fig1_sexMortality.pdf", sel.pheno.plot, base_height = 5)

  
  
  ### End stuff ####
  ls()
  rm(list = ls())
  sessionInfo()
  
  Sys.time()
  sink()
  unlink("log_whoMort.txt")
  ###
  ###
  ###
