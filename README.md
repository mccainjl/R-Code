# "# R-Code" 

This repository showcases several research projects I undertook using R while earning my Ph.D. in psychology at the University of Georgia.  InstagramSelfieRatings.R, Comps.R, IRT.R, and EFAandCFA.R are each r script files, which can be opened in a client like RStudio to run the code and conduct the analyses.  You would first want to install R (https://www.r-project.org/) and/or RStudio (https://www.rstudio.com/products/rstudio/download/), then open the script file and/or paste the code from that file to run it in your R console.  NOTE: First thing before you run each file, you should change the working directory in the function setwd() to the directory on your computer where you downloaded this repo.  That way, it can access the files in this folder.

## InstagramSelfieRatings.R:

This file showcases my ability to do data management by demonstrating some merging and reshaping of datasets taken from multiple sources into one dataset.  The purpose of this exercise was to relate personality ratings of selfies given by subject matter experts (i.e. members of our psychology lab) to personality scores of the individuals who provided the selfies.  This deals only with the subject matter experts portion of the data, as the other data is protected by IRB regulations on confidentiality.  This study was published in Computers in Human Behavior (https://www.sciencedirect.com/science/article/pii/S0747563216304782). 

### The problem: 

The story of this project is a data management nightmare.  After Facebook forbade us from scraping data from Instagram (with particiants' permission, of course), we resorted to having participants come in and provide us screenshots of their instagram, from which selfies were then cropped.  We wound up with a sample of 1153 selfies for our sample of 491 college students.  In order to calculate interrater reliability, we needed to have two ratings for each selfie, which doubled the number of ratings required.  Luckily, we had a dedicated team of research assistants, many of whom were co-authors on the resulting paper.  In order to break the work into manageable chunks and give the researchers access, we uploaded the selfies to Qualtrics and displayed them in a survey format using Qualtrics' loop and merge feature.  We broke the data into four parts, each of which would be rated by two of the eight researchers, and because the number of photos causes a strain on Qualtrics' resources, each of those parts was further broken down into two surveys.  Thus, we wound up with:

16 datasets, which had to be consolidated into 8 datasets with two raters each, and then all combined to provide the full set of 1153 selfies, which then had to be consolidated by individual participants (each of which had contributed varying numbers of selfies) before being matched up with personality and other relevant data on the same individuals.

### The Solution: 

The included R script shows how I solved this problem using a subset of the data, using 11 ratings per selfie for two (Zach and Nevill) of the judges' ratings (i.e., four datasets).  The data came in long format with each rating as a row, so I had to reshape each first to wide format, where each row represented one selfie with all of the 11 ratings (as well as other comments left by the judges) as columns.  Then I had to put together the data and calculate the average rating of each selfie by taking the mean of both raters' scores for each row.

## Comps.R:

This script file shows me using the metafor package in R to conduct a meta-analysis of the relationship between narcissism and social media use.  This means that I collected all of the published data (and some unpublished data through personal channels) that had been collected on the subject for academic research.  I took the average of the effect size, or the size of the relationship between narcissism scores and a given measure of social media use (in this case a correlation coefficient, or pearson's R) across all of these studies to come up with what theoretically would be coser to the true relationship between narcissism and social media use.  All data is included in .csvs in the repo.  The results of this study were used to satisfy my comprehensive exams, as well as published in the Journal of Popular Media Culture.  The resulting publication can be downloaded here: https://www.researchgate.net/profile/W_Keith_Campbell/publication/305766785_Narcissism_and_Social_Media_Use_A_Meta-Analytic_Review/links/579fbbeb08ae100d38065bcb/Narcissism-and-Social-Media-Use-A-Meta-Analytic-Review.  

## IRT.R:

This script file shows me using Item Response Theory (IRT) to evaluate a relatively new measure of narcissism, the Communal Narcissism Inventory (CNI), that was created using Classical Test Theory methods.  We used preexisting data with permission from a colleague and evaluated the CNI based on standard Classical Test Theory metrics (such as Cronbach's alpha) and then using several IRT models, including the GRM, the GPCM, and the Nominal models, as well as creating item characteristic curves and comparing individual item performance metrics.  The results of this analysis were presented as a poster at the Society for Personality and Social Psychology (SPSP) 2016 annual meeting (which can be found here: https://osf.io/rwn5h/), and suggest that the CNI contains rather extreme items that most people who are not strongly narcissistic won't endorse, making it less informative about nonclinical populations.  

## EFAandCFA.R:

This script file uses the Communal Narcissism data from the IRT exercise to perform exploratory and confirmatory factor analysis on the Communal Narcissism Inventory.  This is mostly to demonstrate how I use the psych and lavaan packages in R to conduct factor analysis.  I discuss my process of comparing fit indices, considering theory and parsimony, and how I would use my judgment to determine the best factor structure for the data.
