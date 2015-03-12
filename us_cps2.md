# US:CPS: EmploymentStatus classification:: cps2
bdanalytics  

**  **    
**Date: (Fri) Jun 12, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CPSData.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")

#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CPSData.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "cps2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "condition"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "is.na(EmploymentStatus)" # "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- TRUE; glb_is_binomial <- FALSE

glb_rsp_var_raw <- "EmploymentStatus"

# for classification, the response variable has to be a factor
glb_rsp_var <- "EmploymentStatus.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"),
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
    #relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(
    c("Retired", "Unemployed", "Disabled", "Not in Labor Force", "Employed"))
```

```
## [1] Retired            Unemployed         Disabled          
## [4] Not.in.Labor.Force Employed          
## Levels: Disabled Employed Not.in.Labor.Force Retired Unemployed
```

```r
glb_map_rsp_var_to_raw <- function(var) {
    #as.numeric(var) - 1
    #as.numeric(var)
    gsub("\\.", " ", levels(var)[as.numeric(var)])
    #c(" <=50K", " >50K")[as.numeric(var)]
    #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(
    c("Retired", "Unemployed", "Disabled", "Not in Labor Force", "Employed")))
```

```
## [1] "Retired"            "Unemployed"         "Disabled"          
## [4] "Not in Labor Force" "Employed"
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# PeopleInHousehold: The number of people in the interviewee's household.
# Region: The census region where the interviewee lives.
# State: The state where the interviewee lives.
# MetroAreaCode: A code that identifies the metropolitan area in which the interviewee lives (missing if the interviewee does not live in a metropolitan area). The mapping from codes to names of metropolitan areas is provided in the file MetroAreaCodes.csv.
# Age: The age, in years, of the interviewee. 80 represents people aged 80-84, and 85 represents people aged 85 and higher.
# Married: The marriage status of the interviewee.
# Sex: The sex of the interviewee.
# Education: The maximum level of education obtained by the interviewee.
# Race: The race of the interviewee.
# Hispanic: Whether the interviewee is of Hispanic ethnicity.
# CountryOfBirthCode: A code identifying the country of birth of the interviewee. The mapping from codes to names of countries is provided in the file CountryCodes.csv.
# Citizenship: The United States citizenship status of the interviewee.
# EmploymentStatus: The status of employment of the interviewee.
# Industry: The industry of employment of the interviewee (only available if they are employed).

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")

glb_map_vars <- c("MetroAreaCode", "CountryOfBirthCode")
glb_map_urls <- list();
glb_map_urls[["MetroAreaCode"]] <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/MetroAreaCodes.csv"
glb_map_urls[["CountryOfBirthCode"]] <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CountryCodes.csv"

glb_assign_pairs_lst <- NULL; 
glb_assign_pairs_lst[["Married"]] <- list(from=c(NA),
                                            to=c("NA.my"))
glb_assign_pairs_lst[["Education"]] <- list(from=c(NA),
                                            to=c("NA.my"))
glb_assign_pairs_lst[["Industry"]] <- list(from=c(NA),
                                            to=c("NA.my"))
glb_assign_pairs_lst[["MetroArea"]] <- list(from=c(NA),
                                            to=c("NA.my"))
glb_assign_pairs_lst[["Country"]] <- list(from=c(NA),
                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english
glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]
glb_important_terms <- list()
# Remember to use stemmed terms 
glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

glb_log_vars <- NULL # or c("<numeric_var1>", "<numeric_var2>")

# List transformed vars  
glb_exclude_vars_as_features <- c("State.fctr", "MetroArea.my.fctr", "Country.my.fctr")
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        #glb_models_method_vctr <- c("rpart", "rf")
        glb_models_method_vctr <- c("rpart")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](us_cps2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 12.518  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/CPSData.csv..."
## [1] "dimensions of data in ./data/CPSData.csv: 131,302 rows x 14 cols"
##   PeopleInHousehold Region   State MetroAreaCode Age       Married    Sex
## 1                 1  South Alabama         26620  85       Widowed Female
## 2                 3  South Alabama         13820  21 Never Married   Male
## 3                 3  South Alabama         13820  37 Never Married Female
## 4                 3  South Alabama         13820  18 Never Married   Male
## 5                 3  South Alabama         26620  52       Widowed Female
## 6                 3  South Alabama         26620  24 Never Married   Male
##                Education  Race Hispanic CountryOfBirthCode     Citizenship
## 1       Associate degree White        0                 57 Citizen, Native
## 2            High school Black        0                 57 Citizen, Native
## 3            High school Black        0                 57 Citizen, Native
## 4 No high school diploma Black        0                 57 Citizen, Native
## 5       Associate degree White        0                 57 Citizen, Native
## 6      Bachelor's degree White        0                 57 Citizen, Native
##     EmploymentStatus                           Industry
## 1            Retired                               <NA>
## 2         Unemployed Professional and business services
## 3           Disabled                               <NA>
## 4 Not in Labor Force                               <NA>
## 5           Employed Professional and business services
## 6           Employed    Educational and health services
##        PeopleInHousehold Region       State MetroAreaCode Age
## 4535                   7  South    Arkansas         30780   6
## 20007                  1   West    Colorado         14500  57
## 66863                  3  South Mississippi            NA  48
## 95549                  6  South    Oklahoma            NA  17
## 96594                  1   West      Oregon         38900  85
## 129953                 2   West     Wyoming            NA  54
##              Married    Sex              Education  Race Hispanic
## 4535            <NA> Female                   <NA> Black        0
## 20007       Divorced   Male        Master's degree White        0
## 66863  Never Married Female No high school diploma Black        0
## 95549  Never Married   Male No high school diploma White        0
## 96594        Widowed Female No high school diploma White        0
## 129953       Married   Male            High school White        0
##        CountryOfBirthCode     Citizenship   EmploymentStatus      Industry
## 4535                   57 Citizen, Native               <NA>          <NA>
## 20007                  57 Citizen, Native           Employed     Financial
## 66863                  57 Citizen, Native           Disabled          <NA>
## 95549                  57 Citizen, Native Not in Labor Force          <NA>
## 96594                  57 Citizen, Native            Retired          <NA>
## 129953                 57 Citizen, Native           Employed Manufacturing
##        PeopleInHousehold Region   State MetroAreaCode Age       Married
## 131297                 5   West Wyoming            NA  14          <NA>
## 131298                 5   West Wyoming            NA  17 Never Married
## 131299                 5   West Wyoming            NA  37      Divorced
## 131300                 3   West Wyoming            NA  58       Married
## 131301                 3   West Wyoming            NA  53       Married
## 131302                 3   West Wyoming            NA  14          <NA>
##           Sex              Education  Race Hispanic CountryOfBirthCode
## 131297   Male                   <NA> White        0                 57
## 131298   Male No high school diploma White        0                 57
## 131299   Male            High school White        0                 57
## 131300   Male      Bachelor's degree White        0                 57
## 131301 Female       Associate degree White        0                 57
## 131302 Female                   <NA> White        0                 57
##            Citizenship   EmploymentStatus  Industry
## 131297 Citizen, Native               <NA>      <NA>
## 131298 Citizen, Native Not in Labor Force      <NA>
## 131299 Citizen, Native           Employed    Mining
## 131300 Citizen, Native           Employed Financial
## 131301 Citizen, Native Not in Labor Force      <NA>
## 131302 Citizen, Native               <NA>      <NA>
## 'data.frame':	131302 obs. of  14 variables:
##  $ PeopleInHousehold : int  1 3 3 3 3 3 3 2 2 2 ...
##  $ Region            : chr  "South" "South" "South" "South" ...
##  $ State             : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
##  $ MetroAreaCode     : int  26620 13820 13820 13820 26620 26620 26620 33660 33660 26620 ...
##  $ Age               : int  85 21 37 18 52 24 26 71 43 52 ...
##  $ Married           : chr  "Widowed" "Never Married" "Never Married" "Never Married" ...
##  $ Sex               : chr  "Female" "Male" "Female" "Male" ...
##  $ Education         : chr  "Associate degree" "High school" "High school" "No high school diploma" ...
##  $ Race              : chr  "White" "Black" "Black" "Black" ...
##  $ Hispanic          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CountryOfBirthCode: int  57 57 57 57 57 57 57 57 57 57 ...
##  $ Citizenship       : chr  "Citizen, Native" "Citizen, Native" "Citizen, Native" "Citizen, Native" ...
##  $ EmploymentStatus  : chr  "Retired" "Unemployed" "Disabled" "Not in Labor Force" ...
##  $ Industry          : chr  NA "Professional and business services" NA NA ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
if (glb_is_separate_newent_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
##    PeopleInHousehold Region   State MetroAreaCode Age Married    Sex
## 14                 4  South Alabama         26620   2    <NA> Female
## 15                 4  South Alabama         26620   4    <NA>   Male
## 18                 2  South Alabama         13820  13    <NA> Female
## 28                 3  South Alabama         33860   2    <NA> Female
## 35                 6  South Alabama         33860   3    <NA> Female
## 36                 6  South Alabama         33860  11    <NA> Female
##    Education  Race Hispanic CountryOfBirthCode     Citizenship
## 14      <NA> White        0                 57 Citizen, Native
## 15      <NA> White        0                 57 Citizen, Native
## 18      <NA> Black        0                 57 Citizen, Native
## 28      <NA> White        0                 57 Citizen, Native
## 35      <NA> Black        0                 57 Citizen, Native
## 36      <NA> Black        0                 57 Citizen, Native
##    EmploymentStatus Industry
## 14             <NA>     <NA>
## 15             <NA>     <NA>
## 18             <NA>     <NA>
## 28             <NA>     <NA>
## 35             <NA>     <NA>
## 36             <NA>     <NA>
##       PeopleInHousehold    Region       State MetroAreaCode Age Married
## 229                   3     South     Alabama         13820   6    <NA>
## 22928                 3 Northeast Connecticut         76450  13    <NA>
## 51143                 5     South   Louisiana         35380   2    <NA>
## 51529                 4     South   Louisiana         43340  11    <NA>
## 52960                 5 Northeast       Maine            NA   5    <NA>
## 61204                 3   Midwest    Michigan         19820   0    <NA>
##          Sex Education  Race Hispanic CountryOfBirthCode     Citizenship
## 229   Female      <NA> Black        0                 57 Citizen, Native
## 22928 Female      <NA> White        1                 57 Citizen, Native
## 51143 Female      <NA> White        0                 57 Citizen, Native
## 51529 Female      <NA> Black        0                 57 Citizen, Native
## 52960 Female      <NA> White        0                 57 Citizen, Native
## 61204   Male      <NA> White        0                 57 Citizen, Native
##       EmploymentStatus Industry
## 229               <NA>     <NA>
## 22928             <NA>     <NA>
## 51143             <NA>     <NA>
## 51529             <NA>     <NA>
## 52960             <NA>     <NA>
## 61204             <NA>     <NA>
##        PeopleInHousehold Region   State MetroAreaCode Age Married    Sex
## 131282                 5   West Wyoming            NA   4    <NA> Female
## 131283                 5   West Wyoming            NA   9    <NA> Female
## 131285                 2   West Wyoming            NA  21 Married   Male
## 131296                 5   West Wyoming            NA  10    <NA> Female
## 131297                 5   West Wyoming            NA  14    <NA>   Male
## 131302                 3   West Wyoming            NA  14    <NA> Female
##          Education  Race Hispanic CountryOfBirthCode     Citizenship
## 131282        <NA> White        0                 57 Citizen, Native
## 131283        <NA> White        0                 57 Citizen, Native
## 131285 High school White        1                 57 Citizen, Native
## 131296        <NA> White        0                 57 Citizen, Native
## 131297        <NA> White        0                 57 Citizen, Native
## 131302        <NA> White        0                 57 Citizen, Native
##        EmploymentStatus Industry
## 131282             <NA>     <NA>
## 131283             <NA>     <NA>
## 131285             <NA>     <NA>
## 131296             <NA>     <NA>
## 131297             <NA>     <NA>
## 131302             <NA>     <NA>
## 'data.frame':	25789 obs. of  14 variables:
##  $ PeopleInHousehold : int  4 4 2 3 6 6 2 4 3 3 ...
##  $ Region            : chr  "South" "South" "South" "South" ...
##  $ State             : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
##  $ MetroAreaCode     : int  26620 26620 13820 33860 33860 33860 26620 33660 13820 13820 ...
##  $ Age               : int  2 4 13 2 3 11 5 14 5 11 ...
##  $ Married           : chr  NA NA NA NA ...
##  $ Sex               : chr  "Female" "Male" "Female" "Female" ...
##  $ Education         : chr  NA NA NA NA ...
##  $ Race              : chr  "White" "White" "Black" "White" ...
##  $ Hispanic          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CountryOfBirthCode: int  57 57 57 57 57 57 57 57 57 57 ...
##  $ Citizenship       : chr  "Citizen, Native" "Citizen, Native" "Citizen, Native" "Citizen, Native" ...
##  $ EmploymentStatus  : chr  NA NA NA NA ...
##  $ Industry          : chr  NA NA NA NA ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##   PeopleInHousehold Region   State MetroAreaCode Age       Married    Sex
## 1                 1  South Alabama         26620  85       Widowed Female
## 2                 3  South Alabama         13820  21 Never Married   Male
## 3                 3  South Alabama         13820  37 Never Married Female
## 4                 3  South Alabama         13820  18 Never Married   Male
## 5                 3  South Alabama         26620  52       Widowed Female
## 6                 3  South Alabama         26620  24 Never Married   Male
##                Education  Race Hispanic CountryOfBirthCode     Citizenship
## 1       Associate degree White        0                 57 Citizen, Native
## 2            High school Black        0                 57 Citizen, Native
## 3            High school Black        0                 57 Citizen, Native
## 4 No high school diploma Black        0                 57 Citizen, Native
## 5       Associate degree White        0                 57 Citizen, Native
## 6      Bachelor's degree White        0                 57 Citizen, Native
##     EmploymentStatus                           Industry
## 1            Retired                               <NA>
## 2         Unemployed Professional and business services
## 3           Disabled                               <NA>
## 4 Not in Labor Force                               <NA>
## 5           Employed Professional and business services
## 6           Employed    Educational and health services
##        PeopleInHousehold    Region         State MetroAreaCode Age
## 42850                  2   Midwest       Indiana         26900  73
## 59500                  1 Northeast Massachusetts         71650  85
## 84212                  3 Northeast      New York         35620  51
## 92491                  2   Midwest          Ohio         18140  25
## 124956                 4      West    Washington            NA  18
## 126712                 4     South West Virginia         16620  29
##              Married    Sex         Education  Race Hispanic
## 42850        Married Female       High school White        0
## 59500        Widowed Female       High school White        0
## 84212        Married   Male Bachelor's degree White        1
## 92491        Married Female Bachelor's degree White        0
## 124956 Never Married Female       High school Black        1
## 126712       Married   Male       High school White        0
##        CountryOfBirthCode     Citizenship   EmploymentStatus
## 42850                  57 Citizen, Native            Retired
## 59500                  57 Citizen, Native            Retired
## 84212                  57 Citizen, Native           Employed
## 92491                  57 Citizen, Native Not in Labor Force
## 124956                 57 Citizen, Native Not in Labor Force
## 126712                 57 Citizen, Native           Employed
##                     Industry
## 42850                   <NA>
## 59500                   <NA>
## 84212  Public administration
## 92491                   <NA>
## 124956                  <NA>
## 126712                Mining
##        PeopleInHousehold Region   State MetroAreaCode Age       Married
## 131294                 2   West Wyoming            NA  27 Never Married
## 131295                 5   West Wyoming            NA  39      Divorced
## 131298                 5   West Wyoming            NA  17 Never Married
## 131299                 5   West Wyoming            NA  37      Divorced
## 131300                 3   West Wyoming            NA  58       Married
## 131301                 3   West Wyoming            NA  53       Married
##           Sex              Education  Race Hispanic CountryOfBirthCode
## 131294   Male            High school White        0                 57
## 131295 Female       Associate degree White        0                 57
## 131298   Male No high school diploma White        0                 57
## 131299   Male            High school White        0                 57
## 131300   Male      Bachelor's degree White        0                 57
## 131301 Female       Associate degree White        0                 57
##            Citizenship   EmploymentStatus
## 131294 Citizen, Native         Unemployed
## 131295 Citizen, Native Not in Labor Force
## 131298 Citizen, Native Not in Labor Force
## 131299 Citizen, Native           Employed
## 131300 Citizen, Native           Employed
## 131301 Citizen, Native Not in Labor Force
##                                  Industry
## 131294 Professional and business services
## 131295                               <NA>
## 131298                               <NA>
## 131299                             Mining
## 131300                          Financial
## 131301                               <NA>
## 'data.frame':	105513 obs. of  14 variables:
##  $ PeopleInHousehold : int  1 3 3 3 3 3 3 2 2 2 ...
##  $ Region            : chr  "South" "South" "South" "South" ...
##  $ State             : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
##  $ MetroAreaCode     : int  26620 13820 13820 13820 26620 26620 26620 33660 33660 26620 ...
##  $ Age               : int  85 21 37 18 52 24 26 71 43 52 ...
##  $ Married           : chr  "Widowed" "Never Married" "Never Married" "Never Married" ...
##  $ Sex               : chr  "Female" "Male" "Female" "Male" ...
##  $ Education         : chr  "Associate degree" "High school" "High school" "No high school diploma" ...
##  $ Race              : chr  "White" "Black" "Black" "Black" ...
##  $ Hispanic          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CountryOfBirthCode: int  57 57 57 57 57 57 57 57 57 57 ...
##  $ Citizenship       : chr  "Citizen, Native" "Citizen, Native" "Citizen, Native" "Citizen, Native" ...
##  $ EmploymentStatus  : chr  "Retired" "Unemployed" "Disabled" "Not in Labor Force" ...
##  $ Industry          : chr  NA "Professional and business services" NA NA ...
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(glb_trnobs_df)
    glb_newobs_df$.rownames <- rownames(glb_newobs_df)    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

# Combine trnent & newent into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"
glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 12.518 19.517   6.999
## 2 inspect.data          2          0 19.517     NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## Warning in myplot_histogram(glb_allobs_df, glb_rsp_var_raw): converting
## EmploymentStatus to class:factor
```

```
## Loading required package: reshape2
```

![](us_cps2_files/figure-html/inspect.data-1.png) 

```
##       EmploymentStatus.Disabled EmploymentStatus.Employed
## Test                         NA                        NA
## Train                      5712                     61733
##       EmploymentStatus.Not in Labor Force EmploymentStatus.Retired
## Test                                   NA                       NA
## Train                               15246                    18619
##       EmploymentStatus.Unemployed EmploymentStatus.NA
## Test                           NA               25789
## Train                        4203                  NA
##       EmploymentStatus.Disabled EmploymentStatus.Employed
## Test                         NA                        NA
## Train                0.05413551                 0.5850748
##       EmploymentStatus.Not in Labor Force EmploymentStatus.Retired
## Test                                   NA                       NA
## Train                            0.144494                0.1764617
##       EmploymentStatus.Unemployed EmploymentStatus.NA
## Test                           NA                   1
## Train                  0.03983395                  NA
## [1] "numeric data missing in glb_allobs_df: "
## MetroAreaCode 
##         34238 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames 
##               NA                0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##     EmploymentStatus EmploymentStatus.fctr    .n
## 1           Employed              Employed 61733
## 2               <NA>                  <NA> 25789
## 3            Retired               Retired 18619
## 4 Not in Labor Force    Not.in.Labor.Force 15246
## 5           Disabled              Disabled  5712
## 6         Unemployed            Unemployed  4203
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (position_stack).
```

![](us_cps2_files/figure-html/inspect.data-2.png) 

```
##       EmploymentStatus.fctr.Disabled EmploymentStatus.fctr.Employed
## Test                              NA                             NA
## Train                           5712                          61733
##       EmploymentStatus.fctr.Not.in.Labor.Force
## Test                                        NA
## Train                                    15246
##       EmploymentStatus.fctr.Retired EmploymentStatus.fctr.Unemployed
## Test                             NA                               NA
## Train                         18619                             4203
##       EmploymentStatus.fctr.NA
## Test                     25789
## Train                       NA
##       EmploymentStatus.fctr.Disabled EmploymentStatus.fctr.Employed
## Test                              NA                             NA
## Train                     0.05413551                      0.5850748
##       EmploymentStatus.fctr.Not.in.Labor.Force
## Test                                        NA
## Train                                 0.144494
##       EmploymentStatus.fctr.Retired EmploymentStatus.fctr.Unemployed
## Test                             NA                               NA
## Train                     0.1764617                       0.03983395
##       EmploymentStatus.fctr.NA
## Test                         1
## Train                       NA
```

```r
#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

myextract_dates_df <- function(df, vars, id_vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df            <- df[, id_vars, FALSE]        
        dates_df[, rsp_var] <- df[, rsp_var, FALSE]
        #dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df <- cbind(dates_df, data.frame(.date=strptime(df[, var], 
            glb_date_fmts[[var]], tz=glb_date_tzs[[var]])))
#         print(dates_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", ".date")])
#         print(glb_allobs_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", "Date")])     
#         print(head(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), c("ID", "Arrest.fctr", "Date")]))
#         print(head(strptime(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), "Date"], "%m/%e/%y %H:%M"))
        # Wrong data during EST->EDT transition
#         tmp <- strptime("4/7/02 2:00","%m/%e/%y %H:%M:%S"); print(tmp); print(is.na(tmp))
#         dates_df[dates_df$ID == 2068197, .date] <- tmp
#         grep("(.*?) 2:(.*)", glb_allobs_df[is.na(dates_df$.date), "Date"], value=TRUE)
#         dates_df[is.na(dates_df$.date), ".date"] <- 
#             data.frame(.date=strptime(gsub("(.*?) 2:(.*)", "\\1 3:\\2",
#                 glb_allobs_df[is.na(dates_df$.date), "Date"]), "%m/%e/%y %H:%M"))$.date
        if (sum(is.na(dates_df$.date)) > 0) {
            stop("NA POSIX dates for ", var)
            print(df[is.na(dates_df$.date), c(id_vars, rsp_var, var)])
        }    
        
        .date <- dates_df$.date
        dates_df[, paste0(var, ".POSIX")] <- .date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(.date, "%d")), 5) # by month week  
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(.date, "%j"))        
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(.date, "%w"))
        
        # Get US Federal Holidays for relevant years
        require(XML)
        doc.html = htmlTreeParse('http://about.usps.com/news/events-calendar/2012-federal-holidays.htm', useInternal = TRUE)
        
#         # Extract all the paragraphs (HTML tag is p, starting at
#         # the root of the document). Unlist flattens the list to
#         # create a character vector.
#         doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
#         # Replace all \n by spaces
#         doc.text = gsub('\\n', ' ', doc.text)
#         # Join all the elements of the character vector into a single
#         # character string, separated by spaces
#         doc.text = paste(doc.text, collapse = ' ')
        
        # parse the tree by tables
        txt <- unlist(strsplit(xpathSApply(doc.html, "//*/table", xmlValue), "\n"))
        # do some clean up with regular expressions
        txt <- grep("day, ", txt, value=TRUE)
        txt <- trimws(gsub("(.*?)day, (.*)", "\\2", txt))
#         txt <- gsub("\t","",txt)
#         txt <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", txt, perl=TRUE)
#         txt <- txt[!(txt %in% c("", "|"))]
        hldays <- strptime(paste(txt, ", 2012", sep=""), "%B %e, %Y")
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse(format(.date, "%Y-%m-%d") %in% hldays, 1, 0)
        
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%H")))) <= 1)
                   vals else cut(vals, 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%M")))) <= 1)
                   vals else cut(vals, 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%S")))) <= 1)
                   vals else cut(vals, 4) # by quarter-minutes

        dates_df[, paste0(var, ".day.minutes")] <- 
            60 * dates_df[, paste0(var, ".hour")] + 
                 dates_df[, paste0(var, ".minute")]
        if ((unq_vals_n <- length(unique(dates_df[, paste0(var, ".day.minutes")]))) > 1) {
            max_degree <- min(unq_vals_n, 5)
            dates_df[, paste0(var, ".day.minutes.poly.", 1:max_degree)] <- 
                as.matrix(poly(dates_df[, paste0(var, ".day.minutes")], max_degree))
        } else max_degree <- 0   
        
#         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes", 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
# 
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"), 
#                         colorcol_name=rsp_var))
        
#         print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"), 
#                                    xcol_name=paste0(var, ".juliandate"), 
#                         ycol_name=paste0(var, ".day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_box(df=dates_df, ycol_names=paste0(var, ".hour"), 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_bar(df=dates_df, ycol_names=paste0(var, ".hour.fctr"), 
#                                xcol_name=rsp_var, 
#                                colorcol_name=paste0(var, ".hour.fctr")))                
        keep_feats <- paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep="")
        if (max_degree > 0)
            keep_feats <- union(keep_feats, paste(var, 
              paste0(".day.minutes.poly.", 1:max_degree), sep=""))
        keep_feats <- intersect(keep_feats, names(dates_df))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}

# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Add logs of numerics that are not distributed normally ->  do automatically ???
if (!is.null(glb_log_vars)) {
    # Cycle thru glb_log_vars & create logs
    #         <col_name>.log=log(1 + <col.name>),  
    for (col in glb_log_vars) 
        glb_allobs_df[, paste0(col, ".log")] <- log(1 + glb_allobs_df[, col])
    
    # Add raw_vars to glb_exclude_vars_as_features
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_log_vars)
}      

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: PeopleInHousehold"
```

![](us_cps2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: MetroAreaCode"
```

```
## Warning in loop_apply(n, do.ply): Removed 34238 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 34238 rows containing missing
## values (stat_summary).
```

![](us_cps2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: Age"
```

![](us_cps2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: Hispanic"
```

![](us_cps2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: CountryOfBirthCode"
```

![](us_cps2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: .rnorm"
```

![](us_cps2_files/figure-html/inspect.data-8.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

rm(srt_allobs_df, last1, last10, last100, pd)
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object
## 'srt_allobs_df' not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last1'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last10'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last100'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'pd' not
## found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 19.517 42.418  22.901
## 3   scrub.data          2          1 42.419     NA      NA
```

### Step `2.1: scrub data`

```r
# Options:
#   1. Not fill missing vars
#   2. Fill missing numerics with a different algorithm
#   3. Fill missing chars with data based on clusters 

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##         MetroAreaCode EmploymentStatus.fctr 
##                 34238                 25789 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames 
##               NA                0
```

```r
# if (!is.null(glb_force_0_to_NA_vars)) {
#     for (feat in glb_force_0_to_NA_vars) {
#         warning("Forcing ", sum(glb_allobs_df[, feat] == 0),
#                 " obs with ", feat, " 0s to NAs")
#         glb_allobs_df[glb_allobs_df[, feat] == 0, feat] <- NA
#     }
# }

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##         MetroAreaCode EmploymentStatus.fctr 
##                 34238                 25789 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames 
##               NA                0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "encode.data", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 3  scrub.data          2          1 42.419 45.384   2.965
## 4 encode.data          2          2 45.384     NA      NA
```

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_trnobs_df <- mymap_codes(glb_trnobs_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newobs_df <- mymap_codes(glb_newobs_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_trnobs_df$<col_name>.fctr <- factor(glb_trnobs_df$<col_name>, 
#                     as.factor(union(glb_trnobs_df$<col_name>, glb_newobs_df$<col_name>)))
# glb_newobs_df$<col_name>.fctr <- factor(glb_newobs_df$<col_name>, 
#                     as.factor(union(glb_trnobs_df$<col_name>, glb_newobs_df$<col_name>)))

#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}
```

```
## [1] "Reading file ./data/MetroAreaCodes.csv..."
## [1] "dimensions of data in ./data/MetroAreaCodes.csv: 271 rows x 2 cols"
##   Code                           MetroArea
## 1  460         Appleton-Oshkosh-Neenah, WI
## 2 3000   Grand Rapids-Muskegon-Holland, MI
## 3 3160 Greenville-Spartanburg-Anderson, SC
## 4 3610                       Jamestown, NY
## 5 3720          Kalamazoo-Battle Creek, MI
## 6 6450         Portsmouth-Rochester, NH-ME
##      Code                                MetroArea
## 23  12260           Augusta-Richmond County, GA-SC
## 83  22420                                Flint, MI
## 116 27900                               Joplin, MO
## 172 36540              Omaha-Council Bluffs, NE-IA
## 195 40140             Riverside-San Bernardino, CA
## 262 73450 Hartford-West Hartford-East Hartford, CT
##      Code                            MetroArea
## 266 76750          Portland-South Portland, ME
## 267 77200 Providence-Fall River-Warwick, MA-RI
## 268 77350               Rochester-Dover, NH-ME
## 269 78100                   Springfield, MA-CT
## 270 78700                        Waterbury, CT
## 271 79600                     Worcester, MA-CT
## 'data.frame':	271 obs. of  2 variables:
##  $ Code     : int  460 3000 3160 3610 3720 6450 10420 10500 10580 10740 ...
##  $ MetroArea: chr  "Appleton-Oshkosh-Neenah, WI" "Grand Rapids-Muskegon-Holland, MI" "Greenville-Spartanburg-Anderson, SC" "Jamestown, NY" ...
##  - attr(*, "comment")= chr "map_df"
## NULL
##   MetroAreaCode                                          MetroArea    .n
## 1            NA                                               <NA> 34238
## 2         35620 New York-Northern New Jersey-Long Island, NY-NJ-PA  5409
## 3         47900       Washington-Arlington-Alexandria, DC-VA-MD-WV  4177
## 4         31100               Los Angeles-Long Beach-Santa Ana, CA  4102
## 5         37980           Philadelphia-Camden-Wilmington, PA-NJ-DE  2855
## 6         16980                Chicago-Naperville-Joliet, IN-IN-WI  2772
##     MetroAreaCode                            MetroArea   .n
## 4           31100 Los Angeles-Long Beach-Santa Ana, CA 4102
## 23          38060          Phoenix-Mesa-Scottsdale, AZ  971
## 73          24340             Grand Rapids-Wyoming, MI  304
## 92          27140                          Jackson, MS  222
## 208         15180            Brownsville-Harlingen, TX   79
## 214         33140           Michigan City-La Porte, IN   77
##     MetroAreaCode             MetroArea .n
## 260         46660          Valdosta, GA 42
## 261         47580     Warner Robins, GA 42
## 262         14060 Bloomington-Normal IL 40
## 263         44220       Springfield, OH 34
## 264         36140        Ocean City, NJ 30
## 265         14540     Bowling Green, KY 29
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): position_stack requires constant width:
## output may be incorrect
```

![](us_cps2_files/figure-html/encode.data-1.png) 

```
## [1] "Reading file ./data/CountryCodes.csv..."
## [1] "dimensions of data in ./data/CountryCodes.csv: 149 rows x 2 cols"
##   Code                  Country
## 1   57            United States
## 2   66                     Guam
## 3   73              Puerto Rico
## 4   78     U. S. Virgin Islands
## 5   96 Other U. S. Island Areas
## 6  100                  Albania
##     Code Country
## 42   158 Armenia
## 56   207   China
## 64   215   Japan
## 97   323 Bahamas
## 111  361 Bolivia
## 115  365 Ecuador
##     Code                Country
## 144  508                   Fiji
## 145  515            New Zealand
## 146  523                  Tonga
## 147  527                  Samoa
## 148  528 Oceania, not specified
## 149  555              Elsewhere
## 'data.frame':	149 obs. of  2 variables:
##  $ Code   : int  57 66 73 78 96 100 102 103 104 105 ...
##  $ Country: chr  "United States" "Guam" "Puerto Rico" "U. S. Virgin Islands" ...
##  - attr(*, "comment")= chr "map_df"
## NULL
##   CountryOfBirthCode       Country     .n
## 1                 57 United States 115063
## 2                303        Mexico   3921
## 3                233   Philippines    839
## 4                210         India    770
## 5                207         China    581
## 6                 73   Puerto Rico    518
##     CountryOfBirthCode              Country  .n
## 18                 215                Japan 187
## 20                 163               Russia 173
## 86                 160              Belarus  24
## 101                 78 U. S. Virgin Islands  17
## 138                149             Slovakia   6
## 157                142     Northern Ireland   2
##     CountryOfBirthCode          Country .n
## 156                425             <NA>  3
## 157                142 Northern Ireland  2
## 158                228             <NA>  2
## 159                453         Tanzania  2
## 160                430             <NA>  1
## 161                460             <NA>  1
```

```
## Warning in loop_apply(n, do.ply): Removed 17 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): position_stack requires constant width:
## output may be incorrect
```

![](us_cps2_files/figure-html/encode.data-2.png) 

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }
    
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}
```

```
## [1] "Forced Assignments for: Married -> Married.my..."
## [1] "    NA -> NA.my for 25,338 obs"
## [1] "Forced Assignments for: Education -> Education.my..."
## [1] "    NA -> NA.my for 25,338 obs"
## [1] "Forced Assignments for: Industry -> Industry.my..."
## [1] "    NA -> NA.my for 65,060 obs"
## [1] "Forced Assignments for: MetroArea -> MetroArea.my..."
## [1] "    NA -> NA.my for 34,238 obs"
## [1] "Forced Assignments for: Country -> Country.my..."
## [1] "    NA -> NA.my for 176 obs"
```

### Step `2.2: encode data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4         encode.data          2          2 45.384 63.659  18.275
## 5 manage.missing.data          2          3 63.659     NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in : "
##         MetroAreaCode EmploymentStatus.fctr 
##                 34238                 25789 
## [1] "numeric data w/ 0s in : "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames        MetroArea          Country 
##               NA                0               NA               NA 
##       Married.my     Education.my      Industry.my     MetroArea.my 
##                0                0                0                0 
##       Country.my 
##                0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##         MetroAreaCode EmploymentStatus.fctr 
##                 34238                 25789 
## [1] "numeric data w/ 0s in : "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames        MetroArea          Country 
##               NA                0               NA               NA 
##       Married.my     Education.my      Industry.my     MetroArea.my 
##                0                0                0                0 
##       Country.my 
##                0
```

## Step `2.3: manage missing data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 5 manage.missing.data          2          3 63.659 64.136   0.478
## 6    extract.features          3          0 64.137     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 64.144  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 64.144 64.154
## 2 extract.features_factorize.str.vars          2          0 64.155     NA
##   elapsed
## 1    0.01
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##             Region              State            Married 
##           "Region"            "State"          "Married" 
##                Sex          Education               Race 
##              "Sex"        "Education"             "Race" 
##        Citizenship   EmploymentStatus           Industry 
##      "Citizenship" "EmploymentStatus"         "Industry" 
##          .rownames               .src          MetroArea 
##        ".rownames"             ".src"        "MetroArea" 
##            Country         Married.my       Education.my 
##          "Country"       "Married.my"     "Education.my" 
##        Industry.my       MetroArea.my         Country.my 
##      "Industry.my"     "MetroArea.my"       "Country.my"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- factor(glb_allobs_df[, var], 
                        as.factor(unique(glb_allobs_df[, var])))
#         glb_trnobs_df[, paste0(var, ".fctr")] <- factor(glb_trnobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
#         glb_newobs_df[, paste0(var, ".fctr")] <- factor(glb_newobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: Region: # of unique values: 4
```

```
## Warning: Creating factors of string variable: State: # of unique values: 51
```

```
## Warning: Creating factors of string variable: Sex: # of unique values: 2
```

```
## Warning: Creating factors of string variable: Race: # of unique values: 6
```

```
## Warning: Creating factors of string variable: Citizenship: # of unique
## values: 3
```

```
## Warning: Creating factors of string variable: Married.my: # of unique
## values: 6
```

```
## Warning: Creating factors of string variable: Education.my: # of unique
## values: 9
```

```
## Warning: Creating factors of string variable: Industry.my: # of unique
## values: 15
```

```
## Warning: Creating factors of string variable: MetroArea.my: # of unique
## values: 265
```

```
## Warning: Creating factors of string variable: Country.my: # of unique
## values: 145
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(re_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(re_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }
    #tmp_freq_df <- chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE)
    #subset(chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE), grepl("New [[:upper:]]", pattern))
    #chk_pattern_freq("\\bnew (\\W)+")

    chk_subfn <- function(pos_ix) {
        re_str <- gsubfn_args_lst[["re_str"]][[pos_ix]]
        print("re_str:"); print(re_str)
        rp_frmla <- gsubfn_args_lst[["rp_frmla"]][[pos_ix]]        
        print("rp_frmla:"); print(rp_frmla, showEnv=FALSE)
        tmp_vctr <- grep(re_str, txt_vctr, value=TRUE, ignore.case=TRUE)[1:5]
        print("Before:")
        print(tmp_vctr)
        print("After:")            
        print(gsubfn(re_str, rp_frmla, tmp_vctr, ignore.case=TRUE))
    }
    #chk_subfn(1)

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#all.equal(sav_txt_lst[["Headline"]][1:2000], glb_txt_lst[["Headline"]][1:2000])
#all.equal(sav_txt_lst[["Headline"]][1:1000], glb_txt_lst[["Headline"]][1:1000])
#all.equal(sav_txt_lst[["Headline"]][1:500], glb_txt_lst[["Headline"]][1:500])
#all.equal(sav_txt_lst[["Headline"]][1:200], glb_txt_lst[["Headline"]][1:200])
#all.equal(sav_txt_lst[["Headline"]][1:100], glb_txt_lst[["Headline"]][1:100])
#chk.equal( 1, 100)
#chk.equal(51, 100)
#chk.equal(81, 100)
#chk.equal(81,  90)
#chk.equal(81,  85)
#chk.equal(86,  90)
#chk.equal(96, 100)

#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        print(tmp_vctr <- grep("[[:upper:]]\\.", txt_vctr, value=TRUE, ignore.case=FALSE))
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        # Create user-specified pattern vectors 
        #   <txt_var>.P.year.colon
        txt_X_df[, paste0(txt_var_pfx, ".P.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.daily.clip.report")] <-
            as.integer(0 + mycount_pattern_occ("Daily Clip Report", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.fashion.week")] <-
            as.integer(0 + mycount_pattern_occ("Fashion Week", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.first.draft")] <-
            as.integer(0 + mycount_pattern_occ("First Draft", glb_allobs_df[, txt_var]))

#sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
        if (txt_var %in% c("Snippet", "Abstract")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
                as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
                                                   glb_allobs_df[, txt_var]))
        }

#sum(mycount_pattern_occ("[0-9]{4}:", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df$Headline, perl=TRUE) > 0)
#sum(mycount_pattern_occ("No Comment(.*):", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Friday Night Music:", glb_allobs_df$Headline) > 0)
        if (txt_var %in% c("Headline")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.facts.figures")] <-
                as.integer(0 + mycount_pattern_occ("Facts & Figures:", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.friday.night.music")] <-
                as.integer(0 + mycount_pattern_occ("Friday Night Music", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.no.comment.colon")] <-
                as.integer(0 + mycount_pattern_occ("No Comment(.*):", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.on.this.day")] <-
                as.integer(0 + mycount_pattern_occ("On This Day", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.quandary")] <-
                as.integer(0 + mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df[, txt_var], perl=TRUE))
            txt_X_df[, paste0(txt_var_pfx, ".P.readers.respond")] <-
                as.integer(0 + mycount_pattern_occ("Readers Respond", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.recap.colon")] <-
                as.integer(0 + mycount_pattern_occ("Recap:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.s.notebook")] <-
                as.integer(0 + mycount_pattern_occ("s Notebook", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.politic")] <-
                as.integer(0 + mycount_pattern_occ("Today in Politic", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.smallbusiness")] <-
                as.integer(0 + mycount_pattern_occ("Today in Small Business:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.verbatim.colon")] <-
                as.integer(0 + mycount_pattern_occ("Verbatim:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.what.we.are")] <-
                as.integer(0 + mycount_pattern_occ("What We're", glb_allobs_df[, txt_var]))
        }

#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 64.155 64.338
## 3                extract.features_end          3          0 64.339     NA
##   elapsed
## 2   0.183
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 64.155 64.338
## 1                extract.features_bgn          1          0 64.144 64.154
##   elapsed duration
## 2   0.183    0.183
## 1   0.010    0.010
## [1] "Total Elapsed Time: 64.338 secs"
```

![](us_cps2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](us_cps2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn   end elapsed
## 6 extract.features          3          0 64.137 65.84   1.703
## 7     cluster.data          4          0 65.840    NA      NA
```

## Step `4.0: cluster data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor    bgn    end elapsed
## 7    cluster.data          4          0 65.840 66.701   0.861
## 8 select.features          5          0 66.701     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                    id        cor.y exclude.as.feat
## Industry.my.fctr     Industry.my.fctr -0.376894239               0
## Age                               Age  0.279553016               0
## Education.my.fctr   Education.my.fctr -0.095424564               0
## PeopleInHousehold   PeopleInHousehold -0.086274719               0
## Sex.fctr                     Sex.fctr  0.061614520               0
## Married.my.fctr       Married.my.fctr  0.031303760               0
## Country.my.fctr       Country.my.fctr -0.024685468               1
## CountryOfBirthCode CountryOfBirthCode -0.023914164               1
## Hispanic                     Hispanic -0.020230546               0
## Citizenship.fctr     Citizenship.fctr -0.019730560               0
## State.fctr                 State.fctr -0.017050319               1
## Region.fctr               Region.fctr  0.016534173               0
## MetroArea.my.fctr   MetroArea.my.fctr -0.004844306               1
## MetroAreaCode           MetroAreaCode -0.004243514               1
## Race.fctr                   Race.fctr -0.002325650               0
## .rnorm                         .rnorm  0.002199196               0
##                      cor.y.abs
## Industry.my.fctr   0.376894239
## Age                0.279553016
## Education.my.fctr  0.095424564
## PeopleInHousehold  0.086274719
## Sex.fctr           0.061614520
## Married.my.fctr    0.031303760
## Country.my.fctr    0.024685468
## CountryOfBirthCode 0.023914164
## Hispanic           0.020230546
## Citizenship.fctr   0.019730560
## State.fctr         0.017050319
## Region.fctr        0.016534173
## MetroArea.my.fctr  0.004844306
## MetroAreaCode      0.004243514
## Race.fctr          0.002325650
## .rnorm             0.002199196
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
##                    id        cor.y exclude.as.feat   cor.y.abs cor.high.X
## 2                 Age  0.279553016               0 0.279553016         NA
## 15           Sex.fctr  0.061614520               0 0.061614520         NA
## 9     Married.my.fctr  0.031303760               0 0.031303760         NA
## 14        Region.fctr  0.016534173               0 0.016534173         NA
## 1              .rnorm  0.002199196               0 0.002199196         NA
## 13          Race.fctr -0.002325650               0 0.002325650         NA
## 11      MetroAreaCode -0.004243514               1 0.004243514         NA
## 10  MetroArea.my.fctr -0.004844306               1 0.004844306         NA
## 16         State.fctr -0.017050319               1 0.017050319         NA
## 3    Citizenship.fctr -0.019730560               0 0.019730560         NA
## 7            Hispanic -0.020230546               0 0.020230546         NA
## 5  CountryOfBirthCode -0.023914164               1 0.023914164         NA
## 4     Country.my.fctr -0.024685468               1 0.024685468         NA
## 12  PeopleInHousehold -0.086274719               0 0.086274719         NA
## 6   Education.my.fctr -0.095424564               0 0.095424564         NA
## 8    Industry.my.fctr -0.376894239               0 0.376894239         NA
##    freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 2   1.089125   0.063499284   FALSE FALSE    FALSE            FALSE
## 15  1.097632   0.001895501   FALSE FALSE    FALSE            FALSE
## 9   1.797322   0.004738753   FALSE FALSE    FALSE            FALSE
## 14  1.274445   0.003791002   FALSE FALSE    FALSE            FALSE
## 1   1.000000  99.687242330   FALSE FALSE    FALSE            FALSE
## 13  7.990088   0.005686503   FALSE FALSE    FALSE            FALSE
## 11  1.303351   0.250206136   FALSE FALSE    FALSE            FALSE
## 10  6.253270   0.251153886   FALSE FALSE    FALSE            FALSE
## 16  1.706890   0.048335276   FALSE FALSE    FALSE            FALSE
## 3  12.901057   0.002843252   FALSE FALSE    FALSE            FALSE
## 7   7.182474   0.001895501   FALSE FALSE    FALSE            FALSE
## 5  24.005593   0.152587833   FALSE  TRUE    FALSE            FALSE
## 4  24.005593   0.137423825   FALSE  TRUE    FALSE            FALSE
## 12  1.820761   0.014216258   FALSE FALSE    FALSE            FALSE
## 6   1.590536   0.007582004   FALSE FALSE    FALSE            FALSE
## 8   2.615103   0.014216258   FALSE FALSE    FALSE            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](us_cps2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##         MetroAreaCode EmploymentStatus.fctr 
##                 34238                 25789 
## [1] "numeric data w/ 0s in : "
##      Age Hispanic 
##     1283   113008 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##           Region            State          Married              Sex 
##                0                0               NA                0 
##        Education             Race      Citizenship EmploymentStatus 
##               NA                0                0               NA 
##         Industry        .rownames        MetroArea          Country 
##               NA                0               NA               NA 
##       Married.my     Education.my      Industry.my     MetroArea.my 
##                0                0                0                0 
##       Country.my 
##                0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 66.701 74.324   7.624
## 9 partition.data.training          6          0 74.325     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## Loading required package: caTools
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitobs_df) > glb_max_fitent_obs)) {
    warning("glb_fitobs_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newent_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 16 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                                          id exclude.as.feat rsp_var
## EmploymentStatus.fctr EmploymentStatus.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                                          id cor.y exclude.as.feat
## EmploymentStatus.fctr EmploymentStatus.fctr    NA            TRUE
##                       cor.y.abs cor.high.X freqRatio percentUnique zeroVar
## EmploymentStatus.fctr        NA         NA        NA            NA      NA
##                       nzv myNearZV is.cor.y.abs.low interaction.feat
## EmploymentStatus.fctr  NA       NA               NA               NA
##                       rsp_var_raw rsp_var
## EmploymentStatus.fctr          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 131302     36
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 105513     35
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 77145    35
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 28368    35
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 25789    35
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 74.325 75.172   0.847
## 10              fit.models          7          0 75.172     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] Disabled           Employed           Not.in.Labor.Force
## [4] Retired            Unemployed        
## Levels: Disabled Employed Not.in.Labor.Force Retired Unemployed
## [1] "unique.prob:"
## y
##           Employed            Retired Not.in.Labor.Force 
##         0.58508004         0.17645991         0.14449413 
##           Disabled         Unemployed 
##         0.05413183         0.03983408 
## [1] "MFO.val:"
## [1] "Employed"
##             Length Class      Mode     
## unique.vals 5      factor     numeric  
## unique.prob 5      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   5      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     4176                  0       0
##   Employed                  0    45136                  0       0
##   Not.in.Labor.Force        0    11147                  0       0
##   Retired                   0    13613                  0       0
##   Unemployed                0     3073                  0       0
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.5850800             NA      0.5815936      0.5885601      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.5015403            NaN 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     1536                  0       0
##   Employed                  0    16597                  0       0
##   Not.in.Labor.Force        0     4099                  0       0
##   Retired                   0     5006                  0       0
##   Unemployed                0     1130                  0       0
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.5850606      0.0000000      0.5793009      0.5908029      0.5850606 
## AccuracyPValue  McnemarPValue 
##      0.5025399            NaN 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.Accuracy.fit
## 1                      0.471                 0.012          0.58508
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.5815936             0.5885601            NA
##   max.Accuracy.OOB max.AccuracyLower.OOB max.AccuracyUpper.OOB
## 1        0.5850606             0.5793009             0.5908029
##   max.Kappa.OOB
## 1             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 5      factor     numeric  
## unique.prob 5      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   5      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in sum(ni[1:m] * nj[1:m]): integer overflow - use
## sum(as.numeric(.))
```

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                214     2428                616     749
##   Employed               2512    26320               6565    7896
##   Not.in.Labor.Force      586     6573               1672    1920
##   Retired                 739     8000               1971    2402
##   Unemployed              158     1782                466     554
##                     Prediction
## Reference            Unemployed
##   Disabled                  169
##   Employed                 1843
##   Not.in.Labor.Force        396
##   Retired                   501
##   Unemployed                113
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.3982241             NA      0.3947671      0.4016888      0.5850800 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.2051955 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                 78      884                227     283
##   Employed                891     9700               2466    2925
##   Not.in.Labor.Force      236     2352                605     739
##   Retired                 283     2971                680     877
##   Unemployed               72      676                143     201
##                     Prediction
## Reference            Unemployed
##   Disabled                   64
##   Employed                  615
##   Not.in.Labor.Force        167
##   Retired                   195
##   Unemployed                 38
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   0.3982656514  -0.0008038519   0.3925619972   0.4039901490   0.5850606317 
## AccuracyPValue  McnemarPValue 
##   1.0000000000   0.3564451485 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.Accuracy.fit
## 1                      0.304                  0.01        0.3982241
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.3947671             0.4016888            NA
##   max.Accuracy.OOB max.AccuracyLower.OOB max.AccuracyUpper.OOB
## 1        0.3982657              0.392562             0.4039901
##   max.Kappa.OOB
## 1 -0.0008038519
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: Industry.my.fctr, Age"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.252 on full training set
```

```
## Loading required package: rpart.plot
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##          CP nsplit rel error
## 1 0.2520541      0         1
## 
## Node number 1: 77145 observations
##   predicted class=Employed  expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

![](us_cps2_files/figure-html/fit.models_0-1.png) 

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     4176                  0       0
##   Employed                  0    45136                  0       0
##   Not.in.Labor.Force        0    11147                  0       0
##   Retired                   0    13613                  0       0
##   Unemployed                0     3073                  0       0
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.5850800             NA      0.5815936      0.5885601      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.5015403            NaN 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     1536                  0       0
##   Employed                  0    16597                  0       0
##   Not.in.Labor.Force        0     4099                  0       0
##   Retired                   0     5006                  0       0
##   Unemployed                0     1130                  0       0
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.5850606      0.0000000      0.5793009      0.5908029      0.5850606 
## AccuracyPValue  McnemarPValue 
##      0.5025399            NaN 
##               model_id model_method                 feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart Industry.my.fctr, Age               0
##   min.elapsedtime.everything min.elapsedtime.final max.Accuracy.fit
## 1                     13.775                 4.064          0.58508
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.5815936             0.5885601            NA
##   max.Accuracy.OOB max.AccuracyLower.OOB max.AccuracyUpper.OOB
## 1        0.5850606             0.5793009             0.5908029
##   max.Kappa.OOB
## 1             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: Industry.my.fctr, Age"
## Fitting cp = 0 on full training set
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##              CP nsplit rel error
## 1  2.520541e-01      0 1.0000000
## 2  9.075572e-02      1 0.7479459
## 3  1.930707e-02      2 0.6571902
## 4  1.190290e-02      3 0.6378831
## 5  1.134056e-02      4 0.6259802
## 6  1.065325e-02      5 0.6146396
## 7  9.256772e-03      6 0.6039864
## 8  7.591615e-03     19 0.4122903
## 9  6.310725e-03     20 0.4046987
## 10 6.248243e-03     22 0.3920772
## 11 6.217001e-03     23 0.3858290
## 12 6.092037e-03     24 0.3796120
## 13 6.029554e-03     25 0.3735199
## 14 4.717423e-03     26 0.3674904
## 15 4.592458e-03     27 0.3627730
## 16 4.092599e-03     28 0.3581805
## 17 3.842669e-03     29 0.3540879
## 18 3.811428e-03     30 0.3502452
## 19 3.249086e-03     31 0.3464338
## 20 3.238672e-03     32 0.3431847
## 21 2.936674e-03     41 0.2993221
## 22 2.530538e-03     42 0.2963854
## 23 1.749508e-03     43 0.2938549
## 24 1.687026e-03     44 0.2921053
## 25 1.499578e-03     45 0.2904183
## 26 1.030960e-03     46 0.2889187
## 27 9.684776e-04     47 0.2878878
## 28 9.059952e-04     49 0.2859508
## 29 7.497891e-04     50 0.2850448
## 30 6.248243e-04     51 0.2842950
## 31 4.998594e-04     52 0.2836702
## 32 4.373770e-04     53 0.2831704
## 33 3.748946e-04     54 0.2827330
## 34 2.030679e-04     55 0.2823581
## 35 1.718267e-04     57 0.2819520
## 36 6.248243e-05     59 0.2816083
## 37 3.124121e-05     60 0.2815458
## 38 0.000000e+00     63 0.2814521
## 
## Variable importance
##                                                         Age 
##                                                          43 
##                              Industry.my.fctrOther services 
##                                                           7 
##                       Industry.my.fctrPublic administration 
##                                                           5 
##             Industry.my.fctrEducational and health services 
##                                                           5 
##                     Industry.my.fctrLeisure and hospitality 
##                                                           5 
##                Industry.my.fctrTransportation and utilities 
##                                                           5 
##                                       Industry.my.fctrTrade 
##                                                           5 
##                                Industry.my.fctrConstruction 
##                                                           4 
##                                   Industry.my.fctrFinancial 
##                                                           4 
##          Industry.my.fctrProfessional and business services 
##                                                           4 
##                               Industry.my.fctrManufacturing 
##                                                           4 
## Industry.my.fctrAgriculture, forestry, fishing, and hunting 
##                                                           4 
##                                 Industry.my.fctrInformation 
##                                                           3 
##                                      Industry.my.fctrMining 
##                                                           2 
## 
## Node number 1: 77145 observations,    complexity param=0.2520541
##   predicted class=Employed            expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (62433 obs) right son=3 (14712 obs)
##   Primary splits:
##       Age                                                < 64.5 to the left,  improve=8861.8390, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=2323.6000, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=1186.7910, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 983.1942, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 924.0788, (0 missing)
## 
## Node number 2: 62433 observations,    complexity param=0.09075572
##   predicted class=Employed            expected loss=0.3215447  P(node) =0.8092942
##     class counts:  3550 42358 10819  2767  2939
##    probabilities: 0.057 0.678 0.173 0.044 0.047 
##   left son=4 (57429 obs) right son=5 (5004 obs)
##   Primary splits:
##       Age                                                < 18.5 to the right, improve=3249.3410, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1290.5850, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 624.4119, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 515.9783, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 505.1749, (0 missing)
## 
## Node number 3: 14712 observations,    complexity param=0.01930707
##   predicted class=Retired             expected loss=0.2627787  P(node) =0.1907058
##     class counts:   626  2778   328 10846   134
##    probabilities: 0.043 0.189 0.022 0.737 0.009 
##   left son=6 (696 obs) right son=7 (14016 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=756.4649, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=436.5917, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=408.2808, (0 missing)
##       Age                                                < 70.5 to the left,  improve=364.4106, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=241.2709, (0 missing)
## 
## Node number 4: 57429 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.2781347  P(node) =0.7444293
##     class counts:  3490 41456  7012  2765  2706
##    probabilities: 0.061 0.722 0.122 0.048 0.047 
##   left son=8 (10137 obs) right son=9 (47292 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=880.1351, (0 missing)
##       Age                                                < 59.5 to the left,  improve=604.0624, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=395.1759, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=341.2702, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=334.6625, (0 missing)
## 
## Node number 5: 5004 observations,    complexity param=0.0119029
##   predicted class=Not.in.Labor.Force  expected loss=0.2392086  P(node) =0.06486486
##     class counts:    60   902  3807     2   233
##    probabilities: 0.012 0.180 0.761 0.000 0.047 
##   left son=10 (487 obs) right son=11 (4517 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality         < 0.5  to the right, improve=501.58970, (0 missing)
##       Industry.my.fctrTrade                           < 0.5  to the right, improve=243.57050, (0 missing)
##       Age                                             < 16.5 to the right, improve=107.27410, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5  to the right, improve= 66.39350, (0 missing)
##       Industry.my.fctrOther services                  < 0.5  to the right, improve= 38.80304, (0 missing)
## 
## Node number 6: 696 observations
##   predicted class=Employed            expected loss=0.07758621  P(node) =0.009021972
##     class counts:     0   642     0    24    30
##    probabilities: 0.000 0.922 0.000 0.034 0.043 
## 
## Node number 7: 14016 observations,    complexity param=0.01134056
##   predicted class=Retired             expected loss=0.2278824  P(node) =0.1816838
##     class counts:   626  2136   328 10822   104
##    probabilities: 0.045 0.152 0.023 0.772 0.007 
##   left son=14 (402 obs) right son=15 (13614 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=481.2436, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=450.4300, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=265.4408, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=262.8831, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=260.1050, (0 missing)
## 
## Node number 8: 10137 observations
##   predicted class=Employed            expected loss=0.05011345  P(node) =0.1314019
##     class counts:     9  9629    51    13   435
##    probabilities: 0.001 0.950 0.005 0.001 0.043 
## 
## Node number 9: 47292 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3270109  P(node) =0.6130274
##     class counts:  3481 31827  6961  2752  2271
##    probabilities: 0.074 0.673 0.147 0.058 0.048 
##   left son=18 (42249 obs) right son=19 (5043 obs)
##   Primary splits:
##       Age                                                < 59.5 to the left,  improve=682.6394, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=615.9417, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=526.5017, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=511.4015, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=354.9892, (0 missing)
## 
## Node number 10: 487 observations
##   predicted class=Employed            expected loss=0.1457906  P(node) =0.006312788
##     class counts:     0   416    35     0    36
##    probabilities: 0.000 0.854 0.072 0.000 0.074 
## 
## Node number 11: 4517 observations,    complexity param=0.006029554
##   predicted class=Not.in.Labor.Force  expected loss=0.1649325  P(node) =0.05855208
##     class counts:    60   486  3772     2   197
##    probabilities: 0.013 0.108 0.835 0.000 0.044 
##   left son=22 (232 obs) right son=23 (4285 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=298.35260, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve= 84.49187, (0 missing)
##       Age                                                < 16.5 to the right, improve= 55.14438, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve= 49.12110, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 48.50088, (0 missing)
## 
## Node number 14: 402 observations
##   predicted class=Employed            expected loss=0.06716418  P(node) =0.005210966
##     class counts:     2   375     0    12    13
##    probabilities: 0.005 0.933 0.000 0.030 0.032 
## 
## Node number 15: 13614 observations,    complexity param=0.01065325
##   predicted class=Retired             expected loss=0.2059644  P(node) =0.1764729
##     class counts:   624  1761   328 10810    91
##    probabilities: 0.046 0.129 0.024 0.794 0.007 
##   left son=30 (384 obs) right son=31 (13230 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=478.0754, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=281.2735, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=278.5598, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=275.8898, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=270.8294, (0 missing)
## 
## Node number 18: 42249 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3028001  P(node) =0.547657
##     class counts:  2844 29456  6676  1140  2133
##    probabilities: 0.067 0.697 0.158 0.027 0.050 
##   left son=36 (5469 obs) right son=37 (36780 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=491.7069, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=427.5658, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=419.4708, (0 missing)
##       Age                                                < 23.5 to the right, improve=324.6411, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=289.1954, (0 missing)
## 
## Node number 19: 5043 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.5298433  P(node) =0.06537041
##     class counts:   637  2371   285  1612   138
##    probabilities: 0.126 0.470 0.057 0.320 0.027 
##   left son=38 (451 obs) right son=39 (4592 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=169.22760, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=123.99940, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=118.31870, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve= 90.86328, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve= 76.67119, (0 missing)
## 
## Node number 22: 232 observations
##   predicted class=Employed            expected loss=0.1336207  P(node) =0.003007324
##     class counts:     0   201     8     0    23
##    probabilities: 0.000 0.866 0.034 0.000 0.099 
## 
## Node number 23: 4285 observations,    complexity param=0.001749508
##   predicted class=Not.in.Labor.Force  expected loss=0.1215869  P(node) =0.05554475
##     class counts:    60   285  3764     2   174
##    probabilities: 0.014 0.067 0.878 0.000 0.041 
##   left son=46 (97 obs) right son=47 (4188 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services             < 0.5  to the right, improve=95.90269, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=55.61024, (0 missing)
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=55.21431, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=53.14430, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=39.60641, (0 missing)
## 
## Node number 30: 384 observations
##   predicted class=Employed            expected loss=0.0859375  P(node) =0.00497764
##     class counts:     1   351     2    10    20
##    probabilities: 0.003 0.914 0.005 0.026 0.052 
## 
## Node number 31: 13230 observations,    complexity param=0.006310725
##   predicted class=Retired             expected loss=0.1836735  P(node) =0.1714952
##     class counts:   623  1410   326 10800    71
##    probabilities: 0.047 0.107 0.025 0.816 0.005 
##   left son=62 (219 obs) right son=63 (13011 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=297.6126, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=294.7483, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=292.1900, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=286.7155, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=222.5232, (0 missing)
## 
## Node number 36: 5469 observations
##   predicted class=Employed            expected loss=0.07899067  P(node) =0.07089248
##     class counts:     7  5037    40     1   384
##    probabilities: 0.001 0.921 0.007 0.000 0.070 
## 
## Node number 37: 36780 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3360794  P(node) =0.4767645
##     class counts:  2837 24419  6636  1139  1749
##    probabilities: 0.077 0.664 0.180 0.031 0.048 
##   left son=74 (4668 obs) right son=75 (32112 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=571.8024, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=556.1714, (0 missing)
##       Age                                                < 23.5 to the right, improve=394.3466, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=392.5422, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=371.2783, (0 missing)
## 
## Node number 38: 451 observations
##   predicted class=Employed            expected loss=0.05543237  P(node) =0.005846134
##     class counts:     1   426     1     3    20
##    probabilities: 0.002 0.945 0.002 0.007 0.044 
## 
## Node number 39: 4592 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.5764373  P(node) =0.05952427
##     class counts:   636  1945   284  1609   118
##    probabilities: 0.139 0.424 0.062 0.350 0.026 
##   left son=78 (354 obs) right son=79 (4238 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=151.30200, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=145.26730, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=109.69160, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve= 93.33870, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve= 76.52206, (0 missing)
## 
## Node number 46: 97 observations
##   predicted class=Employed            expected loss=0.2474227  P(node) =0.001257372
##     class counts:     0    73    17     0     7
##    probabilities: 0.000 0.753 0.175 0.000 0.072 
## 
## Node number 47: 4188 observations,    complexity param=0.00103096
##   predicted class=Not.in.Labor.Force  expected loss=0.1053009  P(node) =0.05428738
##     class counts:    60   212  3747     2   167
##    probabilities: 0.014 0.051 0.895 0.000 0.040 
##   left son=94 (55 obs) right son=95 (4133 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=58.19129, (0 missing)
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=57.88160, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=55.33508, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=41.20103, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=37.29178, (0 missing)
## 
## Node number 62: 219 observations
##   predicted class=Employed            expected loss=0.05479452  P(node) =0.00283881
##     class counts:     0   207     0     5     7
##    probabilities: 0.000 0.945 0.000 0.023 0.032 
## 
## Node number 63: 13011 observations,    complexity param=0.006248243
##   predicted class=Retired             expected loss=0.1703174  P(node) =0.1686564
##     class counts:   623  1203   326 10795    64
##    probabilities: 0.048 0.092 0.025 0.830 0.005 
##   left son=126 (217 obs) right son=127 (12794 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=304.8379, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=302.3581, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=296.6215, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=229.9711, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=198.7685, (0 missing)
## 
## Node number 74: 4668 observations
##   predicted class=Employed            expected loss=0.07219366  P(node) =0.06050943
##     class counts:     3  4331    31     0   303
##    probabilities: 0.001 0.928 0.007 0.000 0.065 
## 
## Node number 75: 32112 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3744395  P(node) =0.4162551
##     class counts:  2834 20088  6605  1139  1446
##    probabilities: 0.088 0.626 0.206 0.035 0.045 
##   left son=150 (4364 obs) right son=151 (27748 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing           < 0.5  to the right, improve=739.5833, (0 missing)
##       Industry.my.fctrLeisure and hospitality < 0.5  to the right, improve=532.0889, (0 missing)
##       Industry.my.fctrFinancial               < 0.5  to the right, improve=481.7038, (0 missing)
##       Industry.my.fctrConstruction            < 0.5  to the right, improve=415.8067, (0 missing)
##       Age                                     < 23.5 to the right, improve=364.5501, (0 missing)
## 
## Node number 78: 354 observations
##   predicted class=Employed            expected loss=0.07344633  P(node) =0.004588761
##     class counts:     0   328     0     3    23
##    probabilities: 0.000 0.927 0.000 0.008 0.065 
## 
## Node number 79: 4238 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.6184521  P(node) =0.05493551
##     class counts:   636  1617   284  1606    95
##    probabilities: 0.150 0.382 0.067 0.379 0.022 
##   left son=158 (361 obs) right son=159 (3877 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=172.59900, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=128.53290, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=110.07090, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve= 90.67191, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve= 88.92407, (0 missing)
## 
## Node number 94: 55 observations
##   predicted class=Employed            expected loss=0.2363636  P(node) =0.0007129432
##     class counts:     0    42     9     0     4
##    probabilities: 0.000 0.764 0.164 0.000 0.073 
## 
## Node number 95: 4133 observations,    complexity param=0.0009684776
##   predicted class=Not.in.Labor.Force  expected loss=0.09557222  P(node) =0.05357444
##     class counts:    60   170  3738     2   163
##    probabilities: 0.015 0.041 0.904 0.000 0.039 
##   left son=190 (61 obs) right son=191 (4072 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=59.50594, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=56.66486, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=42.16854, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=38.28705, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=27.80150, (0 missing)
## 
## Node number 126: 217 observations
##   predicted class=Employed            expected loss=0.06912442  P(node) =0.002812885
##     class counts:     0   202     1     2    12
##    probabilities: 0.000 0.931 0.005 0.009 0.055 
## 
## Node number 127: 12794 observations,    complexity param=0.006217001
##   predicted class=Retired             expected loss=0.1564014  P(node) =0.1658435
##     class counts:   623  1001   325 10793    52
##    probabilities: 0.049 0.078 0.025 0.844 0.004 
##   left son=254 (222 obs) right son=255 (12572 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=312.9565, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=306.9477, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=237.7246, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=205.6450, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=192.0610, (0 missing)
## 
## Node number 150: 4364 observations
##   predicted class=Employed            expected loss=0.06393217  P(node) =0.0565688
##     class counts:     2  4085    15     3   259
##    probabilities: 0.000 0.936 0.003 0.001 0.059 
## 
## Node number 151: 27748 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.4232737  P(node) =0.3596863
##     class counts:  2832 16003  6590  1136  1187
##    probabilities: 0.102 0.577 0.237 0.041 0.043 
##   left son=302 (3747 obs) right son=303 (24001 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=743.1407, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=644.6047, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=571.9225, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=471.3980, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=458.7793, (0 missing)
## 
## Node number 158: 361 observations
##   predicted class=Employed            expected loss=0.09418283  P(node) =0.0046795
##     class counts:     1   327     0     5    28
##    probabilities: 0.003 0.906 0.000 0.014 0.078 
## 
## Node number 159: 3877 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5870518  P(node) =0.05025601
##     class counts:   635  1290   284  1601    67
##    probabilities: 0.164 0.333 0.073 0.413 0.017 
##   left son=318 (242 obs) right son=319 (3635 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=152.6798, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=131.5716, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=108.9098, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=106.1004, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=102.9366, (0 missing)
## 
## Node number 190: 61 observations
##   predicted class=Employed            expected loss=0.3606557  P(node) =0.0007907188
##     class counts:     0    39     9     0    13
##    probabilities: 0.000 0.639 0.148 0.000 0.213 
## 
## Node number 191: 4072 observations,    complexity param=0.0009684776
##   predicted class=Not.in.Labor.Force  expected loss=0.08423379  P(node) =0.05278372
##     class counts:    60   131  3729     2   150
##    probabilities: 0.015 0.032 0.916 0.000 0.037 
##   left son=382 (42 obs) right son=383 (4030 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=58.11483, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=43.22076, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=39.39532, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=28.43424, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=24.96236, (0 missing)
## 
## Node number 254: 222 observations
##   predicted class=Employed            expected loss=0.07207207  P(node) =0.002877698
##     class counts:     0   206     1     7     8
##    probabilities: 0.000 0.928 0.005 0.032 0.036 
## 
## Node number 255: 12572 observations,    complexity param=0.006092037
##   predicted class=Retired             expected loss=0.1420617  P(node) =0.1629658
##     class counts:   623   795   324 10786    44
##    probabilities: 0.050 0.063 0.026 0.858 0.003 
##   left son=510 (215 obs) right son=511 (12357 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=317.9065, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=245.9525, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=212.9354, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=198.9882, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=198.2375, (0 missing)
## 
## Node number 302: 3747 observations
##   predicted class=Employed            expected loss=0.09634374  P(node) =0.04857087
##     class counts:     2  3386    43     3   313
##    probabilities: 0.001 0.904 0.011 0.001 0.084 
## 
## Node number 303: 24001 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.4743136  P(node) =0.3111154
##     class counts:  2830 12617  6547  1133   874
##    probabilities: 0.118 0.526 0.273 0.047 0.036 
##   left son=606 (2696 obs) right son=607 (21305 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=849.2295, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=771.8555, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=616.0829, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=606.5147, (0 missing)
##       Age                                          < 26.5 to the right, improve=546.5604, (0 missing)
## 
## Node number 318: 242 observations
##   predicted class=Employed            expected loss=0.04132231  P(node) =0.00313695
##     class counts:     0   232     0     1     9
##    probabilities: 0.000 0.959 0.000 0.004 0.037 
## 
## Node number 319: 3635 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5598349  P(node) =0.04711906
##     class counts:   635  1058   284  1600    58
##    probabilities: 0.175 0.291 0.078 0.440 0.016 
##   left son=638 (226 obs) right son=639 (3409 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=151.0187, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=125.3594, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=121.5617, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=117.7939, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve=108.4572, (0 missing)
## 
## Node number 382: 42 observations
##   predicted class=Employed            expected loss=0.1904762  P(node) =0.0005444293
##     class counts:     0    34     2     0     6
##    probabilities: 0.000 0.810 0.048 0.000 0.143 
## 
## Node number 383: 4030 observations,    complexity param=0.0007497891
##   predicted class=Not.in.Labor.Force  expected loss=0.0751861  P(node) =0.05223929
##     class counts:    60    97  3727     2   144
##    probabilities: 0.015 0.024 0.925 0.000 0.036 
##   left son=766 (30 obs) right son=767 (4000 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=44.11017, (0 missing)
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=40.32177, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=28.97328, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=25.41738, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=21.77548, (0 missing)
## 
## Node number 510: 215 observations
##   predicted class=Employed            expected loss=0.06976744  P(node) =0.00278696
##     class counts:     0   200     0     5    10
##    probabilities: 0.000 0.930 0.000 0.023 0.047 
## 
## Node number 511: 12357 observations,    complexity param=0.004717423
##   predicted class=Retired             expected loss=0.127539  P(node) =0.1601789
##     class counts:   623   595   324 10781    34
##    probabilities: 0.050 0.048 0.026 0.872 0.003 
##   left son=1022 (158 obs) right son=1023 (12199 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=254.39830, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=220.42630, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=206.10560, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=205.04780, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 80.04601, (0 missing)
## 
## Node number 606: 2696 observations
##   predicted class=Employed            expected loss=0.03783383  P(node) =0.03494718
##     class counts:     0  2594     7     1    94
##    probabilities: 0.000 0.962 0.003 0.000 0.035 
## 
## Node number 607: 21305 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.5295471  P(node) =0.2761683
##     class counts:  2830 10023  6540  1132   780
##    probabilities: 0.133 0.470 0.307 0.053 0.037 
##   left son=1214 (2819 obs) right son=1215 (18486 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=1012.4120, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve= 789.2268, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve= 783.2167, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve= 680.7946, (0 missing)
##       Age                                          < 26.5 to the right, improve= 519.2679, (0 missing)
## 
## Node number 638: 226 observations
##   predicted class=Employed            expected loss=0.0619469  P(node) =0.002929548
##     class counts:     0   212     0     6     8
##    probabilities: 0.000 0.938 0.000 0.027 0.035 
## 
## Node number 639: 3409 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5324142  P(node) =0.04418951
##     class counts:   635   846   284  1594    50
##    probabilities: 0.186 0.248 0.083 0.468 0.015 
##   left son=1278 (199 obs) right son=1279 (3210 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=143.50720, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=138.57380, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=134.13540, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=123.53620, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 76.56807, (0 missing)
## 
## Node number 766: 30 observations
##   predicted class=Employed            expected loss=0.1666667  P(node) =0.0003888781
##     class counts:     0    25     1     0     4
##    probabilities: 0.000 0.833 0.033 0.000 0.133 
## 
## Node number 767: 4000 observations,    complexity param=0.0006248243
##   predicted class=Not.in.Labor.Force  expected loss=0.0685  P(node) =0.05185041
##     class counts:    60    72  3726     2   140
##    probabilities: 0.015 0.018 0.931 0.001 0.035 
##   left son=1534 (36 obs) right son=1535 (3964 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=41.016160, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=29.377430, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=25.758690, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=22.067800, (0 missing)
##       Age                                          < 17.5 to the left,  improve= 9.052118, (0 missing)
## 
## Node number 1022: 158 observations
##   predicted class=Employed            expected loss=0.03164557  P(node) =0.002048091
##     class counts:     0   153     0     2     3
##    probabilities: 0.000 0.968 0.000 0.013 0.019 
## 
## Node number 1023: 12199 observations,    complexity param=0.004092599
##   predicted class=Retired             expected loss=0.116403  P(node) =0.1581308
##     class counts:   623   442   324 10779    31
##    probabilities: 0.051 0.036 0.027 0.884 0.003 
##   left son=2046 (144 obs) right son=2047 (12055 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=226.33950, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=211.73010, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=210.42740, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 82.21373, (0 missing)
##       Age                                          < 69.5 to the left,  improve= 54.47671, (0 missing)
## 
## Node number 1214: 2819 observations
##   predicted class=Employed            expected loss=0.08300816  P(node) =0.03654158
##     class counts:     2  2585    17     3   212
##    probabilities: 0.001 0.917 0.006 0.001 0.075 
## 
## Node number 1215: 18486 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.5976415  P(node) =0.2396267
##     class counts:  2828  7438  6523  1129   568
##    probabilities: 0.153 0.402 0.353 0.061 0.031 
##   left son=2430 (2078 obs) right son=2431 (16408 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=1045.4260, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=1044.0940, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve= 916.3636, (0 missing)
##       Age                                          < 26.5 to the right, improve= 504.6234, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 382.4998, (0 missing)
## 
## Node number 1278: 199 observations
##   predicted class=Employed            expected loss=0.09547739  P(node) =0.002579558
##     class counts:     1   180     0     3    15
##    probabilities: 0.005 0.905 0.000 0.015 0.075 
## 
## Node number 1279: 3210 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5043614  P(node) =0.04160996
##     class counts:   634   666   284  1591    35
##    probabilities: 0.198 0.207 0.088 0.496 0.011 
##   left son=2558 (182 obs) right son=2559 (3028 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=156.29700, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=151.07500, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=139.20960, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 86.10358, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 47.69273, (0 missing)
## 
## Node number 1534: 36 observations
##   predicted class=Employed            expected loss=0.3333333  P(node) =0.0004666537
##     class counts:     0    24     4     0     8
##    probabilities: 0.000 0.667 0.111 0.000 0.222 
## 
## Node number 1535: 3964 observations,    complexity param=0.0004998594
##   predicted class=Not.in.Labor.Force  expected loss=0.06104945  P(node) =0.05138376
##     class counts:    60    48  3722     2   132
##    probabilities: 0.015 0.012 0.939 0.001 0.033 
##   left son=3070 (17 obs) right son=3071 (3947 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=29.804300, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=26.116770, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=22.374470, (0 missing)
##       Age                                          < 17.5 to the left,  improve= 6.419004, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve= 5.571355, (0 missing)
## 
## Node number 2046: 144 observations
##   predicted class=Employed            expected loss=0.09027778  P(node) =0.001866615
##     class counts:     0   131     1     0    12
##    probabilities: 0.000 0.910 0.007 0.000 0.083 
## 
## Node number 2047: 12055 observations,    complexity param=0.003842669
##   predicted class=Retired             expected loss=0.1058482  P(node) =0.1562642
##     class counts:   623   311   323 10779    19
##    probabilities: 0.052 0.026 0.027 0.894 0.002 
##   left son=4094 (139 obs) right son=4095 (11916 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=216.92580, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=215.39200, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 84.21834, (0 missing)
##       Age                                          < 69.5 to the left,  improve= 42.58446, (0 missing)
##       Industry.my.fctrMining                       < 0.5  to the right, improve= 21.11926, (0 missing)
## 
## Node number 2430: 2078 observations
##   predicted class=Employed            expected loss=0.05293551  P(node) =0.02693629
##     class counts:     0  1968     6     2   102
##    probabilities: 0.000 0.947 0.003 0.001 0.049 
## 
## Node number 2431: 16408 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.6028157  P(node) =0.2126904
##     class counts:  2828  5470  6517  1127   466
##    probabilities: 0.172 0.333 0.397 0.069 0.028 
##   left son=4862 (1984 obs) right son=4863 (14424 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=1338.0430, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=1188.9860, (0 missing)
##       Age                                                         < 41.5 to the right, improve= 523.3124, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 491.6569, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 358.8742, (0 missing)
## 
## Node number 2558: 182 observations
##   predicted class=Employed            expected loss=0.07142857  P(node) =0.002359194
##     class counts:     0   169     0     1    12
##    probabilities: 0.000 0.929 0.000 0.005 0.066 
## 
## Node number 2559: 3028 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.4749009  P(node) =0.03925076
##     class counts:   634   497   284  1590    23
##    probabilities: 0.209 0.164 0.094 0.525 0.008 
##   left son=5118 (174 obs) right son=5119 (2854 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=170.19160, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=156.88020, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 96.83817, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 53.76955, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 30.07375, (0 missing)
## 
## Node number 3070: 17 observations
##   predicted class=Employed            expected loss=0.05882353  P(node) =0.0002203642
##     class counts:     0    16     0     0     1
##    probabilities: 0.000 0.941 0.000 0.000 0.059 
## 
## Node number 3071: 3947 observations,    complexity param=0.000437377
##   predicted class=Not.in.Labor.Force  expected loss=0.05700532  P(node) =0.05116339
##     class counts:    60    32  3722     2   131
##    probabilities: 0.015 0.008 0.943 0.001 0.033 
##   left son=6142 (14 obs) right son=6143 (3933 obs)
##   Primary splits:
##       Industry.my.fctrFinancial             < 0.5  to the right, improve=26.335330, (0 missing)
##       Industry.my.fctrInformation           < 0.5  to the right, improve=22.561670, (0 missing)
##       Industry.my.fctrPublic administration < 0.5  to the right, improve= 5.647505, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 4.420919, (0 missing)
## 
## Node number 4094: 139 observations
##   predicted class=Employed            expected loss=0.08633094  P(node) =0.001801802
##     class counts:     0   127     1     4     7
##    probabilities: 0.000 0.914 0.007 0.029 0.050 
## 
## Node number 4095: 11916 observations,    complexity param=0.003811428
##   predicted class=Retired             expected loss=0.09575361  P(node) =0.1544624
##     class counts:   623   184   322 10775    12
##    probabilities: 0.052 0.015 0.027 0.904 0.001 
##   left son=8190 (128 obs) right son=8191 (11788 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=220.28360, (0 missing)
##       Industry.my.fctrInformation           < 0.5  to the right, improve= 86.19226, (0 missing)
##       Age                                   < 69.5 to the left,  improve= 32.25519, (0 missing)
##       Industry.my.fctrMining                < 0.5  to the right, improve= 21.65488, (0 missing)
## 
## Node number 4862: 1984 observations
##   predicted class=Employed            expected loss=0.03175403  P(node) =0.0257178
##     class counts:     1  1921     5     3    54
##    probabilities: 0.001 0.968 0.003 0.002 0.027 
## 
## Node number 4863: 14424 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.5485302  P(node) =0.1869726
##     class counts:  2827  3549  6512  1124   412
##    probabilities: 0.196 0.246 0.451 0.078 0.029 
##   left son=9726 (1952 obs) right son=9727 (12472 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=1586.3880, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 648.4297, (0 missing)
##       Age                                                         < 41.5 to the right, improve= 548.0990, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 472.6345, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 283.5885, (0 missing)
## 
## Node number 5118: 174 observations
##   predicted class=Employed            expected loss=0.04597701  P(node) =0.002255493
##     class counts:     0   166     1     5     2
##    probabilities: 0.000 0.954 0.006 0.029 0.011 
## 
## Node number 5119: 2854 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.4446391  P(node) =0.03699527
##     class counts:   634   331   283  1585    21
##    probabilities: 0.222 0.116 0.099 0.555 0.007 
##   left son=10238 (162 obs) right son=10239 (2692 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=177.24140, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=109.19970, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 60.72953, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 33.66763, (0 missing)
##       Age                                                         < 61.5 to the left,  improve= 24.28196, (0 missing)
## 
## Node number 6142: 14 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0001814764
##     class counts:     0    14     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 6143: 3933 observations,    complexity param=0.0003748946
##   predicted class=Not.in.Labor.Force  expected loss=0.05364861  P(node) =0.05098192
##     class counts:    60    18  3722     2   131
##    probabilities: 0.015 0.005 0.946 0.001 0.033 
##   left son=12286 (12 obs) right son=12287 (3921 obs)
##   Primary splits:
##       Industry.my.fctrInformation           < 0.5  to the right, improve=22.722820, (0 missing)
##       Industry.my.fctrPublic administration < 0.5  to the right, improve= 5.713301, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 3.677531, (0 missing)
## 
## Node number 8190: 128 observations
##   predicted class=Employed            expected loss=0.046875  P(node) =0.001659213
##     class counts:     0   122     0     0     6
##    probabilities: 0.000 0.953 0.000 0.000 0.047 
## 
## Node number 8191: 11788 observations,    complexity param=0.001499578
##   predicted class=Retired             expected loss=0.08593485  P(node) =0.1528032
##     class counts:   623    62   322 10775     6
##    probabilities: 0.053 0.005 0.027 0.914 0.001 
##   left son=16382 (54 obs) right son=16383 (11734 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=88.14437, (0 missing)
##       Age                         < 66.5 to the left,  improve=24.62345, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve=22.18490, (0 missing)
## 
## Node number 9726: 1952 observations
##   predicted class=Employed            expected loss=0.07633197  P(node) =0.025303
##     class counts:     2  1803    13     0   134
##    probabilities: 0.001 0.924 0.007 0.000 0.069 
## 
## Node number 9727: 12472 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.4789128  P(node) =0.1616696
##     class counts:  2825  1746  6499  1124   278
##    probabilities: 0.227 0.140 0.521 0.090 0.022 
##   left son=19454 (864 obs) right son=19455 (11608 obs)
##   Primary splits:
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=875.54450, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=636.69090, (0 missing)
##       Age                                                         < 44.5 to the right, improve=628.14260, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=376.68880, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 25.43837, (0 missing)
## 
## Node number 10238: 162 observations
##   predicted class=Employed            expected loss=0.0617284  P(node) =0.002099942
##     class counts:     0   152     0     2     8
##    probabilities: 0.000 0.938 0.000 0.012 0.049 
## 
## Node number 10239: 2692 observations,    complexity param=0.002936674
##   predicted class=Retired             expected loss=0.4119614  P(node) =0.03489533
##     class counts:   634   179   283  1583    13
##    probabilities: 0.236 0.066 0.105 0.588 0.005 
##   left son=20478 (101 obs) right son=20479 (2591 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=122.93260, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 68.49246, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 37.63462, (0 missing)
##       Age                                                         < 61.5 to the left,  improve= 23.23422, (0 missing)
## 
## Node number 12286: 12 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0001555512
##     class counts:     0    12     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 12287: 3921 observations,    complexity param=6.248243e-05
##   predicted class=Not.in.Labor.Force  expected loss=0.05075236  P(node) =0.05082637
##     class counts:    60     6  3722     2   131
##    probabilities: 0.015 0.002 0.949 0.001 0.033 
##   left son=24574 (8 obs) right son=24575 (3913 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=5.770378, (0 missing)
##       Age                                   < 17.5 to the left,  improve=3.474435, (0 missing)
## 
## Node number 16382: 54 observations
##   predicted class=Employed            expected loss=0.09259259  P(node) =0.0006999806
##     class counts:     0    49     0     1     4
##    probabilities: 0.000 0.907 0.000 0.019 0.074 
## 
## Node number 16383: 11734 observations,    complexity param=0.0001718267
##   predicted class=Retired             expected loss=0.08181353  P(node) =0.1521032
##     class counts:   623    13   322 10774     2
##    probabilities: 0.053 0.001 0.027 0.918 0.000 
##   left son=32766 (1428 obs) right son=32767 (10306 obs)
##   Primary splits:
##       Age                    < 66.5 to the left,  improve=22.59645, (0 missing)
##       Industry.my.fctrMining < 0.5  to the right, improve=22.40638, (0 missing)
## 
## Node number 19454: 864 observations
##   predicted class=Employed            expected loss=0.07407407  P(node) =0.01119969
##     class counts:     0   800     6     1    57
##    probabilities: 0.000 0.926 0.007 0.001 0.066 
## 
## Node number 19455: 11608 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.4406444  P(node) =0.1504699
##     class counts:  2825   946  6493  1123   221
##    probabilities: 0.243 0.081 0.559 0.097 0.019 
##   left son=38910 (642 obs) right son=38911 (10966 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=738.10700, (0 missing)
##       Age                                                         < 44.5 to the right, improve=681.98090, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=433.82320, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 26.34573, (0 missing)
## 
## Node number 20478: 101 observations
##   predicted class=Employed            expected loss=0.04950495  P(node) =0.001309223
##     class counts:     0    96     0     2     3
##    probabilities: 0.000 0.950 0.000 0.020 0.030 
## 
## Node number 20479: 2591 observations,    complexity param=0.001687026
##   predicted class=Retired             expected loss=0.3898109  P(node) =0.0335861
##     class counts:   634    83   283  1581    10
##    probabilities: 0.245 0.032 0.109 0.610 0.004 
##   left son=40958 (60 obs) right son=40959 (2531 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=74.12104, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve=40.50551, (0 missing)
##       Age                         < 61.5 to the left,  improve=22.94488, (0 missing)
## 
## Node number 24574: 8 observations
##   predicted class=Employed            expected loss=0.375  P(node) =0.0001037008
##     class counts:     0     5     3     0     0
##    probabilities: 0.000 0.625 0.375 0.000 0.000 
## 
## Node number 24575: 3913 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.04957833  P(node) =0.05072267
##     class counts:    60     1  3719     2   131
##    probabilities: 0.015 0.000 0.950 0.001 0.033 
## 
## Node number 32766: 1428 observations
##   predicted class=Retired             expected loss=0.17507  P(node) =0.0185106
##     class counts:   172     2    76  1178     0
##    probabilities: 0.120 0.001 0.053 0.825 0.000 
## 
## Node number 32767: 10306 observations,    complexity param=0.0001718267
##   predicted class=Retired             expected loss=0.06889191  P(node) =0.1335926
##     class counts:   451    11   246  9596     2
##    probabilities: 0.044 0.001 0.024 0.931 0.000 
##   left son=65534 (12 obs) right son=65535 (10294 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=20.600160, (0 missing)
##       Age                    < 70.5 to the left,  improve= 4.406275, (0 missing)
## 
## Node number 38910: 642 observations
##   predicted class=Employed            expected loss=0.07165109  P(node) =0.008321991
##     class counts:     2   596     7     1    36
##    probabilities: 0.003 0.928 0.011 0.002 0.056 
## 
## Node number 38911: 10966 observations,    complexity param=0.007591615
##   predicted class=Not.in.Labor.Force  expected loss=0.4085355  P(node) =0.1421479
##     class counts:  2823   350  6486  1122   185
##    probabilities: 0.257 0.032 0.591 0.102 0.017 
##   left son=77822 (4676 obs) right son=77823 (6290 obs)
##   Primary splits:
##       Age                          < 43.5 to the right, improve=710.94140, (0 missing)
##       Industry.my.fctrMining       < 0.5  to the right, improve=485.31870, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 27.25333, (0 missing)
## 
## Node number 40958: 60 observations
##   predicted class=Employed            expected loss=0.1  P(node) =0.0007777562
##     class counts:     0    54     0     0     6
##    probabilities: 0.000 0.900 0.000 0.000 0.100 
## 
## Node number 40959: 2531 observations,    complexity param=0.0009059952
##   predicted class=Retired             expected loss=0.3753457  P(node) =0.03280835
##     class counts:   634    29   283  1581     4
##    probabilities: 0.250 0.011 0.112 0.625 0.002 
##   left son=81918 (29 obs) right son=81919 (2502 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=42.32207, (0 missing)
##       Age                    < 61.5 to the left,  improve=22.59598, (0 missing)
## 
## Node number 65534: 12 observations
##   predicted class=Employed            expected loss=0.08333333  P(node) =0.0001555512
##     class counts:     0    11     0     0     1
##    probabilities: 0.000 0.917 0.000 0.000 0.083 
## 
## Node number 65535: 10294 observations
##   predicted class=Retired             expected loss=0.06780649  P(node) =0.133437
##     class counts:   451     0   246  9596     1
##    probabilities: 0.044 0.000 0.024 0.932 0.000 
## 
## Node number 77822: 4676 observations,    complexity param=0.004592458
##   predicted class=Disabled            expected loss=0.5902481  P(node) =0.06061313
##     class counts:  1916   147  1673   927    13
##    probabilities: 0.410 0.031 0.358 0.198 0.003 
##   left son=155644 (152 obs) right son=155645 (4524 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=190.3492, (0 missing)
##       Age                    < 53.5 to the left,  improve= 92.3401, (0 missing)
## 
## Node number 77823: 6290 observations,    complexity param=0.006310725
##   predicted class=Not.in.Labor.Force  expected loss=0.2348172  P(node) =0.08153477
##     class counts:   907   203  4813   195   172
##    probabilities: 0.144 0.032 0.765 0.031 0.027 
##   left son=155646 (211 obs) right son=155647 (6079 obs)
##   Primary splits:
##       Industry.my.fctrMining       < 0.5  to the right, improve=319.75960, (0 missing)
##       Age                          < 26.5 to the right, improve= 58.87956, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 29.62228, (0 missing)
## 
## Node number 81918: 29 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0003759155
##     class counts:     0    29     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 81919: 2502 observations
##   predicted class=Retired             expected loss=0.3681055  P(node) =0.03243243
##     class counts:   634     0   283  1581     4
##    probabilities: 0.253 0.000 0.113 0.632 0.002 
## 
## Node number 155644: 152 observations
##   predicted class=Employed            expected loss=0.03289474  P(node) =0.001970316
##     class counts:     0   147     0     0     5
##    probabilities: 0.000 0.967 0.000 0.000 0.033 
## 
## Node number 155645: 4524 observations,    complexity param=0.003249086
##   predicted class=Disabled            expected loss=0.576481  P(node) =0.05864282
##     class counts:  1916     0  1673   927     8
##    probabilities: 0.424 0.000 0.370 0.205 0.002 
##   left son=311290 (2474 obs) right son=311291 (2050 obs)
##   Primary splits:
##       Age < 53.5 to the left,  improve=96.05737, (0 missing)
## 
## Node number 155646: 211 observations
##   predicted class=Employed            expected loss=0.03791469  P(node) =0.002735109
##     class counts:     0   203     1     0     7
##    probabilities: 0.000 0.962 0.005 0.000 0.033 
## 
## Node number 155647: 6079 observations,    complexity param=0.0002030679
##   predicted class=Not.in.Labor.Force  expected loss=0.2084224  P(node) =0.07879966
##     class counts:   907     0  4812   195   165
##    probabilities: 0.149 0.000 0.792 0.032 0.027 
##   left son=311294 (3558 obs) right son=311295 (2521 obs)
##   Primary splits:
##       Age                          < 26.5 to the right, improve=51.65542, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve=30.42549, (0 missing)
## 
## Node number 311290: 2474 observations,    complexity param=0.002530538
##   predicted class=Not.in.Labor.Force  expected loss=0.5359741  P(node) =0.03206948
##     class counts:  1044     0  1148   276     6
##    probabilities: 0.422 0.000 0.464 0.112 0.002 
##   left son=622580 (1136 obs) right son=622581 (1338 obs)
##   Primary splits:
##       Age < 49.5 to the right, improve=20.27439, (0 missing)
## 
## Node number 311291: 2050 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.5746341  P(node) =0.02657334
##     class counts:   872     0   525   651     2
##    probabilities: 0.425 0.000 0.256 0.318 0.001 
##   left son=622582 (987 obs) right son=622583 (1063 obs)
##   Primary splits:
##       Age < 56.5 to the left,  improve=11.09287, (0 missing)
## 
## Node number 311294: 3558 observations,    complexity param=0.0002030679
##   predicted class=Not.in.Labor.Force  expected loss=0.2613828  P(node) =0.04612094
##     class counts:   724     0  2628   153    53
##    probabilities: 0.203 0.000 0.739 0.043 0.015 
##   left son=622588 (3545 obs) right son=622589 (13 obs)
##   Primary splits:
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve=20.34446, (0 missing)
##       Age                          < 37.5 to the right, improve=13.39218, (0 missing)
## 
## Node number 311295: 2521 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.1336771  P(node) =0.03267872
##     class counts:   183     0  2184    42   112
##    probabilities: 0.073 0.000 0.866 0.017 0.044 
## 
## Node number 622580: 1136 observations
##   predicted class=Disabled            expected loss=0.5440141  P(node) =0.01472552
##     class counts:   518     0   437   180     1
##    probabilities: 0.456 0.000 0.385 0.158 0.001 
## 
## Node number 622581: 1338 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.4686099  P(node) =0.01734396
##     class counts:   526     0   711    96     5
##    probabilities: 0.393 0.000 0.531 0.072 0.004 
## 
## Node number 622582: 987 observations
##   predicted class=Disabled            expected loss=0.5542047  P(node) =0.01279409
##     class counts:   440     0   293   253     1
##    probabilities: 0.446 0.000 0.297 0.256 0.001 
## 
## Node number 622583: 1063 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.593603  P(node) =0.01377925
##     class counts:   432     0   232   398     1
##    probabilities: 0.406 0.000 0.218 0.374 0.001 
##   left son=1245166 (314 obs) right son=1245167 (749 obs)
##   Primary splits:
##       Age < 57.5 to the left,  improve=1.268934, (0 missing)
## 
## Node number 622588: 3545 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.2586742  P(node) =0.04595243
##     class counts:   724     0  2628   153    40
##    probabilities: 0.204 0.000 0.741 0.043 0.011 
## 
## Node number 622589: 13 observations
##   predicted class=Unemployed          expected loss=0  P(node) =0.0001685138
##     class counts:     0     0     0     0    13
##    probabilities: 0.000 0.000 0.000 0.000 1.000 
## 
## Node number 1245166: 314 observations
##   predicted class=Disabled            expected loss=0.5732484  P(node) =0.004070257
##     class counts:   134     0    76   104     0
##    probabilities: 0.427 0.000 0.242 0.331 0.000 
## 
## Node number 1245167: 749 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.6021362  P(node) =0.00970899
##     class counts:   298     0   156   294     1
##    probabilities: 0.398 0.000 0.208 0.393 0.001 
##   left son=2490334 (385 obs) right son=2490335 (364 obs)
##   Primary splits:
##       Age < 58.5 to the left,  improve=0.1735246, (0 missing)
## 
## Node number 2490334: 385 observations
##   predicted class=Disabled            expected loss=0.6  P(node) =0.004990602
##     class counts:   154     0    84   147     0
##    probabilities: 0.400 0.000 0.218 0.382 0.000 
## 
## Node number 2490335: 364 observations
##   predicted class=Retired             expected loss=0.5961538  P(node) =0.004718387
##     class counts:   144     0    72   147     1
##    probabilities: 0.396 0.000 0.198 0.404 0.003 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##       1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04)  
##         2) Age< 64.5 62433 20075 Employed (0.057 0.68 0.17 0.044 0.047)  
##           4) Age>=18.5 57429 15973 Employed (0.061 0.72 0.12 0.048 0.047)  
##             8) Industry.my.fctrEducational and health services>=0.5 10137   508 Employed (0.00089 0.95 0.005 0.0013 0.043) *
##             9) Industry.my.fctrEducational and health services< 0.5 47292 15465 Employed (0.074 0.67 0.15 0.058 0.048)  
##              18) Age< 59.5 42249 12793 Employed (0.067 0.7 0.16 0.027 0.05)  
##                36) Industry.my.fctrTrade>=0.5 5469   432 Employed (0.0013 0.92 0.0073 0.00018 0.07) *
##                37) Industry.my.fctrTrade< 0.5 36780 12361 Employed (0.077 0.66 0.18 0.031 0.048)  
##                  74) Industry.my.fctrProfessional and business services>=0.5 4668   337 Employed (0.00064 0.93 0.0066 0 0.065) *
##                  75) Industry.my.fctrProfessional and business services< 0.5 32112 12024 Employed (0.088 0.63 0.21 0.035 0.045)  
##                   150) Industry.my.fctrManufacturing>=0.5 4364   279 Employed (0.00046 0.94 0.0034 0.00069 0.059) *
##                   151) Industry.my.fctrManufacturing< 0.5 27748 11745 Employed (0.1 0.58 0.24 0.041 0.043)  
##                     302) Industry.my.fctrLeisure and hospitality>=0.5 3747   361 Employed (0.00053 0.9 0.011 0.0008 0.084) *
##                     303) Industry.my.fctrLeisure and hospitality< 0.5 24001 11384 Employed (0.12 0.53 0.27 0.047 0.036)  
##                       606) Industry.my.fctrFinancial>=0.5 2696   102 Employed (0 0.96 0.0026 0.00037 0.035) *
##                       607) Industry.my.fctrFinancial< 0.5 21305 11282 Employed (0.13 0.47 0.31 0.053 0.037)  
##                        1214) Industry.my.fctrConstruction>=0.5 2819   234 Employed (0.00071 0.92 0.006 0.0011 0.075) *
##                        1215) Industry.my.fctrConstruction< 0.5 18486 11048 Employed (0.15 0.4 0.35 0.061 0.031)  
##                          2430) Industry.my.fctrTransportation and utilities>=0.5 2078   110 Employed (0 0.95 0.0029 0.00096 0.049) *
##                          2431) Industry.my.fctrTransportation and utilities< 0.5 16408  9891 Not.in.Labor.Force (0.17 0.33 0.4 0.069 0.028)  
##                            4862) Industry.my.fctrPublic administration>=0.5 1984    63 Employed (0.0005 0.97 0.0025 0.0015 0.027) *
##                            4863) Industry.my.fctrPublic administration< 0.5 14424  7912 Not.in.Labor.Force (0.2 0.25 0.45 0.078 0.029)  
##                              9726) Industry.my.fctrOther services>=0.5 1952   149 Employed (0.001 0.92 0.0067 0 0.069) *
##                              9727) Industry.my.fctrOther services< 0.5 12472  5973 Not.in.Labor.Force (0.23 0.14 0.52 0.09 0.022)  
##                               19454) Industry.my.fctrInformation>=0.5 864    64 Employed (0 0.93 0.0069 0.0012 0.066) *
##                               19455) Industry.my.fctrInformation< 0.5 11608  5115 Not.in.Labor.Force (0.24 0.081 0.56 0.097 0.019)  
##                                 38910) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 642    46 Employed (0.0031 0.93 0.011 0.0016 0.056) *
##                                 38911) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 10966  4480 Not.in.Labor.Force (0.26 0.032 0.59 0.1 0.017)  
##                                   77822) Age>=43.5 4676  2760 Disabled (0.41 0.031 0.36 0.2 0.0028)  
##                                    155644) Industry.my.fctrMining>=0.5 152     5 Employed (0 0.97 0 0 0.033) *
##                                    155645) Industry.my.fctrMining< 0.5 4524  2608 Disabled (0.42 0 0.37 0.2 0.0018)  
##                                      311290) Age< 53.5 2474  1326 Not.in.Labor.Force (0.42 0 0.46 0.11 0.0024)  
##                                        622580) Age>=49.5 1136   618 Disabled (0.46 0 0.38 0.16 0.00088) *
##                                        622581) Age< 49.5 1338   627 Not.in.Labor.Force (0.39 0 0.53 0.072 0.0037) *
##                                      311291) Age>=53.5 2050  1178 Disabled (0.43 0 0.26 0.32 0.00098)  
##                                        622582) Age< 56.5 987   547 Disabled (0.45 0 0.3 0.26 0.001) *
##                                        622583) Age>=56.5 1063   631 Disabled (0.41 0 0.22 0.37 0.00094)  
##                                         1245166) Age< 57.5 314   180 Disabled (0.43 0 0.24 0.33 0) *
##                                         1245167) Age>=57.5 749   451 Disabled (0.4 0 0.21 0.39 0.0013)  
##                                           2490334) Age< 58.5 385   231 Disabled (0.4 0 0.22 0.38 0) *
##                                           2490335) Age>=58.5 364   217 Retired (0.4 0 0.2 0.4 0.0027) *
##                                   77823) Age< 43.5 6290  1477 Not.in.Labor.Force (0.14 0.032 0.77 0.031 0.027)  
##                                    155646) Industry.my.fctrMining>=0.5 211     8 Employed (0 0.96 0.0047 0 0.033) *
##                                    155647) Industry.my.fctrMining< 0.5 6079  1267 Not.in.Labor.Force (0.15 0 0.79 0.032 0.027)  
##                                      311294) Age>=26.5 3558   930 Not.in.Labor.Force (0.2 0 0.74 0.043 0.015)  
##                                        622588) Industry.my.fctrArmed forces< 0.5 3545   917 Not.in.Labor.Force (0.2 0 0.74 0.043 0.011) *
##                                        622589) Industry.my.fctrArmed forces>=0.5 13     0 Unemployed (0 0 0 0 1) *
##                                      311295) Age< 26.5 2521   337 Not.in.Labor.Force (0.073 0 0.87 0.017 0.044) *
##              19) Age>=59.5 5043  2672 Employed (0.13 0.47 0.057 0.32 0.027)  
##                38) Industry.my.fctrTrade>=0.5 451    25 Employed (0.0022 0.94 0.0022 0.0067 0.044) *
##                39) Industry.my.fctrTrade< 0.5 4592  2647 Employed (0.14 0.42 0.062 0.35 0.026)  
##                  78) Industry.my.fctrProfessional and business services>=0.5 354    26 Employed (0 0.93 0 0.0085 0.065) *
##                  79) Industry.my.fctrProfessional and business services< 0.5 4238  2621 Employed (0.15 0.38 0.067 0.38 0.022)  
##                   158) Industry.my.fctrManufacturing>=0.5 361    34 Employed (0.0028 0.91 0 0.014 0.078) *
##                   159) Industry.my.fctrManufacturing< 0.5 3877  2276 Retired (0.16 0.33 0.073 0.41 0.017)  
##                     318) Industry.my.fctrFinancial>=0.5 242    10 Employed (0 0.96 0 0.0041 0.037) *
##                     319) Industry.my.fctrFinancial< 0.5 3635  2035 Retired (0.17 0.29 0.078 0.44 0.016)  
##                       638) Industry.my.fctrConstruction>=0.5 226    14 Employed (0 0.94 0 0.027 0.035) *
##                       639) Industry.my.fctrConstruction< 0.5 3409  1815 Retired (0.19 0.25 0.083 0.47 0.015)  
##                        1278) Industry.my.fctrLeisure and hospitality>=0.5 199    19 Employed (0.005 0.9 0 0.015 0.075) *
##                        1279) Industry.my.fctrLeisure and hospitality< 0.5 3210  1619 Retired (0.2 0.21 0.088 0.5 0.011)  
##                          2558) Industry.my.fctrTransportation and utilities>=0.5 182    13 Employed (0 0.93 0 0.0055 0.066) *
##                          2559) Industry.my.fctrTransportation and utilities< 0.5 3028  1438 Retired (0.21 0.16 0.094 0.53 0.0076)  
##                            5118) Industry.my.fctrPublic administration>=0.5 174     8 Employed (0 0.95 0.0057 0.029 0.011) *
##                            5119) Industry.my.fctrPublic administration< 0.5 2854  1269 Retired (0.22 0.12 0.099 0.56 0.0074)  
##                             10238) Industry.my.fctrOther services>=0.5 162    10 Employed (0 0.94 0 0.012 0.049) *
##                             10239) Industry.my.fctrOther services< 0.5 2692  1109 Retired (0.24 0.066 0.11 0.59 0.0048)  
##                               20478) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 101     5 Employed (0 0.95 0 0.02 0.03) *
##                               20479) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 2591  1010 Retired (0.24 0.032 0.11 0.61 0.0039)  
##                                 40958) Industry.my.fctrInformation>=0.5 60     6 Employed (0 0.9 0 0 0.1) *
##                                 40959) Industry.my.fctrInformation< 0.5 2531   950 Retired (0.25 0.011 0.11 0.62 0.0016)  
##                                   81918) Industry.my.fctrMining>=0.5 29     0 Employed (0 1 0 0 0) *
##                                   81919) Industry.my.fctrMining< 0.5 2502   921 Retired (0.25 0 0.11 0.63 0.0016) *
##           5) Age< 18.5 5004  1197 Not.in.Labor.Force (0.012 0.18 0.76 0.0004 0.047)  
##            10) Industry.my.fctrLeisure and hospitality>=0.5 487    71 Employed (0 0.85 0.072 0 0.074) *
##            11) Industry.my.fctrLeisure and hospitality< 0.5 4517   745 Not.in.Labor.Force (0.013 0.11 0.84 0.00044 0.044)  
##              22) Industry.my.fctrTrade>=0.5 232    31 Employed (0 0.87 0.034 0 0.099) *
##              23) Industry.my.fctrTrade< 0.5 4285   521 Not.in.Labor.Force (0.014 0.067 0.88 0.00047 0.041)  
##                46) Industry.my.fctrEducational and health services>=0.5 97    24 Employed (0 0.75 0.18 0 0.072) *
##                47) Industry.my.fctrEducational and health services< 0.5 4188   441 Not.in.Labor.Force (0.014 0.051 0.89 0.00048 0.04)  
##                  94) Industry.my.fctrOther services>=0.5 55    13 Employed (0 0.76 0.16 0 0.073) *
##                  95) Industry.my.fctrOther services< 0.5 4133   395 Not.in.Labor.Force (0.015 0.041 0.9 0.00048 0.039)  
##                   190) Industry.my.fctrProfessional and business services>=0.5 61    22 Employed (0 0.64 0.15 0 0.21) *
##                   191) Industry.my.fctrProfessional and business services< 0.5 4072   343 Not.in.Labor.Force (0.015 0.032 0.92 0.00049 0.037)  
##                     382) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 42     8 Employed (0 0.81 0.048 0 0.14) *
##                     383) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 4030   303 Not.in.Labor.Force (0.015 0.024 0.92 0.0005 0.036)  
##                       766) Industry.my.fctrConstruction>=0.5 30     5 Employed (0 0.83 0.033 0 0.13) *
##                       767) Industry.my.fctrConstruction< 0.5 4000   274 Not.in.Labor.Force (0.015 0.018 0.93 0.0005 0.035)  
##                        1534) Industry.my.fctrManufacturing>=0.5 36    12 Employed (0 0.67 0.11 0 0.22) *
##                        1535) Industry.my.fctrManufacturing< 0.5 3964   242 Not.in.Labor.Force (0.015 0.012 0.94 0.0005 0.033)  
##                          3070) Industry.my.fctrTransportation and utilities>=0.5 17     1 Employed (0 0.94 0 0 0.059) *
##                          3071) Industry.my.fctrTransportation and utilities< 0.5 3947   225 Not.in.Labor.Force (0.015 0.0081 0.94 0.00051 0.033)  
##                            6142) Industry.my.fctrFinancial>=0.5 14     0 Employed (0 1 0 0 0) *
##                            6143) Industry.my.fctrFinancial< 0.5 3933   211 Not.in.Labor.Force (0.015 0.0046 0.95 0.00051 0.033)  
##                             12286) Industry.my.fctrInformation>=0.5 12     0 Employed (0 1 0 0 0) *
##                             12287) Industry.my.fctrInformation< 0.5 3921   199 Not.in.Labor.Force (0.015 0.0015 0.95 0.00051 0.033)  
##                               24574) Industry.my.fctrPublic administration>=0.5 8     3 Employed (0 0.63 0.37 0 0) *
##                               24575) Industry.my.fctrPublic administration< 0.5 3913   194 Not.in.Labor.Force (0.015 0.00026 0.95 0.00051 0.033) *
##         3) Age>=64.5 14712  3866 Retired (0.043 0.19 0.022 0.74 0.0091)  
##           6) Industry.my.fctrEducational and health services>=0.5 696    54 Employed (0 0.92 0 0.034 0.043) *
##           7) Industry.my.fctrEducational and health services< 0.5 14016  3194 Retired (0.045 0.15 0.023 0.77 0.0074)  
##            14) Industry.my.fctrTrade>=0.5 402    27 Employed (0.005 0.93 0 0.03 0.032) *
##            15) Industry.my.fctrTrade< 0.5 13614  2804 Retired (0.046 0.13 0.024 0.79 0.0067)  
##              30) Industry.my.fctrProfessional and business services>=0.5 384    33 Employed (0.0026 0.91 0.0052 0.026 0.052) *
##              31) Industry.my.fctrProfessional and business services< 0.5 13230  2430 Retired (0.047 0.11 0.025 0.82 0.0054)  
##                62) Industry.my.fctrFinancial>=0.5 219    12 Employed (0 0.95 0 0.023 0.032) *
##                63) Industry.my.fctrFinancial< 0.5 13011  2216 Retired (0.048 0.092 0.025 0.83 0.0049)  
##                 126) Industry.my.fctrManufacturing>=0.5 217    15 Employed (0 0.93 0.0046 0.0092 0.055) *
##                 127) Industry.my.fctrManufacturing< 0.5 12794  2001 Retired (0.049 0.078 0.025 0.84 0.0041)  
##                   254) Industry.my.fctrOther services>=0.5 222    16 Employed (0 0.93 0.0045 0.032 0.036) *
##                   255) Industry.my.fctrOther services< 0.5 12572  1786 Retired (0.05 0.063 0.026 0.86 0.0035)  
##                     510) Industry.my.fctrLeisure and hospitality>=0.5 215    15 Employed (0 0.93 0 0.023 0.047) *
##                     511) Industry.my.fctrLeisure and hospitality< 0.5 12357  1576 Retired (0.05 0.048 0.026 0.87 0.0028)  
##                      1022) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 158     5 Employed (0 0.97 0 0.013 0.019) *
##                      1023) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 12199  1420 Retired (0.051 0.036 0.027 0.88 0.0025)  
##                        2046) Industry.my.fctrConstruction>=0.5 144    13 Employed (0 0.91 0.0069 0 0.083) *
##                        2047) Industry.my.fctrConstruction< 0.5 12055  1276 Retired (0.052 0.026 0.027 0.89 0.0016)  
##                          4094) Industry.my.fctrTransportation and utilities>=0.5 139    12 Employed (0 0.91 0.0072 0.029 0.05) *
##                          4095) Industry.my.fctrTransportation and utilities< 0.5 11916  1141 Retired (0.052 0.015 0.027 0.9 0.001)  
##                            8190) Industry.my.fctrPublic administration>=0.5 128     6 Employed (0 0.95 0 0 0.047) *
##                            8191) Industry.my.fctrPublic administration< 0.5 11788  1013 Retired (0.053 0.0053 0.027 0.91 0.00051)  
##                             16382) Industry.my.fctrInformation>=0.5 54     5 Employed (0 0.91 0 0.019 0.074) *
##                             16383) Industry.my.fctrInformation< 0.5 11734   960 Retired (0.053 0.0011 0.027 0.92 0.00017)  
##                               32766) Age< 66.5 1428   250 Retired (0.12 0.0014 0.053 0.82 0) *
##                               32767) Age>=66.5 10306   710 Retired (0.044 0.0011 0.024 0.93 0.00019)  
##                                 65534) Industry.my.fctrMining>=0.5 12     1 Employed (0 0.92 0 0 0.083) *
##                                 65535) Industry.my.fctrMining< 0.5 10294   698 Retired (0.044 0 0.024 0.93 9.7e-05) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

![](us_cps2_files/figure-html/fit.models_0-2.png) 

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled               1246       36               1493    1401
##   Employed                  0    45133                  1       2
##   Not.in.Labor.Force      890      338               9242     677
##   Retired                 684      134                293   12502
##   Unemployed                2     2764                288       6
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                 13
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8832199             NA      0.8809327      0.8854782      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                468       24                534     510
##   Employed                  0    16594                  1       2
##   Not.in.Labor.Force      348      133               3369     248
##   Retired                 244       41                111    4610
##   Unemployed                3     1006                117       1
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          1
##   Retired                     0
##   Unemployed                  3
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.828257e-01   7.967286e-01   8.790261e-01   8.865467e-01   5.850606e-01 
## AccuracyPValue  McnemarPValue 
##   0.000000e+00  4.618022e-316 
##                    model_id model_method                 feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart Industry.my.fctr, Age
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      4.443                 3.816
##   max.Accuracy.fit max.AccuracyLower.fit max.AccuracyUpper.fit
## 1        0.8832199             0.8809327             0.8854782
##   max.Kappa.fit max.Accuracy.OOB max.AccuracyLower.OOB
## 1            NA        0.8828257             0.8790261
##   max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8865467     0.7967286
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)

# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: Industry.my.fctr, Age"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0193 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method =
## ifelse(glb_is_regression, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](us_cps2_files/figure-html/fit.models_0-3.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##           CP nsplit rel error
## 1 0.25205411      0 1.0000000
## 2 0.09075572      1 0.7479459
## 3 0.01930707      2 0.6571902
## 
## Variable importance
## Age 
## 100 
## 
## Node number 1: 77145 observations,    complexity param=0.2520541
##   predicted class=Employed            expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (62433 obs) right son=3 (14712 obs)
##   Primary splits:
##       Age                                                < 64.5 to the left,  improve=8861.8390, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=2323.6000, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=1186.7910, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 983.1942, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 924.0788, (0 missing)
## 
## Node number 2: 62433 observations,    complexity param=0.09075572
##   predicted class=Employed            expected loss=0.3215447  P(node) =0.8092942
##     class counts:  3550 42358 10819  2767  2939
##    probabilities: 0.057 0.678 0.173 0.044 0.047 
##   left son=4 (57429 obs) right son=5 (5004 obs)
##   Primary splits:
##       Age                                                < 18.5 to the right, improve=3249.3410, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1290.5850, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 624.4119, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 515.9783, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 505.1749, (0 missing)
## 
## Node number 3: 14712 observations
##   predicted class=Retired             expected loss=0.2627787  P(node) =0.1907058
##     class counts:   626  2778   328 10846   134
##    probabilities: 0.043 0.189 0.022 0.737 0.009 
## 
## Node number 4: 57429 observations
##   predicted class=Employed            expected loss=0.2781347  P(node) =0.7444293
##     class counts:  3490 41456  7012  2765  2706
##    probabilities: 0.061 0.722 0.122 0.048 0.047 
## 
## Node number 5: 5004 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.2392086  P(node) =0.06486486
##     class counts:    60   902  3807     2   233
##    probabilities: 0.012 0.180 0.761 0.000 0.047 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04)  
##   2) Age< 64.5 62433 20075 Employed (0.057 0.68 0.17 0.044 0.047)  
##     4) Age>=18.5 57429 15973 Employed (0.061 0.72 0.12 0.048 0.047) *
##     5) Age< 18.5 5004  1197 Not.in.Labor.Force (0.012 0.18 0.76 0.0004 0.047) *
##   3) Age>=64.5 14712  3866 Retired (0.043 0.19 0.022 0.74 0.0091) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

![](us_cps2_files/figure-html/fit.models_0-4.png) 

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     3490                 60     626
##   Employed                  0    41456                902    2778
##   Not.in.Labor.Force        0     7012               3807     328
##   Retired                   0     2765                  2   10846
##   Unemployed                0     2706                233     134
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7273187             NA      0.7241611      0.7304591      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     1289                 21     226
##   Employed                  0    15206                365    1026
##   Not.in.Labor.Force        0     2581               1416     102
##   Retired                   0     1028                  1    3977
##   Unemployed                0      986                 97      47
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7261351      0.4752624      0.7209052      0.7313187      0.5850606 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
##          model_id model_method                 feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart Industry.my.fctr, Age               3
##   min.elapsedtime.everything min.elapsedtime.final max.Accuracy.fit
## 1                     17.853                  3.83          0.73205
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.7241611             0.7304591     0.4874172
##   max.Accuracy.OOB max.AccuracyLower.OOB max.AccuracyUpper.OOB
## 1        0.7261351             0.7209052             0.7313187
##   max.Kappa.OOB max.AccuracySD.fit max.KappaSD.fit
## 1     0.4752624        0.004250054     0.002519487
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    

# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.rpart"
## [1] "    indep_vars: Age, Sex.fctr, Married.my.fctr, Region.fctr, .rnorm, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0193 on full training set
```

```
## Warning in myfit_mdl(model_id = "Low.cor.X", model_method =
## ifelse(glb_is_regression, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](us_cps2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##           CP nsplit rel error
## 1 0.25205411      0 1.0000000
## 2 0.09075572      1 0.7479459
## 3 0.01930707      2 0.6571902
## 
## Variable importance
##                    Age Married.my.fctrWidowed 
##                     88                     12 
## 
## Node number 1: 77145 observations,    complexity param=0.2520541
##   predicted class=Employed            expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (62433 obs) right son=3 (14712 obs)
##   Primary splits:
##       Age                                             < 64.5     to the left,  improve=8861.839, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5      to the right, improve=2323.600, (0 missing)
##       Married.my.fctrWidowed                          < 0.5      to the left,  improve=2200.537, (0 missing)
##       PeopleInHousehold                               < 2.5      to the right, improve=1947.586, (0 missing)
##       Married.my.fctrNever Married                    < 0.5      to the right, improve=1420.837, (0 missing)
##   Surrogate splits:
##       Married.my.fctrWidowed < 0.5      to the left,  agree=0.843, adj=0.178, (0 split)
##       .rnorm                 < 3.985793 to the left,  agree=0.809, adj=0.000, (0 split)
## 
## Node number 2: 62433 observations,    complexity param=0.09075572
##   predicted class=Employed            expected loss=0.3215447  P(node) =0.8092942
##     class counts:  3550 42358 10819  2767  2939
##    probabilities: 0.057 0.678 0.173 0.044 0.047 
##   left son=4 (57429 obs) right son=5 (5004 obs)
##   Primary splits:
##       Age                                             < 18.5     to the right, improve=3249.3410, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5      to the right, improve=1290.5850, (0 missing)
##       Married.my.fctrNever Married                    < 0.5      to the left,  improve=1036.8480, (0 missing)
##       Married.my.fctrMarried                          < 0.5      to the right, improve= 695.1223, (0 missing)
##       Industry.my.fctrTrade                           < 0.5      to the right, improve= 624.4119, (0 missing)
##   Surrogate splits:
##       .rnorm < 3.928213 to the left,  agree=0.92, adj=0, (0 split)
## 
## Node number 3: 14712 observations
##   predicted class=Retired             expected loss=0.2627787  P(node) =0.1907058
##     class counts:   626  2778   328 10846   134
##    probabilities: 0.043 0.189 0.022 0.737 0.009 
## 
## Node number 4: 57429 observations
##   predicted class=Employed            expected loss=0.2781347  P(node) =0.7444293
##     class counts:  3490 41456  7012  2765  2706
##    probabilities: 0.061 0.722 0.122 0.048 0.047 
## 
## Node number 5: 5004 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.2392086  P(node) =0.06486486
##     class counts:    60   902  3807     2   233
##    probabilities: 0.012 0.180 0.761 0.000 0.047 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04)  
##   2) Age< 64.5 62433 20075 Employed (0.057 0.68 0.17 0.044 0.047)  
##     4) Age>=18.5 57429 15973 Employed (0.061 0.72 0.12 0.048 0.047) *
##     5) Age< 18.5 5004  1197 Not.in.Labor.Force (0.012 0.18 0.76 0.0004 0.047) *
##   3) Age>=64.5 14712  3866 Retired (0.043 0.19 0.022 0.74 0.0091) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

![](us_cps2_files/figure-html/fit.models_0-6.png) 

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     3490                 60     626
##   Employed                  0    41456                902    2778
##   Not.in.Labor.Force        0     7012               3807     328
##   Retired                   0     2765                  2   10846
##   Unemployed                0     2706                233     134
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7273187             NA      0.7241611      0.7304591      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     1289                 21     226
##   Employed                  0    15206                365    1026
##   Not.in.Labor.Force        0     2581               1416     102
##   Retired                   0     1028                  1    3977
##   Unemployed                0      986                 97      47
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7261351      0.4752624      0.7209052      0.7313187      0.5850606 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
##          model_id model_method
## 1 Low.cor.X.rpart        rpart
##                                                                                                                                                feats
## 1 Age, Sex.fctr, Married.my.fctr, Region.fctr, .rnorm, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     57.322                10.976
##   max.Accuracy.fit max.AccuracyLower.fit max.AccuracyUpper.fit
## 1          0.73205             0.7241611             0.7304591
##   max.Kappa.fit max.Accuracy.OOB max.AccuracyLower.OOB
## 1     0.4874172        0.7261351             0.7209052
##   max.AccuracyUpper.OOB max.Kappa.OOB max.AccuracySD.fit max.KappaSD.fit
## 1             0.7313187     0.4752624        0.004250054     0.002519487
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0  75.172 186.411 111.239
## 11 fit.models          7          1 186.412      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 190.32  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    if (method %in% c("glm")) # for a "robust" glm model
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(NULL
                                    ,"A.nchrs.log"      # correlated to "S.*"                                                      
                                    ,"A.ndgts.log"      # correlated to "S.*"
                                    ,"A.nuppr.log"      # correlated to "S.*"
                                    ,"A.npnct01.log" # identical  to "S.npnct01.log"
                                    ,"A.npnct03.log" # correlated to "S.npnct03.log"
                                    ,"A.npnct04.log" # correlated to "S.npnct04.log"
                                    ,"A.npnct06.log" # identical  to "S.npnct06.log"
                                    ,"A.npnct07.log" # identical  to "S.npnct07.log"
                                    ,"A.npnct08.log" # correlated to "S.npnct08.log"
                                    ,"A.npnct11.log" # correlated to "S.*"
                                    ,"A.npnct12.log" # correlated to "S.*"
                                    ,"S.npnct14.log" # correlated to "A.*"
                                    ,"A.npnct15.log" # correlated to "S.npnct15.log"
                                    ,"A.npnct16.log" # correlated to "S.npnct16.log"
                                    ,"A.npnct19.log" # correlated to "S.*"
                                    ,"A.npnct20.log" # identical  to "S.npnct20.log"
                                    ,"A.npnct21.log" # correlated to "S.npnct21.log"
                                    ,"A.P.daily.clip.report" # identical  to "S.*"
                                    ,"S.P.daily.clip.report" # identical  to "H.*"
                                    ,"A.P.http" # correlated  to "A.npnct14.log"
                                    ,"A.P.fashion.week" # identical  to "S.*"
                                    ,"H.P.first.draft" # correlated  to "H.T.first"
                                    ,"A.P.first.draft" # identical  to "S.*"
                                    ,"A.P.metropolitan.diary.colon" # identical  to "S.*"
                                    ,"A.P.year.colon" # identical  to "S.P.year.colon"
                                                      ))
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##                label step_major step_minor     bgn     end elapsed
## 1   fit.models_1_bgn          1          0 190.320 190.336   0.016
## 2 fit.models_1_rpart          2          0 190.336      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: Age, Sex.fctr, Married.my.fctr, Region.fctr, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0193 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](us_cps2_files/figure-html/fit.models_1-1.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##           CP nsplit rel error
## 1 0.25205411      0 1.0000000
## 2 0.09075572      1 0.7479459
## 3 0.01930707      2 0.6571902
## 
## Variable importance
##                    Age Married.my.fctrWidowed 
##                     88                     12 
## 
## Node number 1: 77145 observations,    complexity param=0.2520541
##   predicted class=Employed            expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (62433 obs) right son=3 (14712 obs)
##   Primary splits:
##       Age                                             < 64.5 to the left,  improve=8861.839, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5  to the right, improve=2323.600, (0 missing)
##       Married.my.fctrWidowed                          < 0.5  to the left,  improve=2200.537, (0 missing)
##       PeopleInHousehold                               < 2.5  to the right, improve=1947.586, (0 missing)
##       Married.my.fctrNever Married                    < 0.5  to the right, improve=1420.837, (0 missing)
##   Surrogate splits:
##       Married.my.fctrWidowed < 0.5  to the left,  agree=0.843, adj=0.178, (0 split)
## 
## Node number 2: 62433 observations,    complexity param=0.09075572
##   predicted class=Employed            expected loss=0.3215447  P(node) =0.8092942
##     class counts:  3550 42358 10819  2767  2939
##    probabilities: 0.057 0.678 0.173 0.044 0.047 
##   left son=4 (57429 obs) right son=5 (5004 obs)
##   Primary splits:
##       Age                                             < 18.5 to the right, improve=3249.3410, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5  to the right, improve=1290.5850, (0 missing)
##       Married.my.fctrNever Married                    < 0.5  to the left,  improve=1036.8480, (0 missing)
##       Married.my.fctrMarried                          < 0.5  to the right, improve= 695.1223, (0 missing)
##       Industry.my.fctrTrade                           < 0.5  to the right, improve= 624.4119, (0 missing)
## 
## Node number 3: 14712 observations
##   predicted class=Retired             expected loss=0.2627787  P(node) =0.1907058
##     class counts:   626  2778   328 10846   134
##    probabilities: 0.043 0.189 0.022 0.737 0.009 
## 
## Node number 4: 57429 observations
##   predicted class=Employed            expected loss=0.2781347  P(node) =0.7444293
##     class counts:  3490 41456  7012  2765  2706
##    probabilities: 0.061 0.722 0.122 0.048 0.047 
## 
## Node number 5: 5004 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.2392086  P(node) =0.06486486
##     class counts:    60   902  3807     2   233
##    probabilities: 0.012 0.180 0.761 0.000 0.047 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04)  
##   2) Age< 64.5 62433 20075 Employed (0.057 0.68 0.17 0.044 0.047)  
##     4) Age>=18.5 57429 15973 Employed (0.061 0.72 0.12 0.048 0.047) *
##     5) Age< 18.5 5004  1197 Not.in.Labor.Force (0.012 0.18 0.76 0.0004 0.047) *
##   3) Age>=64.5 14712  3866 Retired (0.043 0.19 0.022 0.74 0.0091) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

![](us_cps2_files/figure-html/fit.models_1-2.png) 

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     3490                 60     626
##   Employed                  0    41456                902    2778
##   Not.in.Labor.Force        0     7012               3807     328
##   Retired                   0     2765                  2   10846
##   Unemployed                0     2706                233     134
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7273187             NA      0.7241611      0.7304591      0.5850800 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                  0     1289                 21     226
##   Employed                  0    15206                365    1026
##   Not.in.Labor.Force        0     2581               1416     102
##   Retired                   0     1028                  1    3977
##   Unemployed                0      986                 97      47
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          0
##   Retired                     0
##   Unemployed                  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7261351      0.4752624      0.7209052      0.7313187      0.5850606 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                        feats
## 1 Age, Sex.fctr, Married.my.fctr, Region.fctr, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     50.515                 9.901
##   max.Accuracy.fit max.AccuracyLower.fit max.AccuracyUpper.fit
## 1          0.73205             0.7241611             0.7304591
##   max.Kappa.fit max.Accuracy.OOB max.AccuracyLower.OOB
## 1     0.4874172        0.7261351             0.7209052
##   max.AccuracyUpper.OOB max.Kappa.OOB max.AccuracySD.fit max.KappaSD.fit
## 1             0.7313187     0.4752624        0.004250054     0.002519487
```

```r
# User specified
    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitobs_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df
#table(glb_allobs_df$myCategory, glb_allobs_df$H.P.readers.respond, glb_allobs_df[, glb_rsp_var], useNA="ifany")
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
# #     csm_mdl_id <- paste0(model_id, ".", method)
# #     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Low.cor.X.rpart                     Low.cor.X.rpart            rpart
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
##                                                                                                                                                                        feats
## MFO.myMFO_classfr                                                                                                                                                     .rnorm
## Random.myrandom_classfr                                                                                                                                               .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                   Industry.my.fctr, Age
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                              Industry.my.fctr, Age
## Max.cor.Y.rpart                                                                                                                                        Industry.my.fctr, Age
## Low.cor.X.rpart           Age, Sex.fctr, Married.my.fctr, Region.fctr, .rnorm, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
## All.X.no.rnorm.rpart              Age, Sex.fctr, Married.my.fctr, Region.fctr, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.471
## Random.myrandom_classfr                 0                      0.304
## Max.cor.Y.cv.0.rpart                    0                     13.775
## Max.cor.Y.cv.0.cp.0.rpart               0                      4.443
## Max.cor.Y.rpart                         3                     17.853
## Low.cor.X.rpart                         3                     57.322
## All.X.no.rnorm.rpart                    3                     50.515
##                           min.elapsedtime.final max.Accuracy.fit
## MFO.myMFO_classfr                         0.012        0.5850800
## Random.myrandom_classfr                   0.010        0.3982241
## Max.cor.Y.cv.0.rpart                      4.064        0.5850800
## Max.cor.Y.cv.0.cp.0.rpart                 3.816        0.8832199
## Max.cor.Y.rpart                           3.830        0.7320500
## Low.cor.X.rpart                          10.976        0.7320500
## All.X.no.rnorm.rpart                      9.901        0.7320500
##                           max.AccuracyLower.fit max.AccuracyUpper.fit
## MFO.myMFO_classfr                     0.5815936             0.5885601
## Random.myrandom_classfr               0.3947671             0.4016888
## Max.cor.Y.cv.0.rpart                  0.5815936             0.5885601
## Max.cor.Y.cv.0.cp.0.rpart             0.8809327             0.8854782
## Max.cor.Y.rpart                       0.7241611             0.7304591
## Low.cor.X.rpart                       0.7241611             0.7304591
## All.X.no.rnorm.rpart                  0.7241611             0.7304591
##                           max.Kappa.fit max.Accuracy.OOB
## MFO.myMFO_classfr                    NA        0.5850606
## Random.myrandom_classfr              NA        0.3982657
## Max.cor.Y.cv.0.rpart                 NA        0.5850606
## Max.cor.Y.cv.0.cp.0.rpart            NA        0.8828257
## Max.cor.Y.rpart               0.4874172        0.7261351
## Low.cor.X.rpart               0.4874172        0.7261351
## All.X.no.rnorm.rpart          0.4874172        0.7261351
##                           max.AccuracyLower.OOB max.AccuracyUpper.OOB
## MFO.myMFO_classfr                     0.5793009             0.5908029
## Random.myrandom_classfr               0.3925620             0.4039901
## Max.cor.Y.cv.0.rpart                  0.5793009             0.5908029
## Max.cor.Y.cv.0.cp.0.rpart             0.8790261             0.8865467
## Max.cor.Y.rpart                       0.7209052             0.7313187
## Low.cor.X.rpart                       0.7209052             0.7313187
## All.X.no.rnorm.rpart                  0.7209052             0.7313187
##                           max.Kappa.OOB max.AccuracySD.fit max.KappaSD.fit
## MFO.myMFO_classfr          0.0000000000                 NA              NA
## Random.myrandom_classfr   -0.0008038519                 NA              NA
## Max.cor.Y.cv.0.rpart       0.0000000000                 NA              NA
## Max.cor.Y.cv.0.cp.0.rpart  0.7967286427                 NA              NA
## Max.cor.Y.rpart            0.4752624489        0.004250054     0.002519487
## Low.cor.X.rpart            0.4752624489        0.004250054     0.002519487
## All.X.no.rnorm.rpart       0.4752624489        0.004250054     0.002519487
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 2 fit.models_1_rpart          2          0 190.336 244.878  54.542
## 3   fit.models_1_end          3          0 244.879      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 186.412 244.918  58.506
## 12 fit.models          7          2 244.919      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Low.cor.X.rpart                     Low.cor.X.rpart            rpart
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
##                                                                                                                                                                        feats
## MFO.myMFO_classfr                                                                                                                                                     .rnorm
## Random.myrandom_classfr                                                                                                                                               .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                   Industry.my.fctr, Age
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                              Industry.my.fctr, Age
## Max.cor.Y.rpart                                                                                                                                        Industry.my.fctr, Age
## Low.cor.X.rpart           Age, Sex.fctr, Married.my.fctr, Region.fctr, .rnorm, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
## All.X.no.rnorm.rpart              Age, Sex.fctr, Married.my.fctr, Region.fctr, Race.fctr, Citizenship.fctr, Hispanic, PeopleInHousehold, Education.my.fctr, Industry.my.fctr
##                           max.nTuningRuns max.Accuracy.fit max.Kappa.fit
## MFO.myMFO_classfr                       0        0.5850800            NA
## Random.myrandom_classfr                 0        0.3982241            NA
## Max.cor.Y.cv.0.rpart                    0        0.5850800            NA
## Max.cor.Y.cv.0.cp.0.rpart               0        0.8832199            NA
## Max.cor.Y.rpart                         3        0.7320500     0.4874172
## Low.cor.X.rpart                         3        0.7320500     0.4874172
## All.X.no.rnorm.rpart                    3        0.7320500     0.4874172
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5850606  0.0000000000
## Random.myrandom_classfr          0.3982657 -0.0008038519
## Max.cor.Y.cv.0.rpart             0.5850606  0.0000000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8828257  0.7967286427
## Max.cor.Y.rpart                  0.7261351  0.4752624489
## Low.cor.X.rpart                  0.7261351  0.4752624489
## All.X.no.rnorm.rpart             0.7261351  0.4752624489
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         2.12314225           83.33333333
## Random.myrandom_classfr                   3.28947368          100.00000000
## Max.cor.Y.cv.0.rpart                      0.07259528            0.24606299
## Max.cor.Y.cv.0.cp.0.rpart                 0.22507315            0.26205451
## Max.cor.Y.rpart                           0.05601300            0.26109661
## Low.cor.X.rpart                           0.01744531            0.09110787
## All.X.no.rnorm.rpart                      0.01979610            0.10099990
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have 7.
## Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 11 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (geom_text).
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have 7.
## Consider specifying shapes manually. if you must have them.
```

![](us_cps2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

![](us_cps2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                    model_id max.Accuracy.OOB max.Kappa.OOB
## 4 Max.cor.Y.cv.0.cp.0.rpart        0.8828257  0.7967286427
## 5           Max.cor.Y.rpart        0.7261351  0.4752624489
## 6           Low.cor.X.rpart        0.7261351  0.4752624489
## 7      All.X.no.rnorm.rpart        0.7261351  0.4752624489
## 1         MFO.myMFO_classfr        0.5850606  0.0000000000
## 3      Max.cor.Y.cv.0.rpart        0.5850606  0.0000000000
## 2   Random.myrandom_classfr        0.3982657 -0.0008038519
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have 7.
## Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (geom_point).
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have 7.
## Consider specifying shapes manually. if you must have them.
```

![](us_cps2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.Kappa.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Max.cor.Y.cv.0.cp.0.rpart"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](us_cps2_files/figure-html/fit.models_2-4.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 77145 
## 
##              CP nsplit rel error
## 1  2.520541e-01      0 1.0000000
## 2  9.075572e-02      1 0.7479459
## 3  1.930707e-02      2 0.6571902
## 4  1.190290e-02      3 0.6378831
## 5  1.134056e-02      4 0.6259802
## 6  1.065325e-02      5 0.6146396
## 7  9.256772e-03      6 0.6039864
## 8  7.591615e-03     19 0.4122903
## 9  6.310725e-03     20 0.4046987
## 10 6.248243e-03     22 0.3920772
## 11 6.217001e-03     23 0.3858290
## 12 6.092037e-03     24 0.3796120
## 13 6.029554e-03     25 0.3735199
## 14 4.717423e-03     26 0.3674904
## 15 4.592458e-03     27 0.3627730
## 16 4.092599e-03     28 0.3581805
## 17 3.842669e-03     29 0.3540879
## 18 3.811428e-03     30 0.3502452
## 19 3.249086e-03     31 0.3464338
## 20 3.238672e-03     32 0.3431847
## 21 2.936674e-03     41 0.2993221
## 22 2.530538e-03     42 0.2963854
## 23 1.749508e-03     43 0.2938549
## 24 1.687026e-03     44 0.2921053
## 25 1.499578e-03     45 0.2904183
## 26 1.030960e-03     46 0.2889187
## 27 9.684776e-04     47 0.2878878
## 28 9.059952e-04     49 0.2859508
## 29 7.497891e-04     50 0.2850448
## 30 6.248243e-04     51 0.2842950
## 31 4.998594e-04     52 0.2836702
## 32 4.373770e-04     53 0.2831704
## 33 3.748946e-04     54 0.2827330
## 34 2.030679e-04     55 0.2823581
## 35 1.718267e-04     57 0.2819520
## 36 6.248243e-05     59 0.2816083
## 37 3.124121e-05     60 0.2815458
## 38 0.000000e+00     63 0.2814521
## 
## Variable importance
##                                                         Age 
##                                                          43 
##                              Industry.my.fctrOther services 
##                                                           7 
##                       Industry.my.fctrPublic administration 
##                                                           5 
##             Industry.my.fctrEducational and health services 
##                                                           5 
##                     Industry.my.fctrLeisure and hospitality 
##                                                           5 
##                Industry.my.fctrTransportation and utilities 
##                                                           5 
##                                       Industry.my.fctrTrade 
##                                                           5 
##                                Industry.my.fctrConstruction 
##                                                           4 
##                                   Industry.my.fctrFinancial 
##                                                           4 
##          Industry.my.fctrProfessional and business services 
##                                                           4 
##                               Industry.my.fctrManufacturing 
##                                                           4 
## Industry.my.fctrAgriculture, forestry, fishing, and hunting 
##                                                           4 
##                                 Industry.my.fctrInformation 
##                                                           3 
##                                      Industry.my.fctrMining 
##                                                           2 
## 
## Node number 1: 77145 observations,    complexity param=0.2520541
##   predicted class=Employed            expected loss=0.41492  P(node) =1
##     class counts:  4176 45136 11147 13613  3073
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (62433 obs) right son=3 (14712 obs)
##   Primary splits:
##       Age                                                < 64.5 to the left,  improve=8861.8390, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=2323.6000, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=1186.7910, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 983.1942, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 924.0788, (0 missing)
## 
## Node number 2: 62433 observations,    complexity param=0.09075572
##   predicted class=Employed            expected loss=0.3215447  P(node) =0.8092942
##     class counts:  3550 42358 10819  2767  2939
##    probabilities: 0.057 0.678 0.173 0.044 0.047 
##   left son=4 (57429 obs) right son=5 (5004 obs)
##   Primary splits:
##       Age                                                < 18.5 to the right, improve=3249.3410, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1290.5850, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 624.4119, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 515.9783, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 505.1749, (0 missing)
## 
## Node number 3: 14712 observations,    complexity param=0.01930707
##   predicted class=Retired             expected loss=0.2627787  P(node) =0.1907058
##     class counts:   626  2778   328 10846   134
##    probabilities: 0.043 0.189 0.022 0.737 0.009 
##   left son=6 (696 obs) right son=7 (14016 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=756.4649, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=436.5917, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=408.2808, (0 missing)
##       Age                                                < 70.5 to the left,  improve=364.4106, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=241.2709, (0 missing)
## 
## Node number 4: 57429 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.2781347  P(node) =0.7444293
##     class counts:  3490 41456  7012  2765  2706
##    probabilities: 0.061 0.722 0.122 0.048 0.047 
##   left son=8 (10137 obs) right son=9 (47292 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=880.1351, (0 missing)
##       Age                                                < 59.5 to the left,  improve=604.0624, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=395.1759, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=341.2702, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=334.6625, (0 missing)
## 
## Node number 5: 5004 observations,    complexity param=0.0119029
##   predicted class=Not.in.Labor.Force  expected loss=0.2392086  P(node) =0.06486486
##     class counts:    60   902  3807     2   233
##    probabilities: 0.012 0.180 0.761 0.000 0.047 
##   left son=10 (487 obs) right son=11 (4517 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality         < 0.5  to the right, improve=501.58970, (0 missing)
##       Industry.my.fctrTrade                           < 0.5  to the right, improve=243.57050, (0 missing)
##       Age                                             < 16.5 to the right, improve=107.27410, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5  to the right, improve= 66.39350, (0 missing)
##       Industry.my.fctrOther services                  < 0.5  to the right, improve= 38.80304, (0 missing)
## 
## Node number 6: 696 observations
##   predicted class=Employed            expected loss=0.07758621  P(node) =0.009021972
##     class counts:     0   642     0    24    30
##    probabilities: 0.000 0.922 0.000 0.034 0.043 
## 
## Node number 7: 14016 observations,    complexity param=0.01134056
##   predicted class=Retired             expected loss=0.2278824  P(node) =0.1816838
##     class counts:   626  2136   328 10822   104
##    probabilities: 0.045 0.152 0.023 0.772 0.007 
##   left son=14 (402 obs) right son=15 (13614 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=481.2436, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=450.4300, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=265.4408, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=262.8831, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=260.1050, (0 missing)
## 
## Node number 8: 10137 observations
##   predicted class=Employed            expected loss=0.05011345  P(node) =0.1314019
##     class counts:     9  9629    51    13   435
##    probabilities: 0.001 0.950 0.005 0.001 0.043 
## 
## Node number 9: 47292 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3270109  P(node) =0.6130274
##     class counts:  3481 31827  6961  2752  2271
##    probabilities: 0.074 0.673 0.147 0.058 0.048 
##   left son=18 (42249 obs) right son=19 (5043 obs)
##   Primary splits:
##       Age                                                < 59.5 to the left,  improve=682.6394, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=615.9417, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=526.5017, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=511.4015, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=354.9892, (0 missing)
## 
## Node number 10: 487 observations
##   predicted class=Employed            expected loss=0.1457906  P(node) =0.006312788
##     class counts:     0   416    35     0    36
##    probabilities: 0.000 0.854 0.072 0.000 0.074 
## 
## Node number 11: 4517 observations,    complexity param=0.006029554
##   predicted class=Not.in.Labor.Force  expected loss=0.1649325  P(node) =0.05855208
##     class counts:    60   486  3772     2   197
##    probabilities: 0.013 0.108 0.835 0.000 0.044 
##   left son=22 (232 obs) right son=23 (4285 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=298.35260, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve= 84.49187, (0 missing)
##       Age                                                < 16.5 to the right, improve= 55.14438, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve= 49.12110, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 48.50088, (0 missing)
## 
## Node number 14: 402 observations
##   predicted class=Employed            expected loss=0.06716418  P(node) =0.005210966
##     class counts:     2   375     0    12    13
##    probabilities: 0.005 0.933 0.000 0.030 0.032 
## 
## Node number 15: 13614 observations,    complexity param=0.01065325
##   predicted class=Retired             expected loss=0.2059644  P(node) =0.1764729
##     class counts:   624  1761   328 10810    91
##    probabilities: 0.046 0.129 0.024 0.794 0.007 
##   left son=30 (384 obs) right son=31 (13230 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=478.0754, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=281.2735, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=278.5598, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=275.8898, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=270.8294, (0 missing)
## 
## Node number 18: 42249 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3028001  P(node) =0.547657
##     class counts:  2844 29456  6676  1140  2133
##    probabilities: 0.067 0.697 0.158 0.027 0.050 
##   left son=36 (5469 obs) right son=37 (36780 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=491.7069, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=427.5658, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=419.4708, (0 missing)
##       Age                                                < 23.5 to the right, improve=324.6411, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=289.1954, (0 missing)
## 
## Node number 19: 5043 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.5298433  P(node) =0.06537041
##     class counts:   637  2371   285  1612   138
##    probabilities: 0.126 0.470 0.057 0.320 0.027 
##   left son=38 (451 obs) right son=39 (4592 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=169.22760, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=123.99940, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=118.31870, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve= 90.86328, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve= 76.67119, (0 missing)
## 
## Node number 22: 232 observations
##   predicted class=Employed            expected loss=0.1336207  P(node) =0.003007324
##     class counts:     0   201     8     0    23
##    probabilities: 0.000 0.866 0.034 0.000 0.099 
## 
## Node number 23: 4285 observations,    complexity param=0.001749508
##   predicted class=Not.in.Labor.Force  expected loss=0.1215869  P(node) =0.05554475
##     class counts:    60   285  3764     2   174
##    probabilities: 0.014 0.067 0.878 0.000 0.041 
##   left son=46 (97 obs) right son=47 (4188 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services             < 0.5  to the right, improve=95.90269, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=55.61024, (0 missing)
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=55.21431, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=53.14430, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=39.60641, (0 missing)
## 
## Node number 30: 384 observations
##   predicted class=Employed            expected loss=0.0859375  P(node) =0.00497764
##     class counts:     1   351     2    10    20
##    probabilities: 0.003 0.914 0.005 0.026 0.052 
## 
## Node number 31: 13230 observations,    complexity param=0.006310725
##   predicted class=Retired             expected loss=0.1836735  P(node) =0.1714952
##     class counts:   623  1410   326 10800    71
##    probabilities: 0.047 0.107 0.025 0.816 0.005 
##   left son=62 (219 obs) right son=63 (13011 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=297.6126, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=294.7483, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=292.1900, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=286.7155, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=222.5232, (0 missing)
## 
## Node number 36: 5469 observations
##   predicted class=Employed            expected loss=0.07899067  P(node) =0.07089248
##     class counts:     7  5037    40     1   384
##    probabilities: 0.001 0.921 0.007 0.000 0.070 
## 
## Node number 37: 36780 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3360794  P(node) =0.4767645
##     class counts:  2837 24419  6636  1139  1749
##    probabilities: 0.077 0.664 0.180 0.031 0.048 
##   left son=74 (4668 obs) right son=75 (32112 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=571.8024, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=556.1714, (0 missing)
##       Age                                                < 23.5 to the right, improve=394.3466, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=392.5422, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=371.2783, (0 missing)
## 
## Node number 38: 451 observations
##   predicted class=Employed            expected loss=0.05543237  P(node) =0.005846134
##     class counts:     1   426     1     3    20
##    probabilities: 0.002 0.945 0.002 0.007 0.044 
## 
## Node number 39: 4592 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.5764373  P(node) =0.05952427
##     class counts:   636  1945   284  1609   118
##    probabilities: 0.139 0.424 0.062 0.350 0.026 
##   left son=78 (354 obs) right son=79 (4238 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=151.30200, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=145.26730, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=109.69160, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve= 93.33870, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve= 76.52206, (0 missing)
## 
## Node number 46: 97 observations
##   predicted class=Employed            expected loss=0.2474227  P(node) =0.001257372
##     class counts:     0    73    17     0     7
##    probabilities: 0.000 0.753 0.175 0.000 0.072 
## 
## Node number 47: 4188 observations,    complexity param=0.00103096
##   predicted class=Not.in.Labor.Force  expected loss=0.1053009  P(node) =0.05428738
##     class counts:    60   212  3747     2   167
##    probabilities: 0.014 0.051 0.895 0.000 0.040 
##   left son=94 (55 obs) right son=95 (4133 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=58.19129, (0 missing)
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=57.88160, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=55.33508, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=41.20103, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=37.29178, (0 missing)
## 
## Node number 62: 219 observations
##   predicted class=Employed            expected loss=0.05479452  P(node) =0.00283881
##     class counts:     0   207     0     5     7
##    probabilities: 0.000 0.945 0.000 0.023 0.032 
## 
## Node number 63: 13011 observations,    complexity param=0.006248243
##   predicted class=Retired             expected loss=0.1703174  P(node) =0.1686564
##     class counts:   623  1203   326 10795    64
##    probabilities: 0.048 0.092 0.025 0.830 0.005 
##   left son=126 (217 obs) right son=127 (12794 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=304.8379, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=302.3581, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=296.6215, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=229.9711, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=198.7685, (0 missing)
## 
## Node number 74: 4668 observations
##   predicted class=Employed            expected loss=0.07219366  P(node) =0.06050943
##     class counts:     3  4331    31     0   303
##    probabilities: 0.001 0.928 0.007 0.000 0.065 
## 
## Node number 75: 32112 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.3744395  P(node) =0.4162551
##     class counts:  2834 20088  6605  1139  1446
##    probabilities: 0.088 0.626 0.206 0.035 0.045 
##   left son=150 (4364 obs) right son=151 (27748 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing           < 0.5  to the right, improve=739.5833, (0 missing)
##       Industry.my.fctrLeisure and hospitality < 0.5  to the right, improve=532.0889, (0 missing)
##       Industry.my.fctrFinancial               < 0.5  to the right, improve=481.7038, (0 missing)
##       Industry.my.fctrConstruction            < 0.5  to the right, improve=415.8067, (0 missing)
##       Age                                     < 23.5 to the right, improve=364.5501, (0 missing)
## 
## Node number 78: 354 observations
##   predicted class=Employed            expected loss=0.07344633  P(node) =0.004588761
##     class counts:     0   328     0     3    23
##    probabilities: 0.000 0.927 0.000 0.008 0.065 
## 
## Node number 79: 4238 observations,    complexity param=0.003238672
##   predicted class=Employed            expected loss=0.6184521  P(node) =0.05493551
##     class counts:   636  1617   284  1606    95
##    probabilities: 0.150 0.382 0.067 0.379 0.022 
##   left son=158 (361 obs) right son=159 (3877 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=172.59900, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=128.53290, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=110.07090, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve= 90.67191, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve= 88.92407, (0 missing)
## 
## Node number 94: 55 observations
##   predicted class=Employed            expected loss=0.2363636  P(node) =0.0007129432
##     class counts:     0    42     9     0     4
##    probabilities: 0.000 0.764 0.164 0.000 0.073 
## 
## Node number 95: 4133 observations,    complexity param=0.0009684776
##   predicted class=Not.in.Labor.Force  expected loss=0.09557222  P(node) =0.05357444
##     class counts:    60   170  3738     2   163
##    probabilities: 0.015 0.041 0.904 0.000 0.039 
##   left son=190 (61 obs) right son=191 (4072 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=59.50594, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=56.66486, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=42.16854, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=38.28705, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=27.80150, (0 missing)
## 
## Node number 126: 217 observations
##   predicted class=Employed            expected loss=0.06912442  P(node) =0.002812885
##     class counts:     0   202     1     2    12
##    probabilities: 0.000 0.931 0.005 0.009 0.055 
## 
## Node number 127: 12794 observations,    complexity param=0.006217001
##   predicted class=Retired             expected loss=0.1564014  P(node) =0.1658435
##     class counts:   623  1001   325 10793    52
##    probabilities: 0.049 0.078 0.025 0.844 0.004 
##   left son=254 (222 obs) right son=255 (12572 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=312.9565, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=306.9477, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=237.7246, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=205.6450, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=192.0610, (0 missing)
## 
## Node number 150: 4364 observations
##   predicted class=Employed            expected loss=0.06393217  P(node) =0.0565688
##     class counts:     2  4085    15     3   259
##    probabilities: 0.000 0.936 0.003 0.001 0.059 
## 
## Node number 151: 27748 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.4232737  P(node) =0.3596863
##     class counts:  2832 16003  6590  1136  1187
##    probabilities: 0.102 0.577 0.237 0.041 0.043 
##   left son=302 (3747 obs) right son=303 (24001 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=743.1407, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=644.6047, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=571.9225, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=471.3980, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=458.7793, (0 missing)
## 
## Node number 158: 361 observations
##   predicted class=Employed            expected loss=0.09418283  P(node) =0.0046795
##     class counts:     1   327     0     5    28
##    probabilities: 0.003 0.906 0.000 0.014 0.078 
## 
## Node number 159: 3877 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5870518  P(node) =0.05025601
##     class counts:   635  1290   284  1601    67
##    probabilities: 0.164 0.333 0.073 0.413 0.017 
##   left son=318 (242 obs) right son=319 (3635 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=152.6798, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=131.5716, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=108.9098, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=106.1004, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=102.9366, (0 missing)
## 
## Node number 190: 61 observations
##   predicted class=Employed            expected loss=0.3606557  P(node) =0.0007907188
##     class counts:     0    39     9     0    13
##    probabilities: 0.000 0.639 0.148 0.000 0.213 
## 
## Node number 191: 4072 observations,    complexity param=0.0009684776
##   predicted class=Not.in.Labor.Force  expected loss=0.08423379  P(node) =0.05278372
##     class counts:    60   131  3729     2   150
##    probabilities: 0.015 0.032 0.916 0.000 0.037 
##   left son=382 (42 obs) right son=383 (4030 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=58.11483, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=43.22076, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=39.39532, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=28.43424, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=24.96236, (0 missing)
## 
## Node number 254: 222 observations
##   predicted class=Employed            expected loss=0.07207207  P(node) =0.002877698
##     class counts:     0   206     1     7     8
##    probabilities: 0.000 0.928 0.005 0.032 0.036 
## 
## Node number 255: 12572 observations,    complexity param=0.006092037
##   predicted class=Retired             expected loss=0.1420617  P(node) =0.1629658
##     class counts:   623   795   324 10786    44
##    probabilities: 0.050 0.063 0.026 0.858 0.003 
##   left son=510 (215 obs) right son=511 (12357 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=317.9065, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=245.9525, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=212.9354, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=198.9882, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=198.2375, (0 missing)
## 
## Node number 302: 3747 observations
##   predicted class=Employed            expected loss=0.09634374  P(node) =0.04857087
##     class counts:     2  3386    43     3   313
##    probabilities: 0.001 0.904 0.011 0.001 0.084 
## 
## Node number 303: 24001 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.4743136  P(node) =0.3111154
##     class counts:  2830 12617  6547  1133   874
##    probabilities: 0.118 0.526 0.273 0.047 0.036 
##   left son=606 (2696 obs) right son=607 (21305 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=849.2295, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=771.8555, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=616.0829, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=606.5147, (0 missing)
##       Age                                          < 26.5 to the right, improve=546.5604, (0 missing)
## 
## Node number 318: 242 observations
##   predicted class=Employed            expected loss=0.04132231  P(node) =0.00313695
##     class counts:     0   232     0     1     9
##    probabilities: 0.000 0.959 0.000 0.004 0.037 
## 
## Node number 319: 3635 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5598349  P(node) =0.04711906
##     class counts:   635  1058   284  1600    58
##    probabilities: 0.175 0.291 0.078 0.440 0.016 
##   left son=638 (226 obs) right son=639 (3409 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=151.0187, (0 missing)
##       Industry.my.fctrLeisure and hospitality      < 0.5  to the right, improve=125.3594, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=121.5617, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=117.7939, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve=108.4572, (0 missing)
## 
## Node number 382: 42 observations
##   predicted class=Employed            expected loss=0.1904762  P(node) =0.0005444293
##     class counts:     0    34     2     0     6
##    probabilities: 0.000 0.810 0.048 0.000 0.143 
## 
## Node number 383: 4030 observations,    complexity param=0.0007497891
##   predicted class=Not.in.Labor.Force  expected loss=0.0751861  P(node) =0.05223929
##     class counts:    60    97  3727     2   144
##    probabilities: 0.015 0.024 0.925 0.000 0.036 
##   left son=766 (30 obs) right son=767 (4000 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=44.11017, (0 missing)
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=40.32177, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=28.97328, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=25.41738, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=21.77548, (0 missing)
## 
## Node number 510: 215 observations
##   predicted class=Employed            expected loss=0.06976744  P(node) =0.00278696
##     class counts:     0   200     0     5    10
##    probabilities: 0.000 0.930 0.000 0.023 0.047 
## 
## Node number 511: 12357 observations,    complexity param=0.004717423
##   predicted class=Retired             expected loss=0.127539  P(node) =0.1601789
##     class counts:   623   595   324 10781    34
##    probabilities: 0.050 0.048 0.026 0.872 0.003 
##   left son=1022 (158 obs) right son=1023 (12199 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=254.39830, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=220.42630, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=206.10560, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=205.04780, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 80.04601, (0 missing)
## 
## Node number 606: 2696 observations
##   predicted class=Employed            expected loss=0.03783383  P(node) =0.03494718
##     class counts:     0  2594     7     1    94
##    probabilities: 0.000 0.962 0.003 0.000 0.035 
## 
## Node number 607: 21305 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.5295471  P(node) =0.2761683
##     class counts:  2830 10023  6540  1132   780
##    probabilities: 0.133 0.470 0.307 0.053 0.037 
##   left son=1214 (2819 obs) right son=1215 (18486 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=1012.4120, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve= 789.2268, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve= 783.2167, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve= 680.7946, (0 missing)
##       Age                                          < 26.5 to the right, improve= 519.2679, (0 missing)
## 
## Node number 638: 226 observations
##   predicted class=Employed            expected loss=0.0619469  P(node) =0.002929548
##     class counts:     0   212     0     6     8
##    probabilities: 0.000 0.938 0.000 0.027 0.035 
## 
## Node number 639: 3409 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5324142  P(node) =0.04418951
##     class counts:   635   846   284  1594    50
##    probabilities: 0.186 0.248 0.083 0.468 0.015 
##   left son=1278 (199 obs) right son=1279 (3210 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=143.50720, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=138.57380, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=134.13540, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=123.53620, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 76.56807, (0 missing)
## 
## Node number 766: 30 observations
##   predicted class=Employed            expected loss=0.1666667  P(node) =0.0003888781
##     class counts:     0    25     1     0     4
##    probabilities: 0.000 0.833 0.033 0.000 0.133 
## 
## Node number 767: 4000 observations,    complexity param=0.0006248243
##   predicted class=Not.in.Labor.Force  expected loss=0.0685  P(node) =0.05185041
##     class counts:    60    72  3726     2   140
##    probabilities: 0.015 0.018 0.931 0.001 0.035 
##   left son=1534 (36 obs) right son=1535 (3964 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                < 0.5  to the right, improve=41.016160, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=29.377430, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=25.758690, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=22.067800, (0 missing)
##       Age                                          < 17.5 to the left,  improve= 9.052118, (0 missing)
## 
## Node number 1022: 158 observations
##   predicted class=Employed            expected loss=0.03164557  P(node) =0.002048091
##     class counts:     0   153     0     2     3
##    probabilities: 0.000 0.968 0.000 0.013 0.019 
## 
## Node number 1023: 12199 observations,    complexity param=0.004092599
##   predicted class=Retired             expected loss=0.116403  P(node) =0.1581308
##     class counts:   623   442   324 10779    31
##    probabilities: 0.051 0.036 0.027 0.884 0.003 
##   left son=2046 (144 obs) right son=2047 (12055 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=226.33950, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=211.73010, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=210.42740, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 82.21373, (0 missing)
##       Age                                          < 69.5 to the left,  improve= 54.47671, (0 missing)
## 
## Node number 1214: 2819 observations
##   predicted class=Employed            expected loss=0.08300816  P(node) =0.03654158
##     class counts:     2  2585    17     3   212
##    probabilities: 0.001 0.917 0.006 0.001 0.075 
## 
## Node number 1215: 18486 observations,    complexity param=0.009256772
##   predicted class=Employed            expected loss=0.5976415  P(node) =0.2396267
##     class counts:  2828  7438  6523  1129   568
##    probabilities: 0.153 0.402 0.353 0.061 0.031 
##   left son=2430 (2078 obs) right son=2431 (16408 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=1045.4260, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=1044.0940, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve= 916.3636, (0 missing)
##       Age                                          < 26.5 to the right, improve= 504.6234, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 382.4998, (0 missing)
## 
## Node number 1278: 199 observations
##   predicted class=Employed            expected loss=0.09547739  P(node) =0.002579558
##     class counts:     1   180     0     3    15
##    probabilities: 0.005 0.905 0.000 0.015 0.075 
## 
## Node number 1279: 3210 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.5043614  P(node) =0.04160996
##     class counts:   634   666   284  1591    35
##    probabilities: 0.198 0.207 0.088 0.496 0.011 
##   left son=2558 (182 obs) right son=2559 (3028 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=156.29700, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=151.07500, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=139.20960, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 86.10358, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 47.69273, (0 missing)
## 
## Node number 1534: 36 observations
##   predicted class=Employed            expected loss=0.3333333  P(node) =0.0004666537
##     class counts:     0    24     4     0     8
##    probabilities: 0.000 0.667 0.111 0.000 0.222 
## 
## Node number 1535: 3964 observations,    complexity param=0.0004998594
##   predicted class=Not.in.Labor.Force  expected loss=0.06104945  P(node) =0.05138376
##     class counts:    60    48  3722     2   132
##    probabilities: 0.015 0.012 0.939 0.001 0.033 
##   left son=3070 (17 obs) right son=3071 (3947 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=29.804300, (0 missing)
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=26.116770, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=22.374470, (0 missing)
##       Age                                          < 17.5 to the left,  improve= 6.419004, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve= 5.571355, (0 missing)
## 
## Node number 2046: 144 observations
##   predicted class=Employed            expected loss=0.09027778  P(node) =0.001866615
##     class counts:     0   131     1     0    12
##    probabilities: 0.000 0.910 0.007 0.000 0.083 
## 
## Node number 2047: 12055 observations,    complexity param=0.003842669
##   predicted class=Retired             expected loss=0.1058482  P(node) =0.1562642
##     class counts:   623   311   323 10779    19
##    probabilities: 0.052 0.026 0.027 0.894 0.002 
##   left son=4094 (139 obs) right son=4095 (11916 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=216.92580, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=215.39200, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 84.21834, (0 missing)
##       Age                                          < 69.5 to the left,  improve= 42.58446, (0 missing)
##       Industry.my.fctrMining                       < 0.5  to the right, improve= 21.11926, (0 missing)
## 
## Node number 2430: 2078 observations
##   predicted class=Employed            expected loss=0.05293551  P(node) =0.02693629
##     class counts:     0  1968     6     2   102
##    probabilities: 0.000 0.947 0.003 0.001 0.049 
## 
## Node number 2431: 16408 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.6028157  P(node) =0.2126904
##     class counts:  2828  5470  6517  1127   466
##    probabilities: 0.172 0.333 0.397 0.069 0.028 
##   left son=4862 (1984 obs) right son=4863 (14424 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=1338.0430, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=1188.9860, (0 missing)
##       Age                                                         < 41.5 to the right, improve= 523.3124, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 491.6569, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 358.8742, (0 missing)
## 
## Node number 2558: 182 observations
##   predicted class=Employed            expected loss=0.07142857  P(node) =0.002359194
##     class counts:     0   169     0     1    12
##    probabilities: 0.000 0.929 0.000 0.005 0.066 
## 
## Node number 2559: 3028 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.4749009  P(node) =0.03925076
##     class counts:   634   497   284  1590    23
##    probabilities: 0.209 0.164 0.094 0.525 0.008 
##   left son=5118 (174 obs) right son=5119 (2854 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=170.19160, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=156.88020, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 96.83817, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 53.76955, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 30.07375, (0 missing)
## 
## Node number 3070: 17 observations
##   predicted class=Employed            expected loss=0.05882353  P(node) =0.0002203642
##     class counts:     0    16     0     0     1
##    probabilities: 0.000 0.941 0.000 0.000 0.059 
## 
## Node number 3071: 3947 observations,    complexity param=0.000437377
##   predicted class=Not.in.Labor.Force  expected loss=0.05700532  P(node) =0.05116339
##     class counts:    60    32  3722     2   131
##    probabilities: 0.015 0.008 0.943 0.001 0.033 
##   left son=6142 (14 obs) right son=6143 (3933 obs)
##   Primary splits:
##       Industry.my.fctrFinancial             < 0.5  to the right, improve=26.335330, (0 missing)
##       Industry.my.fctrInformation           < 0.5  to the right, improve=22.561670, (0 missing)
##       Industry.my.fctrPublic administration < 0.5  to the right, improve= 5.647505, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 4.420919, (0 missing)
## 
## Node number 4094: 139 observations
##   predicted class=Employed            expected loss=0.08633094  P(node) =0.001801802
##     class counts:     0   127     1     4     7
##    probabilities: 0.000 0.914 0.007 0.029 0.050 
## 
## Node number 4095: 11916 observations,    complexity param=0.003811428
##   predicted class=Retired             expected loss=0.09575361  P(node) =0.1544624
##     class counts:   623   184   322 10775    12
##    probabilities: 0.052 0.015 0.027 0.904 0.001 
##   left son=8190 (128 obs) right son=8191 (11788 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=220.28360, (0 missing)
##       Industry.my.fctrInformation           < 0.5  to the right, improve= 86.19226, (0 missing)
##       Age                                   < 69.5 to the left,  improve= 32.25519, (0 missing)
##       Industry.my.fctrMining                < 0.5  to the right, improve= 21.65488, (0 missing)
## 
## Node number 4862: 1984 observations
##   predicted class=Employed            expected loss=0.03175403  P(node) =0.0257178
##     class counts:     1  1921     5     3    54
##    probabilities: 0.001 0.968 0.003 0.002 0.027 
## 
## Node number 4863: 14424 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.5485302  P(node) =0.1869726
##     class counts:  2827  3549  6512  1124   412
##    probabilities: 0.196 0.246 0.451 0.078 0.029 
##   left son=9726 (1952 obs) right son=9727 (12472 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=1586.3880, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 648.4297, (0 missing)
##       Age                                                         < 41.5 to the right, improve= 548.0990, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 472.6345, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 283.5885, (0 missing)
## 
## Node number 5118: 174 observations
##   predicted class=Employed            expected loss=0.04597701  P(node) =0.002255493
##     class counts:     0   166     1     5     2
##    probabilities: 0.000 0.954 0.006 0.029 0.011 
## 
## Node number 5119: 2854 observations,    complexity param=0.003238672
##   predicted class=Retired             expected loss=0.4446391  P(node) =0.03699527
##     class counts:   634   331   283  1585    21
##    probabilities: 0.222 0.116 0.099 0.555 0.007 
##   left son=10238 (162 obs) right son=10239 (2692 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=177.24140, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=109.19970, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 60.72953, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 33.66763, (0 missing)
##       Age                                                         < 61.5 to the left,  improve= 24.28196, (0 missing)
## 
## Node number 6142: 14 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0001814764
##     class counts:     0    14     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 6143: 3933 observations,    complexity param=0.0003748946
##   predicted class=Not.in.Labor.Force  expected loss=0.05364861  P(node) =0.05098192
##     class counts:    60    18  3722     2   131
##    probabilities: 0.015 0.005 0.946 0.001 0.033 
##   left son=12286 (12 obs) right son=12287 (3921 obs)
##   Primary splits:
##       Industry.my.fctrInformation           < 0.5  to the right, improve=22.722820, (0 missing)
##       Industry.my.fctrPublic administration < 0.5  to the right, improve= 5.713301, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 3.677531, (0 missing)
## 
## Node number 8190: 128 observations
##   predicted class=Employed            expected loss=0.046875  P(node) =0.001659213
##     class counts:     0   122     0     0     6
##    probabilities: 0.000 0.953 0.000 0.000 0.047 
## 
## Node number 8191: 11788 observations,    complexity param=0.001499578
##   predicted class=Retired             expected loss=0.08593485  P(node) =0.1528032
##     class counts:   623    62   322 10775     6
##    probabilities: 0.053 0.005 0.027 0.914 0.001 
##   left son=16382 (54 obs) right son=16383 (11734 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=88.14437, (0 missing)
##       Age                         < 66.5 to the left,  improve=24.62345, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve=22.18490, (0 missing)
## 
## Node number 9726: 1952 observations
##   predicted class=Employed            expected loss=0.07633197  P(node) =0.025303
##     class counts:     2  1803    13     0   134
##    probabilities: 0.001 0.924 0.007 0.000 0.069 
## 
## Node number 9727: 12472 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.4789128  P(node) =0.1616696
##     class counts:  2825  1746  6499  1124   278
##    probabilities: 0.227 0.140 0.521 0.090 0.022 
##   left son=19454 (864 obs) right son=19455 (11608 obs)
##   Primary splits:
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=875.54450, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=636.69090, (0 missing)
##       Age                                                         < 44.5 to the right, improve=628.14260, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=376.68880, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 25.43837, (0 missing)
## 
## Node number 10238: 162 observations
##   predicted class=Employed            expected loss=0.0617284  P(node) =0.002099942
##     class counts:     0   152     0     2     8
##    probabilities: 0.000 0.938 0.000 0.012 0.049 
## 
## Node number 10239: 2692 observations,    complexity param=0.002936674
##   predicted class=Retired             expected loss=0.4119614  P(node) =0.03489533
##     class counts:   634   179   283  1583    13
##    probabilities: 0.236 0.066 0.105 0.588 0.005 
##   left son=20478 (101 obs) right son=20479 (2591 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=122.93260, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 68.49246, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 37.63462, (0 missing)
##       Age                                                         < 61.5 to the left,  improve= 23.23422, (0 missing)
## 
## Node number 12286: 12 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0001555512
##     class counts:     0    12     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 12287: 3921 observations,    complexity param=6.248243e-05
##   predicted class=Not.in.Labor.Force  expected loss=0.05075236  P(node) =0.05082637
##     class counts:    60     6  3722     2   131
##    probabilities: 0.015 0.002 0.949 0.001 0.033 
##   left son=24574 (8 obs) right son=24575 (3913 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=5.770378, (0 missing)
##       Age                                   < 17.5 to the left,  improve=3.474435, (0 missing)
## 
## Node number 16382: 54 observations
##   predicted class=Employed            expected loss=0.09259259  P(node) =0.0006999806
##     class counts:     0    49     0     1     4
##    probabilities: 0.000 0.907 0.000 0.019 0.074 
## 
## Node number 16383: 11734 observations,    complexity param=0.0001718267
##   predicted class=Retired             expected loss=0.08181353  P(node) =0.1521032
##     class counts:   623    13   322 10774     2
##    probabilities: 0.053 0.001 0.027 0.918 0.000 
##   left son=32766 (1428 obs) right son=32767 (10306 obs)
##   Primary splits:
##       Age                    < 66.5 to the left,  improve=22.59645, (0 missing)
##       Industry.my.fctrMining < 0.5  to the right, improve=22.40638, (0 missing)
## 
## Node number 19454: 864 observations
##   predicted class=Employed            expected loss=0.07407407  P(node) =0.01119969
##     class counts:     0   800     6     1    57
##    probabilities: 0.000 0.926 0.007 0.001 0.066 
## 
## Node number 19455: 11608 observations,    complexity param=0.009256772
##   predicted class=Not.in.Labor.Force  expected loss=0.4406444  P(node) =0.1504699
##     class counts:  2825   946  6493  1123   221
##    probabilities: 0.243 0.081 0.559 0.097 0.019 
##   left son=38910 (642 obs) right son=38911 (10966 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=738.10700, (0 missing)
##       Age                                                         < 44.5 to the right, improve=681.98090, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=433.82320, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 26.34573, (0 missing)
## 
## Node number 20478: 101 observations
##   predicted class=Employed            expected loss=0.04950495  P(node) =0.001309223
##     class counts:     0    96     0     2     3
##    probabilities: 0.000 0.950 0.000 0.020 0.030 
## 
## Node number 20479: 2591 observations,    complexity param=0.001687026
##   predicted class=Retired             expected loss=0.3898109  P(node) =0.0335861
##     class counts:   634    83   283  1581    10
##    probabilities: 0.245 0.032 0.109 0.610 0.004 
##   left son=40958 (60 obs) right son=40959 (2531 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=74.12104, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve=40.50551, (0 missing)
##       Age                         < 61.5 to the left,  improve=22.94488, (0 missing)
## 
## Node number 24574: 8 observations
##   predicted class=Employed            expected loss=0.375  P(node) =0.0001037008
##     class counts:     0     5     3     0     0
##    probabilities: 0.000 0.625 0.375 0.000 0.000 
## 
## Node number 24575: 3913 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.04957833  P(node) =0.05072267
##     class counts:    60     1  3719     2   131
##    probabilities: 0.015 0.000 0.950 0.001 0.033 
## 
## Node number 32766: 1428 observations
##   predicted class=Retired             expected loss=0.17507  P(node) =0.0185106
##     class counts:   172     2    76  1178     0
##    probabilities: 0.120 0.001 0.053 0.825 0.000 
## 
## Node number 32767: 10306 observations,    complexity param=0.0001718267
##   predicted class=Retired             expected loss=0.06889191  P(node) =0.1335926
##     class counts:   451    11   246  9596     2
##    probabilities: 0.044 0.001 0.024 0.931 0.000 
##   left son=65534 (12 obs) right son=65535 (10294 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=20.600160, (0 missing)
##       Age                    < 70.5 to the left,  improve= 4.406275, (0 missing)
## 
## Node number 38910: 642 observations
##   predicted class=Employed            expected loss=0.07165109  P(node) =0.008321991
##     class counts:     2   596     7     1    36
##    probabilities: 0.003 0.928 0.011 0.002 0.056 
## 
## Node number 38911: 10966 observations,    complexity param=0.007591615
##   predicted class=Not.in.Labor.Force  expected loss=0.4085355  P(node) =0.1421479
##     class counts:  2823   350  6486  1122   185
##    probabilities: 0.257 0.032 0.591 0.102 0.017 
##   left son=77822 (4676 obs) right son=77823 (6290 obs)
##   Primary splits:
##       Age                          < 43.5 to the right, improve=710.94140, (0 missing)
##       Industry.my.fctrMining       < 0.5  to the right, improve=485.31870, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 27.25333, (0 missing)
## 
## Node number 40958: 60 observations
##   predicted class=Employed            expected loss=0.1  P(node) =0.0007777562
##     class counts:     0    54     0     0     6
##    probabilities: 0.000 0.900 0.000 0.000 0.100 
## 
## Node number 40959: 2531 observations,    complexity param=0.0009059952
##   predicted class=Retired             expected loss=0.3753457  P(node) =0.03280835
##     class counts:   634    29   283  1581     4
##    probabilities: 0.250 0.011 0.112 0.625 0.002 
##   left son=81918 (29 obs) right son=81919 (2502 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=42.32207, (0 missing)
##       Age                    < 61.5 to the left,  improve=22.59598, (0 missing)
## 
## Node number 65534: 12 observations
##   predicted class=Employed            expected loss=0.08333333  P(node) =0.0001555512
##     class counts:     0    11     0     0     1
##    probabilities: 0.000 0.917 0.000 0.000 0.083 
## 
## Node number 65535: 10294 observations
##   predicted class=Retired             expected loss=0.06780649  P(node) =0.133437
##     class counts:   451     0   246  9596     1
##    probabilities: 0.044 0.000 0.024 0.932 0.000 
## 
## Node number 77822: 4676 observations,    complexity param=0.004592458
##   predicted class=Disabled            expected loss=0.5902481  P(node) =0.06061313
##     class counts:  1916   147  1673   927    13
##    probabilities: 0.410 0.031 0.358 0.198 0.003 
##   left son=155644 (152 obs) right son=155645 (4524 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=190.3492, (0 missing)
##       Age                    < 53.5 to the left,  improve= 92.3401, (0 missing)
## 
## Node number 77823: 6290 observations,    complexity param=0.006310725
##   predicted class=Not.in.Labor.Force  expected loss=0.2348172  P(node) =0.08153477
##     class counts:   907   203  4813   195   172
##    probabilities: 0.144 0.032 0.765 0.031 0.027 
##   left son=155646 (211 obs) right son=155647 (6079 obs)
##   Primary splits:
##       Industry.my.fctrMining       < 0.5  to the right, improve=319.75960, (0 missing)
##       Age                          < 26.5 to the right, improve= 58.87956, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 29.62228, (0 missing)
## 
## Node number 81918: 29 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0003759155
##     class counts:     0    29     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 81919: 2502 observations
##   predicted class=Retired             expected loss=0.3681055  P(node) =0.03243243
##     class counts:   634     0   283  1581     4
##    probabilities: 0.253 0.000 0.113 0.632 0.002 
## 
## Node number 155644: 152 observations
##   predicted class=Employed            expected loss=0.03289474  P(node) =0.001970316
##     class counts:     0   147     0     0     5
##    probabilities: 0.000 0.967 0.000 0.000 0.033 
## 
## Node number 155645: 4524 observations,    complexity param=0.003249086
##   predicted class=Disabled            expected loss=0.576481  P(node) =0.05864282
##     class counts:  1916     0  1673   927     8
##    probabilities: 0.424 0.000 0.370 0.205 0.002 
##   left son=311290 (2474 obs) right son=311291 (2050 obs)
##   Primary splits:
##       Age < 53.5 to the left,  improve=96.05737, (0 missing)
## 
## Node number 155646: 211 observations
##   predicted class=Employed            expected loss=0.03791469  P(node) =0.002735109
##     class counts:     0   203     1     0     7
##    probabilities: 0.000 0.962 0.005 0.000 0.033 
## 
## Node number 155647: 6079 observations,    complexity param=0.0002030679
##   predicted class=Not.in.Labor.Force  expected loss=0.2084224  P(node) =0.07879966
##     class counts:   907     0  4812   195   165
##    probabilities: 0.149 0.000 0.792 0.032 0.027 
##   left son=311294 (3558 obs) right son=311295 (2521 obs)
##   Primary splits:
##       Age                          < 26.5 to the right, improve=51.65542, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve=30.42549, (0 missing)
## 
## Node number 311290: 2474 observations,    complexity param=0.002530538
##   predicted class=Not.in.Labor.Force  expected loss=0.5359741  P(node) =0.03206948
##     class counts:  1044     0  1148   276     6
##    probabilities: 0.422 0.000 0.464 0.112 0.002 
##   left son=622580 (1136 obs) right son=622581 (1338 obs)
##   Primary splits:
##       Age < 49.5 to the right, improve=20.27439, (0 missing)
## 
## Node number 311291: 2050 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.5746341  P(node) =0.02657334
##     class counts:   872     0   525   651     2
##    probabilities: 0.425 0.000 0.256 0.318 0.001 
##   left son=622582 (987 obs) right son=622583 (1063 obs)
##   Primary splits:
##       Age < 56.5 to the left,  improve=11.09287, (0 missing)
## 
## Node number 311294: 3558 observations,    complexity param=0.0002030679
##   predicted class=Not.in.Labor.Force  expected loss=0.2613828  P(node) =0.04612094
##     class counts:   724     0  2628   153    53
##    probabilities: 0.203 0.000 0.739 0.043 0.015 
##   left son=622588 (3545 obs) right son=622589 (13 obs)
##   Primary splits:
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve=20.34446, (0 missing)
##       Age                          < 37.5 to the right, improve=13.39218, (0 missing)
## 
## Node number 311295: 2521 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.1336771  P(node) =0.03267872
##     class counts:   183     0  2184    42   112
##    probabilities: 0.073 0.000 0.866 0.017 0.044 
## 
## Node number 622580: 1136 observations
##   predicted class=Disabled            expected loss=0.5440141  P(node) =0.01472552
##     class counts:   518     0   437   180     1
##    probabilities: 0.456 0.000 0.385 0.158 0.001 
## 
## Node number 622581: 1338 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.4686099  P(node) =0.01734396
##     class counts:   526     0   711    96     5
##    probabilities: 0.393 0.000 0.531 0.072 0.004 
## 
## Node number 622582: 987 observations
##   predicted class=Disabled            expected loss=0.5542047  P(node) =0.01279409
##     class counts:   440     0   293   253     1
##    probabilities: 0.446 0.000 0.297 0.256 0.001 
## 
## Node number 622583: 1063 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.593603  P(node) =0.01377925
##     class counts:   432     0   232   398     1
##    probabilities: 0.406 0.000 0.218 0.374 0.001 
##   left son=1245166 (314 obs) right son=1245167 (749 obs)
##   Primary splits:
##       Age < 57.5 to the left,  improve=1.268934, (0 missing)
## 
## Node number 622588: 3545 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.2586742  P(node) =0.04595243
##     class counts:   724     0  2628   153    40
##    probabilities: 0.204 0.000 0.741 0.043 0.011 
## 
## Node number 622589: 13 observations
##   predicted class=Unemployed          expected loss=0  P(node) =0.0001685138
##     class counts:     0     0     0     0    13
##    probabilities: 0.000 0.000 0.000 0.000 1.000 
## 
## Node number 1245166: 314 observations
##   predicted class=Disabled            expected loss=0.5732484  P(node) =0.004070257
##     class counts:   134     0    76   104     0
##    probabilities: 0.427 0.000 0.242 0.331 0.000 
## 
## Node number 1245167: 749 observations,    complexity param=3.124121e-05
##   predicted class=Disabled            expected loss=0.6021362  P(node) =0.00970899
##     class counts:   298     0   156   294     1
##    probabilities: 0.398 0.000 0.208 0.393 0.001 
##   left son=2490334 (385 obs) right son=2490335 (364 obs)
##   Primary splits:
##       Age < 58.5 to the left,  improve=0.1735246, (0 missing)
## 
## Node number 2490334: 385 observations
##   predicted class=Disabled            expected loss=0.6  P(node) =0.004990602
##     class counts:   154     0    84   147     0
##    probabilities: 0.400 0.000 0.218 0.382 0.000 
## 
## Node number 2490335: 364 observations
##   predicted class=Retired             expected loss=0.5961538  P(node) =0.004718387
##     class counts:   144     0    72   147     1
##    probabilities: 0.396 0.000 0.198 0.404 0.003 
## 
## n= 77145 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##       1) root 77145 32009 Employed (0.054 0.59 0.14 0.18 0.04)  
##         2) Age< 64.5 62433 20075 Employed (0.057 0.68 0.17 0.044 0.047)  
##           4) Age>=18.5 57429 15973 Employed (0.061 0.72 0.12 0.048 0.047)  
##             8) Industry.my.fctrEducational and health services>=0.5 10137   508 Employed (0.00089 0.95 0.005 0.0013 0.043) *
##             9) Industry.my.fctrEducational and health services< 0.5 47292 15465 Employed (0.074 0.67 0.15 0.058 0.048)  
##              18) Age< 59.5 42249 12793 Employed (0.067 0.7 0.16 0.027 0.05)  
##                36) Industry.my.fctrTrade>=0.5 5469   432 Employed (0.0013 0.92 0.0073 0.00018 0.07) *
##                37) Industry.my.fctrTrade< 0.5 36780 12361 Employed (0.077 0.66 0.18 0.031 0.048)  
##                  74) Industry.my.fctrProfessional and business services>=0.5 4668   337 Employed (0.00064 0.93 0.0066 0 0.065) *
##                  75) Industry.my.fctrProfessional and business services< 0.5 32112 12024 Employed (0.088 0.63 0.21 0.035 0.045)  
##                   150) Industry.my.fctrManufacturing>=0.5 4364   279 Employed (0.00046 0.94 0.0034 0.00069 0.059) *
##                   151) Industry.my.fctrManufacturing< 0.5 27748 11745 Employed (0.1 0.58 0.24 0.041 0.043)  
##                     302) Industry.my.fctrLeisure and hospitality>=0.5 3747   361 Employed (0.00053 0.9 0.011 0.0008 0.084) *
##                     303) Industry.my.fctrLeisure and hospitality< 0.5 24001 11384 Employed (0.12 0.53 0.27 0.047 0.036)  
##                       606) Industry.my.fctrFinancial>=0.5 2696   102 Employed (0 0.96 0.0026 0.00037 0.035) *
##                       607) Industry.my.fctrFinancial< 0.5 21305 11282 Employed (0.13 0.47 0.31 0.053 0.037)  
##                        1214) Industry.my.fctrConstruction>=0.5 2819   234 Employed (0.00071 0.92 0.006 0.0011 0.075) *
##                        1215) Industry.my.fctrConstruction< 0.5 18486 11048 Employed (0.15 0.4 0.35 0.061 0.031)  
##                          2430) Industry.my.fctrTransportation and utilities>=0.5 2078   110 Employed (0 0.95 0.0029 0.00096 0.049) *
##                          2431) Industry.my.fctrTransportation and utilities< 0.5 16408  9891 Not.in.Labor.Force (0.17 0.33 0.4 0.069 0.028)  
##                            4862) Industry.my.fctrPublic administration>=0.5 1984    63 Employed (0.0005 0.97 0.0025 0.0015 0.027) *
##                            4863) Industry.my.fctrPublic administration< 0.5 14424  7912 Not.in.Labor.Force (0.2 0.25 0.45 0.078 0.029)  
##                              9726) Industry.my.fctrOther services>=0.5 1952   149 Employed (0.001 0.92 0.0067 0 0.069) *
##                              9727) Industry.my.fctrOther services< 0.5 12472  5973 Not.in.Labor.Force (0.23 0.14 0.52 0.09 0.022)  
##                               19454) Industry.my.fctrInformation>=0.5 864    64 Employed (0 0.93 0.0069 0.0012 0.066) *
##                               19455) Industry.my.fctrInformation< 0.5 11608  5115 Not.in.Labor.Force (0.24 0.081 0.56 0.097 0.019)  
##                                 38910) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 642    46 Employed (0.0031 0.93 0.011 0.0016 0.056) *
##                                 38911) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 10966  4480 Not.in.Labor.Force (0.26 0.032 0.59 0.1 0.017)  
##                                   77822) Age>=43.5 4676  2760 Disabled (0.41 0.031 0.36 0.2 0.0028)  
##                                    155644) Industry.my.fctrMining>=0.5 152     5 Employed (0 0.97 0 0 0.033) *
##                                    155645) Industry.my.fctrMining< 0.5 4524  2608 Disabled (0.42 0 0.37 0.2 0.0018)  
##                                      311290) Age< 53.5 2474  1326 Not.in.Labor.Force (0.42 0 0.46 0.11 0.0024)  
##                                        622580) Age>=49.5 1136   618 Disabled (0.46 0 0.38 0.16 0.00088) *
##                                        622581) Age< 49.5 1338   627 Not.in.Labor.Force (0.39 0 0.53 0.072 0.0037) *
##                                      311291) Age>=53.5 2050  1178 Disabled (0.43 0 0.26 0.32 0.00098)  
##                                        622582) Age< 56.5 987   547 Disabled (0.45 0 0.3 0.26 0.001) *
##                                        622583) Age>=56.5 1063   631 Disabled (0.41 0 0.22 0.37 0.00094)  
##                                         1245166) Age< 57.5 314   180 Disabled (0.43 0 0.24 0.33 0) *
##                                         1245167) Age>=57.5 749   451 Disabled (0.4 0 0.21 0.39 0.0013)  
##                                           2490334) Age< 58.5 385   231 Disabled (0.4 0 0.22 0.38 0) *
##                                           2490335) Age>=58.5 364   217 Retired (0.4 0 0.2 0.4 0.0027) *
##                                   77823) Age< 43.5 6290  1477 Not.in.Labor.Force (0.14 0.032 0.77 0.031 0.027)  
##                                    155646) Industry.my.fctrMining>=0.5 211     8 Employed (0 0.96 0.0047 0 0.033) *
##                                    155647) Industry.my.fctrMining< 0.5 6079  1267 Not.in.Labor.Force (0.15 0 0.79 0.032 0.027)  
##                                      311294) Age>=26.5 3558   930 Not.in.Labor.Force (0.2 0 0.74 0.043 0.015)  
##                                        622588) Industry.my.fctrArmed forces< 0.5 3545   917 Not.in.Labor.Force (0.2 0 0.74 0.043 0.011) *
##                                        622589) Industry.my.fctrArmed forces>=0.5 13     0 Unemployed (0 0 0 0 1) *
##                                      311295) Age< 26.5 2521   337 Not.in.Labor.Force (0.073 0 0.87 0.017 0.044) *
##              19) Age>=59.5 5043  2672 Employed (0.13 0.47 0.057 0.32 0.027)  
##                38) Industry.my.fctrTrade>=0.5 451    25 Employed (0.0022 0.94 0.0022 0.0067 0.044) *
##                39) Industry.my.fctrTrade< 0.5 4592  2647 Employed (0.14 0.42 0.062 0.35 0.026)  
##                  78) Industry.my.fctrProfessional and business services>=0.5 354    26 Employed (0 0.93 0 0.0085 0.065) *
##                  79) Industry.my.fctrProfessional and business services< 0.5 4238  2621 Employed (0.15 0.38 0.067 0.38 0.022)  
##                   158) Industry.my.fctrManufacturing>=0.5 361    34 Employed (0.0028 0.91 0 0.014 0.078) *
##                   159) Industry.my.fctrManufacturing< 0.5 3877  2276 Retired (0.16 0.33 0.073 0.41 0.017)  
##                     318) Industry.my.fctrFinancial>=0.5 242    10 Employed (0 0.96 0 0.0041 0.037) *
##                     319) Industry.my.fctrFinancial< 0.5 3635  2035 Retired (0.17 0.29 0.078 0.44 0.016)  
##                       638) Industry.my.fctrConstruction>=0.5 226    14 Employed (0 0.94 0 0.027 0.035) *
##                       639) Industry.my.fctrConstruction< 0.5 3409  1815 Retired (0.19 0.25 0.083 0.47 0.015)  
##                        1278) Industry.my.fctrLeisure and hospitality>=0.5 199    19 Employed (0.005 0.9 0 0.015 0.075) *
##                        1279) Industry.my.fctrLeisure and hospitality< 0.5 3210  1619 Retired (0.2 0.21 0.088 0.5 0.011)  
##                          2558) Industry.my.fctrTransportation and utilities>=0.5 182    13 Employed (0 0.93 0 0.0055 0.066) *
##                          2559) Industry.my.fctrTransportation and utilities< 0.5 3028  1438 Retired (0.21 0.16 0.094 0.53 0.0076)  
##                            5118) Industry.my.fctrPublic administration>=0.5 174     8 Employed (0 0.95 0.0057 0.029 0.011) *
##                            5119) Industry.my.fctrPublic administration< 0.5 2854  1269 Retired (0.22 0.12 0.099 0.56 0.0074)  
##                             10238) Industry.my.fctrOther services>=0.5 162    10 Employed (0 0.94 0 0.012 0.049) *
##                             10239) Industry.my.fctrOther services< 0.5 2692  1109 Retired (0.24 0.066 0.11 0.59 0.0048)  
##                               20478) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 101     5 Employed (0 0.95 0 0.02 0.03) *
##                               20479) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 2591  1010 Retired (0.24 0.032 0.11 0.61 0.0039)  
##                                 40958) Industry.my.fctrInformation>=0.5 60     6 Employed (0 0.9 0 0 0.1) *
##                                 40959) Industry.my.fctrInformation< 0.5 2531   950 Retired (0.25 0.011 0.11 0.62 0.0016)  
##                                   81918) Industry.my.fctrMining>=0.5 29     0 Employed (0 1 0 0 0) *
##                                   81919) Industry.my.fctrMining< 0.5 2502   921 Retired (0.25 0 0.11 0.63 0.0016) *
##           5) Age< 18.5 5004  1197 Not.in.Labor.Force (0.012 0.18 0.76 0.0004 0.047)  
##            10) Industry.my.fctrLeisure and hospitality>=0.5 487    71 Employed (0 0.85 0.072 0 0.074) *
##            11) Industry.my.fctrLeisure and hospitality< 0.5 4517   745 Not.in.Labor.Force (0.013 0.11 0.84 0.00044 0.044)  
##              22) Industry.my.fctrTrade>=0.5 232    31 Employed (0 0.87 0.034 0 0.099) *
##              23) Industry.my.fctrTrade< 0.5 4285   521 Not.in.Labor.Force (0.014 0.067 0.88 0.00047 0.041)  
##                46) Industry.my.fctrEducational and health services>=0.5 97    24 Employed (0 0.75 0.18 0 0.072) *
##                47) Industry.my.fctrEducational and health services< 0.5 4188   441 Not.in.Labor.Force (0.014 0.051 0.89 0.00048 0.04)  
##                  94) Industry.my.fctrOther services>=0.5 55    13 Employed (0 0.76 0.16 0 0.073) *
##                  95) Industry.my.fctrOther services< 0.5 4133   395 Not.in.Labor.Force (0.015 0.041 0.9 0.00048 0.039)  
##                   190) Industry.my.fctrProfessional and business services>=0.5 61    22 Employed (0 0.64 0.15 0 0.21) *
##                   191) Industry.my.fctrProfessional and business services< 0.5 4072   343 Not.in.Labor.Force (0.015 0.032 0.92 0.00049 0.037)  
##                     382) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 42     8 Employed (0 0.81 0.048 0 0.14) *
##                     383) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 4030   303 Not.in.Labor.Force (0.015 0.024 0.92 0.0005 0.036)  
##                       766) Industry.my.fctrConstruction>=0.5 30     5 Employed (0 0.83 0.033 0 0.13) *
##                       767) Industry.my.fctrConstruction< 0.5 4000   274 Not.in.Labor.Force (0.015 0.018 0.93 0.0005 0.035)  
##                        1534) Industry.my.fctrManufacturing>=0.5 36    12 Employed (0 0.67 0.11 0 0.22) *
##                        1535) Industry.my.fctrManufacturing< 0.5 3964   242 Not.in.Labor.Force (0.015 0.012 0.94 0.0005 0.033)  
##                          3070) Industry.my.fctrTransportation and utilities>=0.5 17     1 Employed (0 0.94 0 0 0.059) *
##                          3071) Industry.my.fctrTransportation and utilities< 0.5 3947   225 Not.in.Labor.Force (0.015 0.0081 0.94 0.00051 0.033)  
##                            6142) Industry.my.fctrFinancial>=0.5 14     0 Employed (0 1 0 0 0) *
##                            6143) Industry.my.fctrFinancial< 0.5 3933   211 Not.in.Labor.Force (0.015 0.0046 0.95 0.00051 0.033)  
##                             12286) Industry.my.fctrInformation>=0.5 12     0 Employed (0 1 0 0 0) *
##                             12287) Industry.my.fctrInformation< 0.5 3921   199 Not.in.Labor.Force (0.015 0.0015 0.95 0.00051 0.033)  
##                               24574) Industry.my.fctrPublic administration>=0.5 8     3 Employed (0 0.63 0.37 0 0) *
##                               24575) Industry.my.fctrPublic administration< 0.5 3913   194 Not.in.Labor.Force (0.015 0.00026 0.95 0.00051 0.033) *
##         3) Age>=64.5 14712  3866 Retired (0.043 0.19 0.022 0.74 0.0091)  
##           6) Industry.my.fctrEducational and health services>=0.5 696    54 Employed (0 0.92 0 0.034 0.043) *
##           7) Industry.my.fctrEducational and health services< 0.5 14016  3194 Retired (0.045 0.15 0.023 0.77 0.0074)  
##            14) Industry.my.fctrTrade>=0.5 402    27 Employed (0.005 0.93 0 0.03 0.032) *
##            15) Industry.my.fctrTrade< 0.5 13614  2804 Retired (0.046 0.13 0.024 0.79 0.0067)  
##              30) Industry.my.fctrProfessional and business services>=0.5 384    33 Employed (0.0026 0.91 0.0052 0.026 0.052) *
##              31) Industry.my.fctrProfessional and business services< 0.5 13230  2430 Retired (0.047 0.11 0.025 0.82 0.0054)  
##                62) Industry.my.fctrFinancial>=0.5 219    12 Employed (0 0.95 0 0.023 0.032) *
##                63) Industry.my.fctrFinancial< 0.5 13011  2216 Retired (0.048 0.092 0.025 0.83 0.0049)  
##                 126) Industry.my.fctrManufacturing>=0.5 217    15 Employed (0 0.93 0.0046 0.0092 0.055) *
##                 127) Industry.my.fctrManufacturing< 0.5 12794  2001 Retired (0.049 0.078 0.025 0.84 0.0041)  
##                   254) Industry.my.fctrOther services>=0.5 222    16 Employed (0 0.93 0.0045 0.032 0.036) *
##                   255) Industry.my.fctrOther services< 0.5 12572  1786 Retired (0.05 0.063 0.026 0.86 0.0035)  
##                     510) Industry.my.fctrLeisure and hospitality>=0.5 215    15 Employed (0 0.93 0 0.023 0.047) *
##                     511) Industry.my.fctrLeisure and hospitality< 0.5 12357  1576 Retired (0.05 0.048 0.026 0.87 0.0028)  
##                      1022) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 158     5 Employed (0 0.97 0 0.013 0.019) *
##                      1023) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 12199  1420 Retired (0.051 0.036 0.027 0.88 0.0025)  
##                        2046) Industry.my.fctrConstruction>=0.5 144    13 Employed (0 0.91 0.0069 0 0.083) *
##                        2047) Industry.my.fctrConstruction< 0.5 12055  1276 Retired (0.052 0.026 0.027 0.89 0.0016)  
##                          4094) Industry.my.fctrTransportation and utilities>=0.5 139    12 Employed (0 0.91 0.0072 0.029 0.05) *
##                          4095) Industry.my.fctrTransportation and utilities< 0.5 11916  1141 Retired (0.052 0.015 0.027 0.9 0.001)  
##                            8190) Industry.my.fctrPublic administration>=0.5 128     6 Employed (0 0.95 0 0 0.047) *
##                            8191) Industry.my.fctrPublic administration< 0.5 11788  1013 Retired (0.053 0.0053 0.027 0.91 0.00051)  
##                             16382) Industry.my.fctrInformation>=0.5 54     5 Employed (0 0.91 0 0.019 0.074) *
##                             16383) Industry.my.fctrInformation< 0.5 11734   960 Retired (0.053 0.0011 0.027 0.92 0.00017)  
##                               32766) Age< 66.5 1428   250 Retired (0.12 0.0014 0.053 0.82 0) *
##                               32767) Age>=66.5 10306   710 Retired (0.044 0.0011 0.024 0.93 0.00019)  
##                                 65534) Industry.my.fctrMining>=0.5 12     1 Employed (0 0.92 0 0 0.083) *
##                                 65535) Industry.my.fctrMining< 0.5 10294   698 Retired (0.044 0 0.024 0.93 9.7e-05) *
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

```
## Warning in `[<-.data.frame`(`*tmp*`, , paste0(rsp_var_out, ".prob"), value
## = structure(list(: provided 5 variables to replace 1 variables
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                                               importance
## Age                                                           100.000000
## Industry.my.fctrOther services                                 33.090343
## Industry.my.fctrPublic administration                          29.566192
## Industry.my.fctrManufacturing                                  28.370178
## Industry.my.fctrEducational and health services                27.058509
## Industry.my.fctrProfessional and business services             25.591436
## Industry.my.fctrTrade                                          24.329022
## Industry.my.fctrTransportation and utilities                   23.012814
## Industry.my.fctrConstruction                                   22.685481
## Industry.my.fctrLeisure and hospitality                        22.062655
## Industry.my.fctrFinancial                                      21.645346
## Industry.my.fctrAgriculture, forestry, fishing, and hunting    20.237759
## Industry.my.fctrInformation                                    15.923595
## Industry.my.fctrMining                                         11.722476
## Industry.my.fctrArmed forces                                    0.784697
## `Industry.my.fctrLeisure and hospitality`                       0.000000
## `Industry.my.fctrEducational and health services`               0.000000
## `Industry.my.fctrOther services`                                0.000000
## `Industry.my.fctrTransportation and utilities`                  0.000000
## `Industry.my.fctrProfessional and business services`            0.000000
## `Industry.my.fctrPublic administration`                         0.000000
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`   0.000000
## `Industry.my.fctrArmed forces`                                  0.000000
##                                                               Max.cor.Y.cv.0.cp.0.rpart.importance
## Age                                                                                     100.000000
## Industry.my.fctrOther services                                                           33.090343
## Industry.my.fctrPublic administration                                                    29.566192
## Industry.my.fctrManufacturing                                                            28.370178
## Industry.my.fctrEducational and health services                                          27.058509
## Industry.my.fctrProfessional and business services                                       25.591436
## Industry.my.fctrTrade                                                                    24.329022
## Industry.my.fctrTransportation and utilities                                             23.012814
## Industry.my.fctrConstruction                                                             22.685481
## Industry.my.fctrLeisure and hospitality                                                  22.062655
## Industry.my.fctrFinancial                                                                21.645346
## Industry.my.fctrAgriculture, forestry, fishing, and hunting                              20.237759
## Industry.my.fctrInformation                                                              15.923595
## Industry.my.fctrMining                                                                   11.722476
## Industry.my.fctrArmed forces                                                              0.784697
## `Industry.my.fctrLeisure and hospitality`                                                 0.000000
## `Industry.my.fctrEducational and health services`                                         0.000000
## `Industry.my.fctrOther services`                                                          0.000000
## `Industry.my.fctrTransportation and utilities`                                            0.000000
## `Industry.my.fctrProfessional and business services`                                      0.000000
## `Industry.my.fctrPublic administration`                                                   0.000000
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`                             0.000000
## `Industry.my.fctrArmed forces`                                                            0.000000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](us_cps2_files/figure-html/fit.models_2-5.png) ![](us_cps2_files/figure-html/fit.models_2-6.png) 

```
## [1] "Min/Max Boundaries: "
##       .rownames EmploymentStatus.fctr
## 232       83771               Retired
## 933       80246    Not.in.Labor.Force
## 4110     111997              Disabled
## 5360      55065              Disabled
## 12602     41421              Disabled
## 18898      1371              Disabled
## 26299     20313    Not.in.Labor.Force
## 13635     92281            Unemployed
## 26264     61504            Unemployed
## 27111     57730              Disabled
## 31868     66224              Disabled
## 41907     49837              Disabled
## 43708     66821              Disabled
## 12806     39311            Unemployed
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart
## 232                                                   Retired
## 933                                        Not.in.Labor.Force
## 4110                                                  Retired
## 5360                                                  Retired
## 12602                                                 Retired
## 18898                                                 Retired
## 26299                                                 Retired
## 13635                                      Not.in.Labor.Force
## 26264                                      Not.in.Labor.Force
## 27111                                      Not.in.Labor.Force
## 31868                                      Not.in.Labor.Force
## 41907                                      Not.in.Labor.Force
## 43708                                      Not.in.Labor.Force
## 12806                                                Employed
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob
## 232                                                    0.043811929
## 933                                                    0.015333504
## 4110                                                   0.043811929
## 5360                                                   0.043811929
## 12602                                                  0.043811929
## 18898                                                  0.043811929
## 26299                                                  0.043811929
## 13635                                                  0.015333504
## 26264                                                  0.015333504
## 27111                                                  0.015333504
## 31868                                                  0.015333504
## 41907                                                  0.015333504
## 43708                                                  0.015333504
## 12806                                                  0.002604167
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## 232                                                               TRUE
## 933                                                               TRUE
## 4110                                                             FALSE
## 5360                                                             FALSE
## 12602                                                            FALSE
## 18898                                                            FALSE
## 26299                                                            FALSE
## 13635                                                            FALSE
## 26264                                                            FALSE
## 27111                                                            FALSE
## 31868                                                            FALSE
## 41907                                                            FALSE
## 43708                                                            FALSE
## 12806                                                            FALSE
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.error .label
## 232                                                       0.0000000  83771
## 933                                                       0.0000000  80246
## 4110                                                      0.9561881 111997
## 5360                                                      0.9561881  55065
## 12602                                                     0.9561881  41421
## 18898                                                     0.9561881   1371
## 26299                                                     0.9561881  20313
## 13635                                                     0.9846665  92281
## 26264                                                     0.9846665  61504
## 27111                                                     0.9846665  57730
## 31868                                                     0.9846665  66224
## 41907                                                     0.9846665  49837
## 43708                                                     0.9846665  66821
## 12806                                                     0.9973958  39311
## [1] "Inaccurate: "
##      .rownames EmploymentStatus.fctr
## 1751      1258               Retired
## 2857      1287               Retired
## 4163    110947               Retired
## 5504     56944               Retired
## 5947     56276    Not.in.Labor.Force
## 7059       286    Not.in.Labor.Force
##      EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart
## 1751                                                Disabled
## 2857                                                Disabled
## 4163                                                Disabled
## 5504                                                Disabled
## 5947                                                Disabled
## 7059                                                Disabled
##      EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob
## 1751                                                    0.4559859
## 2857                                                    0.4559859
## 4163                                                    0.4559859
## 5504                                                    0.4559859
## 5947                                                    0.4559859
## 7059                                                    0.4559859
##      EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## 1751                                                            FALSE
## 2857                                                            FALSE
## 4163                                                            FALSE
## 5504                                                            FALSE
## 5947                                                            FALSE
## 7059                                                            FALSE
##      EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.error
## 1751                                                     0.5440141
## 2857                                                     0.5440141
## 4163                                                     0.5440141
## 5504                                                     0.5440141
## 5947                                                     0.5440141
## 7059                                                     0.5440141
##       .rownames EmploymentStatus.fctr
## 48873    107691    Not.in.Labor.Force
## 90321    126562              Disabled
## 32989    100297            Unemployed
## 7390        436            Unemployed
## 63675     96452            Unemployed
## 71179     47259            Unemployed
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart
## 48873                                                Disabled
## 90321                                                 Retired
## 32989                                      Not.in.Labor.Force
## 7390                                                 Employed
## 63675                                                Employed
## 71179                                                Employed
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob
## 48873                                                 0.4457953394
## 90321                                                 0.0438119293
## 32989                                                 0.0153335037
## 7390                                                  0.0012799415
## 63675                                                 0.0007094714
## 71179                                                 0.0004582951
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## 48873                                                            FALSE
## 90321                                                            FALSE
## 32989                                                            FALSE
## 7390                                                             FALSE
## 63675                                                            FALSE
## 71179                                                            FALSE
##       EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.error
## 48873                                                     0.5542047
## 90321                                                     0.9561881
## 32989                                                     0.9846665
## 7390                                                      0.9987201
## 63675                                                     0.9992905
## 71179                                                     0.9995417
##        .rownames EmploymentStatus.fctr
## 129200     73943            Unemployed
## 129586     21684            Unemployed
## 130229     58290            Unemployed
## 130736     20447            Unemployed
## 130934     33835            Unemployed
## 131084     40385              Disabled
##        EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart
## 129200                                                Employed
## 129586                                                Employed
## 130229                                                Employed
## 130736                                                Employed
## 130934                                                Employed
## 131084                                                Employed
##        EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob
## 129200                                                            0
## 129586                                                            0
## 130229                                                            0
## 130736                                                            0
## 130934                                                            0
## 131084                                                            0
##        EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## 129200                                                            FALSE
## 129586                                                            FALSE
## 130229                                                            FALSE
## 130736                                                            FALSE
## 130934                                                            FALSE
## 131084                                                            FALSE
##        EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.error
## 129200                                                             1
## 129586                                                             1
## 130229                                                             1
## 130736                                                             1
## 130934                                                             1
## 131084                                                             1
```

![](us_cps2_files/figure-html/fit.models_2-7.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
```

```
## [1] EmploymentStatus.fctr                                           
## [2] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart         
## [3] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob    
## [4] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] Age             Sex.fctr        Married.my.fctr Region.fctr    
## [5] .rnorm         
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## data frame with 0 columns and 0 rows
```

```r
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 244.919 268.677  23.758
## 13 fit.models          7          3 268.677      NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart"         
## [2] "EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob"    
## [3] "EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](us_cps2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 268.677 281.596   12.92
## 14 fit.data.training          8          0 281.597      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.rpart"
## [1] "    indep_vars: Industry.my.fctr, Age"
## Aggregating results
## Fitting final model on full training set
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 105513 
## 
##              CP nsplit rel error
## 1  0.2499771585      0 1.0000000
## 2  0.0888533577      1 0.7500228
## 3  0.0227272727      2 0.6611695
## 4  0.0167884879      3 0.6384422
## 5  0.0135449977      4 0.6216537
## 6  0.0127912289      5 0.6081087
## 7  0.0102558246      6 0.5953175
## 8  0.0089107152      7 0.5850617
## 9  0.0080630425     23 0.3582001
## 10 0.0076975788     24 0.3501370
## 11 0.0073777981     25 0.3424395
## 12 0.0071950662     26 0.3350617
## 13 0.0066240292     27 0.3278666
## 14 0.0052992234     28 0.3212426
## 15 0.0046596619     29 0.3159434
## 16 0.0046368205     30 0.3112837
## 17 0.0045911375     31 0.3066469
## 18 0.0037460027     32 0.3020557
## 19 0.0026496117     33 0.2983097
## 20 0.0020100503     34 0.2956601
## 21 0.0019186843     35 0.2936501
## 22 0.0016902695     36 0.2917314
## 23 0.0015303792     37 0.2900411
## 24 0.0013933303     38 0.2885107
## 25 0.0010963910     39 0.2871174
## 26 0.0009821836     40 0.2860210
## 27 0.0007994518     41 0.2850388
## 28 0.0007766103     42 0.2842394
## 29 0.0007537688     43 0.2834628
## 30 0.0002512563     44 0.2827090
## 31 0.0002055733     45 0.2824577
## 32 0.0001713111     47 0.2820466
## 33 0.0001598904     49 0.2817040
## 34 0.0001370489     50 0.2815441
## 35 0.0000000000     51 0.2814070
## 
## Variable importance
##                                                         Age 
##                                                          42 
##                              Industry.my.fctrOther services 
##                                                           6 
##             Industry.my.fctrEducational and health services 
##                                                           6 
##                     Industry.my.fctrLeisure and hospitality 
##                                                           5 
##                Industry.my.fctrTransportation and utilities 
##                                                           5 
##                                       Industry.my.fctrTrade 
##                                                           5 
##                                Industry.my.fctrConstruction 
##                                                           4 
##                       Industry.my.fctrPublic administration 
##                                                           4 
##                                   Industry.my.fctrFinancial 
##                                                           4 
##          Industry.my.fctrProfessional and business services 
##                                                           4 
##                               Industry.my.fctrManufacturing 
##                                                           4 
## Industry.my.fctrAgriculture, forestry, fishing, and hunting 
##                                                           4 
##                                 Industry.my.fctrInformation 
##                                                           3 
##                                      Industry.my.fctrMining 
##                                                           2 
## 
## Node number 1: 105513 observations,    complexity param=0.2499772
##   predicted class=Employed            expected loss=0.4149252  P(node) =1
##     class counts:  5712 61733 15246 18619  4203
##    probabilities: 0.054 0.585 0.144 0.176 0.040 
##   left son=2 (83904 obs) right son=3 (21609 obs)
##   Primary splits:
##       Age                                                < 63.5 to the left,  improve=12147.510, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve= 3188.841, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 1623.725, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 1349.429, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 1254.102, (0 missing)
## 
## Node number 2: 83904 observations,    complexity param=0.08885336
##   predicted class=Employed            expected loss=0.3176249  P(node) =0.7952006
##     class counts:  4727 57254 14741  3196  3986
##    probabilities: 0.056 0.682 0.176 0.038 0.048 
##   left son=4 (75489 obs) right son=5 (8415 obs)
##   Primary splits:
##       Age                                                < 19.5 to the right, improve=4446.9060, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1719.4460, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 826.8062, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 684.5403, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 663.4894, (0 missing)
## 
## Node number 3: 21609 observations,    complexity param=0.02272727
##   predicted class=Retired             expected loss=0.2862696  P(node) =0.2047994
##     class counts:   985  4479   505 15423   217
##    probabilities: 0.046 0.207 0.023 0.714 0.010 
##   left son=6 (1104 obs) right son=7 (20505 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1157.8420, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 677.5990, (0 missing)
##       Age                                                < 70.5 to the left,  improve= 657.3056, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 635.9218, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 399.3432, (0 missing)
## 
## Node number 4: 75489 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.2676681  P(node) =0.7154474
##     class counts:  4622 55283  8880  3188  3516
##    probabilities: 0.061 0.732 0.118 0.042 0.047 
##   left son=8 (13669 obs) right son=9 (61820 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=1092.0010, (0 missing)
##       Age                                                < 57.5 to the left,  improve= 564.9899, (0 missing)
##       Industry.my.fctrTrade                              < 0.5  to the right, improve= 477.2123, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 418.9047, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve= 405.9581, (0 missing)
## 
## Node number 5: 8415 observations,    complexity param=0.01678849
##   predicted class=Not.in.Labor.Force  expected loss=0.3035056  P(node) =0.07975321
##     class counts:   105  1971  5861     8   470
##    probabilities: 0.012 0.234 0.696 0.001 0.056 
##   left son=10 (930 obs) right son=11 (7485 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality         < 0.5  to the right, improve=821.24650, (0 missing)
##       Industry.my.fctrTrade                           < 0.5  to the right, improve=483.79540, (0 missing)
##       Age                                             < 17.5 to the right, improve=315.44200, (0 missing)
##       Industry.my.fctrEducational and health services < 0.5  to the right, improve=162.07580, (0 missing)
##       Industry.my.fctrOther services                  < 0.5  to the right, improve= 87.34646, (0 missing)
## 
## Node number 6: 1104 observations
##   predicted class=Employed            expected loss=0.07065217  P(node) =0.01046317
##     class counts:     0  1026     0    31    47
##    probabilities: 0.000 0.929 0.000 0.028 0.043 
## 
## Node number 7: 20505 observations,    complexity param=0.013545
##   predicted class=Retired             expected loss=0.2493538  P(node) =0.1943362
##     class counts:   985  3453   505 15392   170
##    probabilities: 0.048 0.168 0.025 0.751 0.008 
##   left son=14 (647 obs) right son=15 (19858 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=752.9797, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=707.5644, (0 missing)
##       Age                                                < 69.5 to the left,  improve=469.6243, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=443.2823, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=404.4244, (0 missing)
## 
## Node number 8: 13669 observations
##   predicted class=Employed            expected loss=0.05004024  P(node) =0.129548
##     class counts:    13 12985    65    18   588
##    probabilities: 0.001 0.950 0.005 0.001 0.043 
## 
## Node number 9: 61820 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.3157878  P(node) =0.5858994
##     class counts:  4609 42298  8815  3170  2928
##    probabilities: 0.075 0.684 0.143 0.051 0.047 
##   left son=18 (7743 obs) right son=19 (54077 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=751.9581, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=655.3610, (0 missing)
##       Age                                                < 57.5 to the left,  improve=651.8145, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=629.3461, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=439.6490, (0 missing)
## 
## Node number 10: 930 observations
##   predicted class=Employed            expected loss=0.1462366  P(node) =0.00881408
##     class counts:     0   794    59     0    77
##    probabilities: 0.000 0.854 0.063 0.000 0.083 
## 
## Node number 11: 7485 observations,    complexity param=0.01025582
##   predicted class=Not.in.Labor.Force  expected loss=0.2248497  P(node) =0.07093913
##     class counts:   105  1177  5802     8   393
##    probabilities: 0.014 0.157 0.775 0.001 0.053 
##   left son=22 (543 obs) right son=23 (6942 obs)
##   Primary splits:
##       Industry.my.fctrTrade                              < 0.5  to the right, improve=612.76670, (0 missing)
##       Age                                                < 17.5 to the right, improve=239.19990, (0 missing)
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=210.34410, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=112.11510, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve= 95.73304, (0 missing)
## 
## Node number 14: 647 observations
##   predicted class=Employed            expected loss=0.05873261  P(node) =0.006131946
##     class counts:     3   609     1    16    18
##    probabilities: 0.005 0.941 0.002 0.025 0.028 
## 
## Node number 15: 19858 observations,    complexity param=0.01279123
##   predicted class=Retired             expected loss=0.2257025  P(node) =0.1882043
##     class counts:   982  2844   504 15376   152
##    probabilities: 0.049 0.143 0.025 0.774 0.008 
##   left son=30 (624 obs) right son=31 (19234 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=755.7690, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=472.8057, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=431.4262, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=419.6137, (0 missing)
##       Age                                                < 69.5 to the left,  improve=392.8600, (0 missing)
## 
## Node number 18: 7743 observations
##   predicted class=Employed            expected loss=0.07464807  P(node) =0.07338432
##     class counts:    10  7165    50     6   512
##    probabilities: 0.001 0.925 0.006 0.001 0.066 
## 
## Node number 19: 54077 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.3503153  P(node) =0.512515
##     class counts:  4599 35133  8765  3164  2416
##    probabilities: 0.085 0.650 0.162 0.059 0.045 
##   left son=38 (6767 obs) right son=39 (47310 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=870.1717, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=830.7705, (0 missing)
##       Age                                                < 57.5 to the left,  improve=692.9737, (0 missing)
##       Industry.my.fctrLeisure and hospitality            < 0.5  to the right, improve=575.1856, (0 missing)
##       Industry.my.fctrFinancial                          < 0.5  to the right, improve=565.4602, (0 missing)
## 
## Node number 22: 543 observations
##   predicted class=Employed            expected loss=0.1362799  P(node) =0.005146285
##     class counts:     0   469    20     0    54
##    probabilities: 0.000 0.864 0.037 0.000 0.099 
## 
## Node number 23: 6942 observations,    complexity param=0.003746003
##   predicted class=Not.in.Labor.Force  expected loss=0.1670988  P(node) =0.06579284
##     class counts:   105   708  5782     8   339
##    probabilities: 0.015 0.102 0.833 0.001 0.049 
##   left son=46 (244 obs) right son=47 (6698 obs)
##   Primary splits:
##       Industry.my.fctrEducational and health services    < 0.5  to the right, improve=249.47940, (0 missing)
##       Age                                                < 17.5 to the right, improve=136.16640, (0 missing)
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=132.10790, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=114.47680, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve= 99.72118, (0 missing)
## 
## Node number 30: 624 observations
##   predicted class=Employed            expected loss=0.08173077  P(node) =0.005913963
##     class counts:     1   573     4    13    33
##    probabilities: 0.002 0.918 0.006 0.021 0.053 
## 
## Node number 31: 19234 observations,    complexity param=0.008063042
##   predicted class=Retired             expected loss=0.2012582  P(node) =0.1822903
##     class counts:   981  2271   500 15363   119
##    probabilities: 0.051 0.118 0.026 0.799 0.006 
##   left son=62 (383 obs) right son=63 (18851 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=503.8076, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=459.7628, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=447.4862, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=411.0300, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=330.4545, (0 missing)
## 
## Node number 38: 6767 observations
##   predicted class=Employed            expected loss=0.07285355  P(node) =0.06413428
##     class counts:     7  6274    35     3   448
##    probabilities: 0.001 0.927 0.005 0.000 0.066 
## 
## Node number 39: 47310 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.3900021  P(node) =0.4483808
##     class counts:  4592 28859  8730  3161  1968
##    probabilities: 0.097 0.610 0.185 0.067 0.042 
##   left son=78 (6319 obs) right son=79 (40991 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing           < 0.5  to the right, improve=1101.9840, (0 missing)
##       Industry.my.fctrLeisure and hospitality < 0.5  to the right, improve= 772.3607, (0 missing)
##       Industry.my.fctrFinancial               < 0.5  to the right, improve= 731.6832, (0 missing)
##       Age                                     < 57.5 to the left,  improve= 724.2414, (0 missing)
##       Industry.my.fctrConstruction            < 0.5  to the right, improve= 634.3962, (0 missing)
## 
## Node number 46: 244 observations
##   predicted class=Employed            expected loss=0.2090164  P(node) =0.002312511
##     class counts:     0   193    29     0    22
##    probabilities: 0.000 0.791 0.119 0.000 0.090 
## 
## Node number 47: 6698 observations,    complexity param=0.00201005
##   predicted class=Not.in.Labor.Force  expected loss=0.1410869  P(node) =0.06348033
##     class counts:   105   515  5753     8   317
##    probabilities: 0.016 0.077 0.859 0.001 0.047 
##   left son=94 (124 obs) right son=95 (6574 obs)
##   Primary splits:
##       Industry.my.fctrOther services                     < 0.5  to the right, improve=141.70320, (0 missing)
##       Industry.my.fctrProfessional and business services < 0.5  to the right, improve=123.52010, (0 missing)
##       Industry.my.fctrConstruction                       < 0.5  to the right, improve=106.74930, (0 missing)
##       Industry.my.fctrManufacturing                      < 0.5  to the right, improve=101.13560, (0 missing)
##       Age                                                < 17.5 to the right, improve= 93.61962, (0 missing)
## 
## Node number 62: 383 observations
##   predicted class=Employed            expected loss=0.06788512  P(node) =0.003629884
##     class counts:     0   357     2     4    20
##    probabilities: 0.000 0.932 0.005 0.010 0.052 
## 
## Node number 63: 18851 observations,    complexity param=0.007377798
##   predicted class=Retired             expected loss=0.1852422  P(node) =0.1786604
##     class counts:   981  1914   498 15359    99
##    probabilities: 0.052 0.102 0.026 0.815 0.005 
##   left son=126 (352 obs) right son=127 (18499 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=478.8641, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=466.2788, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=428.3619, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=343.8240, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=300.2703, (0 missing)
## 
## Node number 78: 6319 observations
##   predicted class=Employed            expected loss=0.06678272  P(node) =0.05988835
##     class counts:     8  5897    21    10   383
##    probabilities: 0.001 0.933 0.003 0.002 0.061 
## 
## Node number 79: 40991 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.4398283  P(node) =0.3884924
##     class counts:  4584 22962  8709  3151  1585
##    probabilities: 0.112 0.560 0.212 0.077 0.039 
##   left son=158 (5109 obs) right son=159 (35882 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality < 0.5  to the right, improve=1064.4410, (0 missing)
##       Industry.my.fctrFinancial               < 0.5  to the right, improve= 973.0519, (0 missing)
##       Industry.my.fctrConstruction            < 0.5  to the right, improve= 864.2965, (0 missing)
##       Age                                     < 55.5 to the left,  improve= 797.2025, (0 missing)
##       Industry.my.fctrPublic administration   < 0.5  to the right, improve= 719.5783, (0 missing)
## 
## Node number 94: 124 observations
##   predicted class=Employed            expected loss=0.2096774  P(node) =0.001175211
##     class counts:     0    98    10     0    16
##    probabilities: 0.000 0.790 0.081 0.000 0.129 
## 
## Node number 95: 6574 observations,    complexity param=0.00169027
##   predicted class=Not.in.Labor.Force  expected loss=0.1264071  P(node) =0.06230512
##     class counts:   105   417  5743     8   301
##    probabilities: 0.016 0.063 0.874 0.001 0.046 
##   left son=190 (128 obs) right son=191 (6446 obs)
##   Primary splits:
##       Industry.my.fctrProfessional and business services          < 0.5  to the right, improve=128.68200, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=110.73290, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=104.99030, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 81.24365, (0 missing)
##       Age                                                         < 17.5 to the right, improve= 75.73573, (0 missing)
## 
## Node number 126: 352 observations
##   predicted class=Employed            expected loss=0.05965909  P(node) =0.003336082
##     class counts:     0   331     1     8    12
##    probabilities: 0.000 0.940 0.003 0.023 0.034 
## 
## Node number 127: 18499 observations,    complexity param=0.007195066
##   predicted class=Retired             expected loss=0.1701714  P(node) =0.1753244
##     class counts:   981  1583   497 15351    87
##    probabilities: 0.053 0.086 0.027 0.830 0.005 
##   left son=254 (350 obs) right son=255 (18149 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=484.5522, (0 missing)
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=445.2126, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=356.8226, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=311.9446, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=310.0414, (0 missing)
## 
## Node number 158: 5109 observations
##   predicted class=Employed            expected loss=0.09062439  P(node) =0.04842057
##     class counts:     3  4646    54     6   400
##    probabilities: 0.001 0.909 0.011 0.001 0.078 
## 
## Node number 159: 35882 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.4895491  P(node) =0.3400718
##     class counts:  4581 18316  8655  3145  1185
##    probabilities: 0.128 0.510 0.241 0.088 0.033 
##   left son=318 (3949 obs) right son=319 (31933 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=1257.5160, (0 missing)
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=1139.3140, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve= 923.8724, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve= 883.1958, (0 missing)
##       Age                                          < 54.5 to the left,  improve= 794.6089, (0 missing)
## 
## Node number 190: 128 observations
##   predicted class=Employed            expected loss=0.296875  P(node) =0.001213121
##     class counts:     0    90    16     0    22
##    probabilities: 0.000 0.703 0.125 0.000 0.172 
## 
## Node number 191: 6446 observations,    complexity param=0.001530379
##   predicted class=Not.in.Labor.Force  expected loss=0.111542  P(node) =0.061092
##     class counts:   105   327  5727     8   279
##    probabilities: 0.016 0.051 0.888 0.001 0.043 
##   left son=382 (89 obs) right son=383 (6357 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=114.72730, (0 missing)
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=108.88810, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 84.40884, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve= 72.69765, (0 missing)
##       Age                                                         < 17.5 to the right, improve= 57.89279, (0 missing)
## 
## Node number 254: 350 observations
##   predicted class=Employed            expected loss=0.07714286  P(node) =0.003317127
##     class counts:     0   323     0     8    19
##    probabilities: 0.000 0.923 0.000 0.023 0.054 
## 
## Node number 255: 18149 observations,    complexity param=0.006624029
##   predicted class=Retired             expected loss=0.1546091  P(node) =0.1720072
##     class counts:   981  1260   497 15343    68
##    probabilities: 0.054 0.069 0.027 0.845 0.004 
##   left son=510 (325 obs) right son=511 (17824 obs)
##   Primary splits:
##       Industry.my.fctrLeisure and hospitality                     < 0.5  to the right, improve=462.7893, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=370.3579, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=324.1177, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=322.1780, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=319.2237, (0 missing)
## 
## Node number 318: 3949 observations
##   predicted class=Employed            expected loss=0.03925044  P(node) =0.03742667
##     class counts:     0  3794    12     2   141
##    probabilities: 0.000 0.961 0.003 0.001 0.036 
## 
## Node number 319: 31933 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.5452353  P(node) =0.3026452
##     class counts:  4581 14522  8643  3143  1044
##    probabilities: 0.143 0.455 0.271 0.098 0.033 
##   left son=638 (4073 obs) right son=639 (27860 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=1482.8070, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=1177.6570, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=1133.7060, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve= 994.4203, (0 missing)
##       Age                                          < 53.5 to the left,  improve= 854.2872, (0 missing)
## 
## Node number 382: 89 observations
##   predicted class=Employed            expected loss=0.1797753  P(node) =0.000843498
##     class counts:     0    73     6     0    10
##    probabilities: 0.000 0.820 0.067 0.000 0.112 
## 
## Node number 383: 6357 observations,    complexity param=0.00139333
##   predicted class=Not.in.Labor.Force  expected loss=0.1000472  P(node) =0.0602485
##     class counts:   105   254  5721     8   269
##    probabilities: 0.017 0.040 0.900 0.001 0.042 
##   left son=766 (89 obs) right son=767 (6268 obs)
##   Primary splits:
##       Industry.my.fctrManufacturing                               < 0.5  to the right, improve=112.03640, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 86.98183, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve= 74.58660, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve= 59.02914, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 57.30520, (0 missing)
## 
## Node number 510: 325 observations
##   predicted class=Employed            expected loss=0.08615385  P(node) =0.003080189
##     class counts:     1   297     0     7    20
##    probabilities: 0.003 0.914 0.000 0.022 0.062 
## 
## Node number 511: 17824 observations,    complexity param=0.005299223
##   predicted class=Retired             expected loss=0.1395871  P(node) =0.1689271
##     class counts:   980   963   497 15336    48
##    probabilities: 0.055 0.054 0.028 0.860 0.003 
##   left son=1022 (243 obs) right son=1023 (17581 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=383.5801, (0 missing)
##       Industry.my.fctrConstruction                                < 0.5  to the right, improve=336.0193, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=334.0430, (0 missing)
##       Industry.my.fctrPublic administration                       < 0.5  to the right, improve=330.8698, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=137.2679, (0 missing)
## 
## Node number 638: 4073 observations
##   predicted class=Employed            expected loss=0.08102136  P(node) =0.03860188
##     class counts:     2  3743    19     9   300
##    probabilities: 0.000 0.919 0.005 0.002 0.074 
## 
## Node number 639: 27860 observations,    complexity param=0.008910715
##   predicted class=Employed            expected loss=0.6131012  P(node) =0.2640433
##     class counts:  4579 10779  8624  3134   744
##    probabilities: 0.164 0.387 0.310 0.112 0.027 
##   left son=1278 (2949 obs) right son=1279 (24911 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=1543.6710, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=1497.3620, (0 missing)
##       Industry.my.fctrOther services               < 0.5  to the right, improve=1317.7560, (0 missing)
##       Age                                          < 50.5 to the left,  improve= 939.5404, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve= 531.7269, (0 missing)
## 
## Node number 766: 89 observations
##   predicted class=Employed            expected loss=0.2696629  P(node) =0.000843498
##     class counts:     0    65     4     0    20
##    probabilities: 0.000 0.730 0.045 0.000 0.225 
## 
## Node number 767: 6268 observations,    complexity param=0.001096391
##   predicted class=Not.in.Labor.Force  expected loss=0.08790683  P(node) =0.059405
##     class counts:   105   189  5717     8   249
##    probabilities: 0.017 0.030 0.912 0.001 0.040 
##   left son=1534 (76 obs) right son=1535 (6192 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=89.59828, (0 missing)
##       Industry.my.fctrFinancial                                   < 0.5  to the right, improve=76.47611, (0 missing)
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=60.57117, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=58.68684, (0 missing)
##       Age                                                         < 17.5 to the left,  improve=26.06129, (0 missing)
## 
## Node number 1022: 243 observations
##   predicted class=Employed            expected loss=0.03292181  P(node) =0.002303034
##     class counts:     0   235     0     3     5
##    probabilities: 0.000 0.967 0.000 0.012 0.021 
## 
## Node number 1023: 17581 observations,    complexity param=0.004659662
##   predicted class=Retired             expected loss=0.1278653  P(node) =0.166624
##     class counts:   980   728   497 15333    43
##    probabilities: 0.056 0.041 0.028 0.872 0.002 
##   left son=2046 (225 obs) right son=2047 (17356 obs)
##   Primary splits:
##       Industry.my.fctrConstruction                 < 0.5  to the right, improve=345.6840, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=343.6840, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=340.3347, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=141.2113, (0 missing)
##       Age                                          < 69.5 to the left,  improve=115.1508, (0 missing)
## 
## Node number 1278: 2949 observations
##   predicted class=Employed            expected loss=0.0342489  P(node) =0.02794916
##     class counts:     4  2848     7     8    82
##    probabilities: 0.001 0.966 0.002 0.003 0.028 
## 
## Node number 1279: 24911 observations,    complexity param=0.008910715
##   predicted class=Not.in.Labor.Force  expected loss=0.6540886  P(node) =0.2360941
##     class counts:  4575  7931  8617  3126   662
##    probabilities: 0.184 0.318 0.346 0.125 0.027 
##   left son=2558 (2995 obs) right son=2559 (21916 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities                < 0.5  to the right, improve=1909.4590, (0 missing)
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=1683.7930, (0 missing)
##       Age                                                         < 50.5 to the left,  improve=1038.2920, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 675.5507, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 564.8951, (0 missing)
## 
## Node number 1534: 76 observations
##   predicted class=Employed            expected loss=0.2631579  P(node) =0.0007202904
##     class counts:     0    56     8     0    12
##    probabilities: 0.000 0.737 0.105 0.000 0.158 
## 
## Node number 1535: 6192 observations,    complexity param=0.0009821836
##   predicted class=Not.in.Labor.Force  expected loss=0.07800388  P(node) =0.05868471
##     class counts:   105   133  5709     8   237
##    probabilities: 0.017 0.021 0.922 0.001 0.038 
##   left son=3070 (48 obs) right son=3071 (6144 obs)
##   Primary splits:
##       Industry.my.fctrFinancial                    < 0.5  to the right, improve=78.09325, (0 missing)
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=61.88774, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=59.87133, (0 missing)
##       Age                                          < 17.5 to the left,  improve=21.60476, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=20.63575, (0 missing)
## 
## Node number 2046: 225 observations
##   predicted class=Employed            expected loss=0.08  P(node) =0.002132439
##     class counts:     0   207     1     3    14
##    probabilities: 0.000 0.920 0.004 0.013 0.062 
## 
## Node number 2047: 17356 observations,    complexity param=0.00463682
##   predicted class=Retired             expected loss=0.116732  P(node) =0.1644916
##     class counts:   980   521   496 15330    29
##    probabilities: 0.056 0.030 0.029 0.883 0.002 
##   left son=4094 (225 obs) right son=4095 (17131 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=352.73740, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=349.21860, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=144.91390, (0 missing)
##       Age                                          < 69.5 to the left,  improve= 93.27941, (0 missing)
##       Industry.my.fctrMining                       < 0.5  to the right, improve= 36.16099, (0 missing)
## 
## Node number 2558: 2995 observations
##   predicted class=Employed            expected loss=0.05409015  P(node) =0.02838513
##     class counts:     3  2833     8     4   147
##    probabilities: 0.001 0.946 0.003 0.001 0.049 
## 
## Node number 2559: 21916 observations,    complexity param=0.008910715
##   predicted class=Not.in.Labor.Force  expected loss=0.607182  P(node) =0.207709
##     class counts:  4572  5098  8609  3122   515
##    probabilities: 0.209 0.233 0.393 0.142 0.023 
##   left son=5118 (2748 obs) right son=5119 (19168 obs)
##   Primary splits:
##       Industry.my.fctrOther services                              < 0.5  to the right, improve=2220.5360, (0 missing)
##       Age                                                         < 50.5 to the left,  improve=1171.3010, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve= 883.7817, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 735.8725, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 388.6810, (0 missing)
## 
## Node number 3070: 48 observations
##   predicted class=Employed            expected loss=0.08333333  P(node) =0.0004549202
##     class counts:     0    44     1     0     3
##    probabilities: 0.000 0.917 0.021 0.000 0.063 
## 
## Node number 3071: 6144 observations,    complexity param=0.0007766103
##   predicted class=Not.in.Labor.Force  expected loss=0.07096354  P(node) =0.05822979
##     class counts:   105    89  5708     8   234
##    probabilities: 0.017 0.014 0.929 0.001 0.038 
##   left son=6142 (40 obs) right son=6143 (6104 obs)
##   Primary splits:
##       Industry.my.fctrTransportation and utilities < 0.5  to the right, improve=62.88517, (0 missing)
##       Industry.my.fctrInformation                  < 0.5  to the right, improve=60.77505, (0 missing)
##       Industry.my.fctrPublic administration        < 0.5  to the right, improve=21.02117, (0 missing)
##       Industry.my.fctrMining                       < 0.5  to the left,  improve=15.99500, (0 missing)
##       Age                                          < 17.5 to the left,  improve=14.80758, (0 missing)
## 
## Node number 4094: 225 observations
##   predicted class=Employed            expected loss=0.07555556  P(node) =0.002132439
##     class counts:     0   208     1     5    11
##    probabilities: 0.000 0.924 0.004 0.022 0.049 
## 
## Node number 4095: 17131 observations,    complexity param=0.004591138
##   predicted class=Retired             expected loss=0.1054229  P(node) =0.1623591
##     class counts:   980   313   495 15325    18
##    probabilities: 0.057 0.018 0.029 0.895 0.001 
##   left son=8190 (219 obs) right son=8191 (16912 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=358.43180, (0 missing)
##       Industry.my.fctrInformation           < 0.5  to the right, improve=148.75290, (0 missing)
##       Age                                   < 68.5 to the left,  improve= 73.21838, (0 missing)
##       Industry.my.fctrMining                < 0.5  to the right, improve= 37.16997, (0 missing)
## 
## Node number 5118: 2748 observations
##   predicted class=Employed            expected loss=0.06550218  P(node) =0.02604418
##     class counts:     2  2568    17     1   160
##    probabilities: 0.001 0.934 0.006 0.000 0.058 
## 
## Node number 5119: 19168 observations,    complexity param=0.008910715
##   predicted class=Not.in.Labor.Force  expected loss=0.5517529  P(node) =0.1816648
##     class counts:  4570  2530  8592  3121   355
##    probabilities: 0.238 0.132 0.448 0.163 0.019 
##   left son=10238 (7944 obs) right son=10239 (11224 obs)
##   Primary splits:
##       Age                                                         < 49.5 to the right, improve=1339.92900, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=1167.34600, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve= 967.76410, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve= 506.78100, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve=  30.53093, (0 missing)
## 
## Node number 6142: 40 observations
##   predicted class=Employed            expected loss=0.125  P(node) =0.0003791002
##     class counts:     0    35     1     0     4
##    probabilities: 0.000 0.875 0.025 0.000 0.100 
## 
## Node number 6143: 6104 observations,    complexity param=0.0007537688
##   predicted class=Not.in.Labor.Force  expected loss=0.06503932  P(node) =0.05785069
##     class counts:   105    54  5707     8   230
##    probabilities: 0.017 0.009 0.935 0.001 0.038 
##   left son=12286 (34 obs) right son=12287 (6070 obs)
##   Primary splits:
##       Industry.my.fctrInformation           < 0.5  to the right, improve=61.524490, (0 missing)
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=21.341600, (0 missing)
##       Industry.my.fctrMining                < 0.5  to the left,  improve=16.209240, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 9.827868, (0 missing)
## 
## Node number 8190: 219 observations
##   predicted class=Employed            expected loss=0.06392694  P(node) =0.002075574
##     class counts:     0   205     1     4     9
##    probabilities: 0.000 0.936 0.005 0.018 0.041 
## 
## Node number 8191: 16912 observations,    complexity param=0.001918684
##   predicted class=Retired             expected loss=0.09407521  P(node) =0.1602836
##     class counts:   980   108   494 15321     9
##    probabilities: 0.058 0.006 0.029 0.906 0.001 
##   left son=16382 (93 obs) right son=16383 (16819 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=152.67140, (0 missing)
##       Age                         < 66.5 to the left,  improve= 57.53166, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve= 38.20025, (0 missing)
## 
## Node number 10238: 7944 observations,    complexity param=0.008910715
##   predicted class=Retired             expected loss=0.6570997  P(node) =0.0752893
##     class counts:  2647   853  1671  2724    49
##    probabilities: 0.333 0.107 0.210 0.343 0.006 
##   left son=20476 (410 obs) right son=20477 (7534 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=428.8616, (0 missing)
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=329.0827, (0 missing)
##       Age                                                         < 59.5 to the left,  improve=280.2615, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=160.4271, (0 missing)
## 
## Node number 10239: 11224 observations,    complexity param=0.008910715
##   predicted class=Not.in.Labor.Force  expected loss=0.3833749  P(node) =0.1063755
##     class counts:  1923  1677  6921   397   306
##    probabilities: 0.171 0.149 0.617 0.035 0.027 
##   left son=20478 (870 obs) right son=20479 (10354 obs)
##   Primary splits:
##       Industry.my.fctrInformation                                 < 0.5  to the right, improve=946.83090, (0 missing)
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=610.14300, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=386.43570, (0 missing)
##       Age                                                         < 37.5 to the right, improve=187.39810, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 30.97126, (0 missing)
## 
## Node number 12286: 34 observations
##   predicted class=Employed            expected loss=0.02941176  P(node) =0.0003222352
##     class counts:     0    33     0     0     1
##    probabilities: 0.000 0.971 0.000 0.000 0.029 
## 
## Node number 12287: 6070 observations,    complexity param=0.0002512563
##   predicted class=Not.in.Labor.Force  expected loss=0.05980231  P(node) =0.05752846
##     class counts:   105    21  5707     8   229
##    probabilities: 0.017 0.003 0.940 0.001 0.038 
##   left son=24574 (18 obs) right son=24575 (6052 obs)
##   Primary splits:
##       Industry.my.fctrPublic administration < 0.5  to the right, improve=21.637840, (0 missing)
##       Industry.my.fctrMining                < 0.5  to the left,  improve=16.401890, (0 missing)
##       Age                                   < 17.5 to the left,  improve= 7.415681, (0 missing)
## 
## Node number 16382: 93 observations
##   predicted class=Employed            expected loss=0.07526882  P(node) =0.000881408
##     class counts:     0    86     0     2     5
##    probabilities: 0.000 0.925 0.000 0.022 0.054 
## 
## Node number 16383: 16819 observations,    complexity param=0.0001713111
##   predicted class=Retired             expected loss=0.08918485  P(node) =0.1594022
##     class counts:   980    22   494 15319     4
##    probabilities: 0.058 0.001 0.029 0.911 0.000 
##   left son=32766 (2744 obs) right son=32767 (14075 obs)
##   Primary splits:
##       Age                    < 66.5 to the left,  improve=52.53577, (0 missing)
##       Industry.my.fctrMining < 0.5  to the right, improve=38.64696, (0 missing)
## 
## Node number 20476: 410 observations
##   predicted class=Employed            expected loss=0.04146341  P(node) =0.003885777
##     class counts:     1   393     1     2    13
##    probabilities: 0.002 0.959 0.002 0.005 0.032 
## 
## Node number 20477: 7534 observations,    complexity param=0.008910715
##   predicted class=Retired             expected loss=0.6387045  P(node) =0.07140352
##     class counts:  2646   460  1670  2722    36
##    probabilities: 0.351 0.061 0.222 0.361 0.005 
##   left son=40954 (331 obs) right son=40955 (7203 obs)
##   Primary splits:
##       Industry.my.fctrInformation < 0.5  to the right, improve=367.4837, (0 missing)
##       Age                         < 59.5 to the left,  improve=288.8698, (0 missing)
##       Industry.my.fctrMining      < 0.5  to the right, improve=178.5305, (0 missing)
## 
## Node number 20478: 870 observations
##   predicted class=Employed            expected loss=0.07701149  P(node) =0.008245429
##     class counts:     0   803     5     1    61
##    probabilities: 0.000 0.923 0.006 0.001 0.070 
## 
## Node number 20479: 10354 observations,    complexity param=0.008910715
##   predicted class=Not.in.Labor.Force  expected loss=0.3320456  P(node) =0.09813009
##     class counts:  1923   874  6916   396   245
##    probabilities: 0.186 0.084 0.668 0.038 0.024 
##   left son=40958 (578 obs) right son=40959 (9776 obs)
##   Primary splits:
##       Industry.my.fctrAgriculture, forestry, fishing, and hunting < 0.5  to the right, improve=720.41770, (0 missing)
##       Industry.my.fctrMining                                      < 0.5  to the right, improve=453.50210, (0 missing)
##       Age                                                         < 40.5 to the right, improve=193.70940, (0 missing)
##       Industry.my.fctrArmed forces                                < 0.5  to the left,  improve= 32.49911, (0 missing)
## 
## Node number 24574: 18 observations
##   predicted class=Employed            expected loss=0.2222222  P(node) =0.0001705951
##     class counts:     0    14     3     0     1
##    probabilities: 0.000 0.778 0.167 0.000 0.056 
## 
## Node number 24575: 6052 observations,    complexity param=0.0001598904
##   predicted class=Not.in.Labor.Force  expected loss=0.05750165  P(node) =0.05735786
##     class counts:   105     7  5704     8   228
##    probabilities: 0.017 0.001 0.942 0.001 0.038 
##   left son=49150 (6040 obs) right son=49151 (12 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the left,  improve=16.486750, (0 missing)
##       Age                    < 17.5 to the left,  improve= 6.424928, (0 missing)
## 
## Node number 32766: 2744 observations,    complexity param=0.0001370489
##   predicted class=Retired             expected loss=0.1909621  P(node) =0.02600627
##     class counts:   346     7   170  2220     1
##    probabilities: 0.126 0.003 0.062 0.809 0.000 
##   left son=65532 (9 obs) right son=65533 (2735 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=9.990317, (0 missing)
##       Age                    < 64.5 to the left,  improve=8.016844, (0 missing)
## 
## Node number 32767: 14075 observations,    complexity param=0.0001713111
##   predicted class=Retired             expected loss=0.06934281  P(node) =0.1333959
##     class counts:   634    15   324 13099     3
##    probabilities: 0.045 0.001 0.023 0.931 0.000 
##   left son=65534 (17 obs) right son=65535 (14058 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=28.239480, (0 missing)
##       Age                    < 70.5 to the left,  improve= 7.452007, (0 missing)
## 
## Node number 40954: 331 observations
##   predicted class=Employed            expected loss=0.0694864  P(node) =0.003137054
##     class counts:     0   308     1     0    22
##    probabilities: 0.000 0.931 0.003 0.000 0.066 
## 
## Node number 40955: 7203 observations,    complexity param=0.008910715
##   predicted class=Retired             expected loss=0.6221019  P(node) =0.06826647
##     class counts:  2646   152  1669  2722    14
##    probabilities: 0.367 0.021 0.232 0.378 0.002 
##   left son=81910 (4514 obs) right son=81911 (2689 obs)
##   Primary splits:
##       Age                    < 59.5 to the left,  improve=291.1724, (0 missing)
##       Industry.my.fctrMining < 0.5  to the right, improve=195.1395, (0 missing)
## 
## Node number 40958: 578 observations
##   predicted class=Employed            expected loss=0.07439446  P(node) =0.005477998
##     class counts:     1   535     6     0    36
##    probabilities: 0.002 0.926 0.010 0.000 0.062 
## 
## Node number 40959: 9776 observations,    complexity param=0.007697579
##   predicted class=Not.in.Labor.Force  expected loss=0.2931669  P(node) =0.09265209
##     class counts:  1922   339  6910   396   209
##    probabilities: 0.197 0.035 0.707 0.041 0.021 
##   left son=81918 (354 obs) right son=81919 (9422 obs)
##   Primary splits:
##       Industry.my.fctrMining       < 0.5  to the right, improve=508.35280, (0 missing)
##       Age                          < 40.5 to the right, improve=198.32880, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 33.88074, (0 missing)
## 
## Node number 49150: 6040 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.05562914  P(node) =0.05724413
##     class counts:   105     0  5704     8   223
##    probabilities: 0.017 0.000 0.944 0.001 0.037 
## 
## Node number 49151: 12 observations
##   predicted class=Employed            expected loss=0.4166667  P(node) =0.0001137301
##     class counts:     0     7     0     0     5
##    probabilities: 0.000 0.583 0.000 0.000 0.417 
## 
## Node number 65532: 9 observations
##   predicted class=Employed            expected loss=0.2222222  P(node) =8.529755e-05
##     class counts:     0     7     1     1     0
##    probabilities: 0.000 0.778 0.111 0.111 0.000 
## 
## Node number 65533: 2735 observations
##   predicted class=Retired             expected loss=0.1886654  P(node) =0.02592098
##     class counts:   346     0   169  2219     1
##    probabilities: 0.127 0.000 0.062 0.811 0.000 
## 
## Node number 65534: 17 observations
##   predicted class=Employed            expected loss=0.1176471  P(node) =0.0001611176
##     class counts:     0    15     0     0     2
##    probabilities: 0.000 0.882 0.000 0.000 0.118 
## 
## Node number 65535: 14058 observations
##   predicted class=Retired             expected loss=0.06821739  P(node) =0.1332348
##     class counts:   634     0   324 13099     1
##    probabilities: 0.045 0.000 0.023 0.932 0.000 
## 
## Node number 81910: 4514 observations,    complexity param=0.002649612
##   predicted class=Disabled            expected loss=0.5750997  P(node) =0.04278146
##     class counts:  1918   117  1342  1127    10
##    probabilities: 0.425 0.026 0.297 0.250 0.002 
##   left son=163820 (123 obs) right son=163821 (4391 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=148.8923, (0 missing)
##       Age                    < 54.5 to the left,  improve= 43.2640, (0 missing)
## 
## Node number 81911: 2689 observations,    complexity param=0.0007994518
##   predicted class=Retired             expected loss=0.4068427  P(node) =0.02548501
##     class counts:   728    35   327  1595     4
##    probabilities: 0.271 0.013 0.122 0.593 0.001 
##   left son=163822 (35 obs) right son=163823 (2654 obs)
##   Primary splits:
##       Industry.my.fctrMining < 0.5  to the right, improve=50.14477, (0 missing)
##       Age                    < 61.5 to the left,  improve=21.72335, (0 missing)
## 
## Node number 81918: 354 observations
##   predicted class=Employed            expected loss=0.04237288  P(node) =0.003355037
##     class counts:     0   339     2     0    13
##    probabilities: 0.000 0.958 0.006 0.000 0.037 
## 
## Node number 81919: 9422 observations,    complexity param=0.0002055733
##   predicted class=Not.in.Labor.Force  expected loss=0.2668223  P(node) =0.08929705
##     class counts:  1922     0  6908   396   196
##    probabilities: 0.204 0.000 0.733 0.042 0.021 
##   left son=163838 (3482 obs) right son=163839 (5940 obs)
##   Primary splits:
##       Age                          < 37.5 to the right, improve=200.26230, (0 missing)
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve= 34.89351, (0 missing)
## 
## Node number 163820: 123 observations
##   predicted class=Employed            expected loss=0.04878049  P(node) =0.001165733
##     class counts:     1   117     0     1     4
##    probabilities: 0.008 0.951 0.000 0.008 0.033 
## 
## Node number 163821: 4391 observations
##   predicted class=Disabled            expected loss=0.5634252  P(node) =0.04161573
##     class counts:  1917     0  1342  1126     6
##    probabilities: 0.437 0.000 0.306 0.256 0.001 
## 
## Node number 163822: 35 observations
##   predicted class=Employed            expected loss=0  P(node) =0.0003317127
##     class counts:     0    35     0     0     0
##    probabilities: 0.000 1.000 0.000 0.000 0.000 
## 
## Node number 163823: 2654 observations
##   predicted class=Retired             expected loss=0.3990203  P(node) =0.0251533
##     class counts:   728     0   327  1595     4
##    probabilities: 0.274 0.000 0.123 0.601 0.002 
## 
## Node number 163838: 3482 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.4029294  P(node) =0.03300067
##     class counts:  1167     0  2079   211    25
##    probabilities: 0.335 0.000 0.597 0.061 0.007 
## 
## Node number 163839: 5940 observations,    complexity param=0.0002055733
##   predicted class=Not.in.Labor.Force  expected loss=0.187037  P(node) =0.05629638
##     class counts:   755     0  4829   185   171
##    probabilities: 0.127 0.000 0.813 0.031 0.029 
##   left son=327678 (5920 obs) right son=327679 (20 obs)
##   Primary splits:
##       Industry.my.fctrArmed forces < 0.5  to the left,  improve=29.05522, (0 missing)
##       Age                          < 27.5 to the right, improve=22.60554, (0 missing)
## 
## Node number 327678: 5920 observations
##   predicted class=Not.in.Labor.Force  expected loss=0.1844595  P(node) =0.05610683
##     class counts:   755     0  4828   185   152
##    probabilities: 0.128 0.000 0.816 0.031 0.026 
## 
## Node number 327679: 20 observations
##   predicted class=Unemployed          expected loss=0.05  P(node) =0.0001895501
##     class counts:     0     0     1     0    19
##    probabilities: 0.000 0.000 0.050 0.000 0.950 
## 
## n= 105513 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 105513 43780 Employed (0.054 0.59 0.14 0.18 0.04)  
##        2) Age< 63.5 83904 26650 Employed (0.056 0.68 0.18 0.038 0.048)  
##          4) Age>=19.5 75489 20206 Employed (0.061 0.73 0.12 0.042 0.047)  
##            8) Industry.my.fctrEducational and health services>=0.5 13669   684 Employed (0.00095 0.95 0.0048 0.0013 0.043) *
##            9) Industry.my.fctrEducational and health services< 0.5 61820 19522 Employed (0.075 0.68 0.14 0.051 0.047)  
##             18) Industry.my.fctrTrade>=0.5 7743   578 Employed (0.0013 0.93 0.0065 0.00077 0.066) *
##             19) Industry.my.fctrTrade< 0.5 54077 18944 Employed (0.085 0.65 0.16 0.059 0.045)  
##               38) Industry.my.fctrProfessional and business services>=0.5 6767   493 Employed (0.001 0.93 0.0052 0.00044 0.066) *
##               39) Industry.my.fctrProfessional and business services< 0.5 47310 18451 Employed (0.097 0.61 0.18 0.067 0.042)  
##                 78) Industry.my.fctrManufacturing>=0.5 6319   422 Employed (0.0013 0.93 0.0033 0.0016 0.061) *
##                 79) Industry.my.fctrManufacturing< 0.5 40991 18029 Employed (0.11 0.56 0.21 0.077 0.039)  
##                  158) Industry.my.fctrLeisure and hospitality>=0.5 5109   463 Employed (0.00059 0.91 0.011 0.0012 0.078) *
##                  159) Industry.my.fctrLeisure and hospitality< 0.5 35882 17566 Employed (0.13 0.51 0.24 0.088 0.033)  
##                    318) Industry.my.fctrFinancial>=0.5 3949   155 Employed (0 0.96 0.003 0.00051 0.036) *
##                    319) Industry.my.fctrFinancial< 0.5 31933 17411 Employed (0.14 0.45 0.27 0.098 0.033)  
##                      638) Industry.my.fctrConstruction>=0.5 4073   330 Employed (0.00049 0.92 0.0047 0.0022 0.074) *
##                      639) Industry.my.fctrConstruction< 0.5 27860 17081 Employed (0.16 0.39 0.31 0.11 0.027)  
##                       1278) Industry.my.fctrPublic administration>=0.5 2949   101 Employed (0.0014 0.97 0.0024 0.0027 0.028) *
##                       1279) Industry.my.fctrPublic administration< 0.5 24911 16294 Not.in.Labor.Force (0.18 0.32 0.35 0.13 0.027)  
##                         2558) Industry.my.fctrTransportation and utilities>=0.5 2995   162 Employed (0.001 0.95 0.0027 0.0013 0.049) *
##                         2559) Industry.my.fctrTransportation and utilities< 0.5 21916 13307 Not.in.Labor.Force (0.21 0.23 0.39 0.14 0.023)  
##                           5118) Industry.my.fctrOther services>=0.5 2748   180 Employed (0.00073 0.93 0.0062 0.00036 0.058) *
##                           5119) Industry.my.fctrOther services< 0.5 19168 10576 Not.in.Labor.Force (0.24 0.13 0.45 0.16 0.019)  
##                            10238) Age>=49.5 7944  5220 Retired (0.33 0.11 0.21 0.34 0.0062)  
##                              20476) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 410    17 Employed (0.0024 0.96 0.0024 0.0049 0.032) *
##                              20477) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 7534  4812 Retired (0.35 0.061 0.22 0.36 0.0048)  
##                                40954) Industry.my.fctrInformation>=0.5 331    23 Employed (0 0.93 0.003 0 0.066) *
##                                40955) Industry.my.fctrInformation< 0.5 7203  4481 Retired (0.37 0.021 0.23 0.38 0.0019)  
##                                  81910) Age< 59.5 4514  2596 Disabled (0.42 0.026 0.3 0.25 0.0022)  
##                                   163820) Industry.my.fctrMining>=0.5 123     6 Employed (0.0081 0.95 0 0.0081 0.033) *
##                                   163821) Industry.my.fctrMining< 0.5 4391  2474 Disabled (0.44 0 0.31 0.26 0.0014) *
##                                  81911) Age>=59.5 2689  1094 Retired (0.27 0.013 0.12 0.59 0.0015)  
##                                   163822) Industry.my.fctrMining>=0.5 35     0 Employed (0 1 0 0 0) *
##                                   163823) Industry.my.fctrMining< 0.5 2654  1059 Retired (0.27 0 0.12 0.6 0.0015) *
##                            10239) Age< 49.5 11224  4303 Not.in.Labor.Force (0.17 0.15 0.62 0.035 0.027)  
##                              20478) Industry.my.fctrInformation>=0.5 870    67 Employed (0 0.92 0.0057 0.0011 0.07) *
##                              20479) Industry.my.fctrInformation< 0.5 10354  3438 Not.in.Labor.Force (0.19 0.084 0.67 0.038 0.024)  
##                                40958) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 578    43 Employed (0.0017 0.93 0.01 0 0.062) *
##                                40959) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 9776  2866 Not.in.Labor.Force (0.2 0.035 0.71 0.041 0.021)  
##                                  81918) Industry.my.fctrMining>=0.5 354    15 Employed (0 0.96 0.0056 0 0.037) *
##                                  81919) Industry.my.fctrMining< 0.5 9422  2514 Not.in.Labor.Force (0.2 0 0.73 0.042 0.021)  
##                                   163838) Age>=37.5 3482  1403 Not.in.Labor.Force (0.34 0 0.6 0.061 0.0072) *
##                                   163839) Age< 37.5 5940  1111 Not.in.Labor.Force (0.13 0 0.81 0.031 0.029)  
##                                     327678) Industry.my.fctrArmed forces< 0.5 5920  1092 Not.in.Labor.Force (0.13 0 0.82 0.031 0.026) *
##                                     327679) Industry.my.fctrArmed forces>=0.5 20     1 Unemployed (0 0 0.05 0 0.95) *
##          5) Age< 19.5 8415  2554 Not.in.Labor.Force (0.012 0.23 0.7 0.00095 0.056)  
##           10) Industry.my.fctrLeisure and hospitality>=0.5 930   136 Employed (0 0.85 0.063 0 0.083) *
##           11) Industry.my.fctrLeisure and hospitality< 0.5 7485  1683 Not.in.Labor.Force (0.014 0.16 0.78 0.0011 0.053)  
##             22) Industry.my.fctrTrade>=0.5 543    74 Employed (0 0.86 0.037 0 0.099) *
##             23) Industry.my.fctrTrade< 0.5 6942  1160 Not.in.Labor.Force (0.015 0.1 0.83 0.0012 0.049)  
##               46) Industry.my.fctrEducational and health services>=0.5 244    51 Employed (0 0.79 0.12 0 0.09) *
##               47) Industry.my.fctrEducational and health services< 0.5 6698   945 Not.in.Labor.Force (0.016 0.077 0.86 0.0012 0.047)  
##                 94) Industry.my.fctrOther services>=0.5 124    26 Employed (0 0.79 0.081 0 0.13) *
##                 95) Industry.my.fctrOther services< 0.5 6574   831 Not.in.Labor.Force (0.016 0.063 0.87 0.0012 0.046)  
##                  190) Industry.my.fctrProfessional and business services>=0.5 128    38 Employed (0 0.7 0.13 0 0.17) *
##                  191) Industry.my.fctrProfessional and business services< 0.5 6446   719 Not.in.Labor.Force (0.016 0.051 0.89 0.0012 0.043)  
##                    382) Industry.my.fctrConstruction>=0.5 89    16 Employed (0 0.82 0.067 0 0.11) *
##                    383) Industry.my.fctrConstruction< 0.5 6357   636 Not.in.Labor.Force (0.017 0.04 0.9 0.0013 0.042)  
##                      766) Industry.my.fctrManufacturing>=0.5 89    24 Employed (0 0.73 0.045 0 0.22) *
##                      767) Industry.my.fctrManufacturing< 0.5 6268   551 Not.in.Labor.Force (0.017 0.03 0.91 0.0013 0.04)  
##                       1534) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 76    20 Employed (0 0.74 0.11 0 0.16) *
##                       1535) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 6192   483 Not.in.Labor.Force (0.017 0.021 0.92 0.0013 0.038)  
##                         3070) Industry.my.fctrFinancial>=0.5 48     4 Employed (0 0.92 0.021 0 0.063) *
##                         3071) Industry.my.fctrFinancial< 0.5 6144   436 Not.in.Labor.Force (0.017 0.014 0.93 0.0013 0.038)  
##                           6142) Industry.my.fctrTransportation and utilities>=0.5 40     5 Employed (0 0.88 0.025 0 0.1) *
##                           6143) Industry.my.fctrTransportation and utilities< 0.5 6104   397 Not.in.Labor.Force (0.017 0.0088 0.93 0.0013 0.038)  
##                            12286) Industry.my.fctrInformation>=0.5 34     1 Employed (0 0.97 0 0 0.029) *
##                            12287) Industry.my.fctrInformation< 0.5 6070   363 Not.in.Labor.Force (0.017 0.0035 0.94 0.0013 0.038)  
##                              24574) Industry.my.fctrPublic administration>=0.5 18     4 Employed (0 0.78 0.17 0 0.056) *
##                              24575) Industry.my.fctrPublic administration< 0.5 6052   348 Not.in.Labor.Force (0.017 0.0012 0.94 0.0013 0.038)  
##                                49150) Industry.my.fctrMining< 0.5 6040   336 Not.in.Labor.Force (0.017 0 0.94 0.0013 0.037) *
##                                49151) Industry.my.fctrMining>=0.5 12     5 Employed (0 0.58 0 0 0.42) *
##        3) Age>=63.5 21609  6186 Retired (0.046 0.21 0.023 0.71 0.01)  
##          6) Industry.my.fctrEducational and health services>=0.5 1104    78 Employed (0 0.93 0 0.028 0.043) *
##          7) Industry.my.fctrEducational and health services< 0.5 20505  5113 Retired (0.048 0.17 0.025 0.75 0.0083)  
##           14) Industry.my.fctrTrade>=0.5 647    38 Employed (0.0046 0.94 0.0015 0.025 0.028) *
##           15) Industry.my.fctrTrade< 0.5 19858  4482 Retired (0.049 0.14 0.025 0.77 0.0077)  
##             30) Industry.my.fctrProfessional and business services>=0.5 624    51 Employed (0.0016 0.92 0.0064 0.021 0.053) *
##             31) Industry.my.fctrProfessional and business services< 0.5 19234  3871 Retired (0.051 0.12 0.026 0.8 0.0062)  
##               62) Industry.my.fctrManufacturing>=0.5 383    26 Employed (0 0.93 0.0052 0.01 0.052) *
##               63) Industry.my.fctrManufacturing< 0.5 18851  3492 Retired (0.052 0.1 0.026 0.81 0.0053)  
##                126) Industry.my.fctrOther services>=0.5 352    21 Employed (0 0.94 0.0028 0.023 0.034) *
##                127) Industry.my.fctrOther services< 0.5 18499  3148 Retired (0.053 0.086 0.027 0.83 0.0047)  
##                  254) Industry.my.fctrFinancial>=0.5 350    27 Employed (0 0.92 0 0.023 0.054) *
##                  255) Industry.my.fctrFinancial< 0.5 18149  2806 Retired (0.054 0.069 0.027 0.85 0.0037)  
##                    510) Industry.my.fctrLeisure and hospitality>=0.5 325    28 Employed (0.0031 0.91 0 0.022 0.062) *
##                    511) Industry.my.fctrLeisure and hospitality< 0.5 17824  2488 Retired (0.055 0.054 0.028 0.86 0.0027)  
##                     1022) Industry.my.fctrAgriculture, forestry, fishing, and hunting>=0.5 243     8 Employed (0 0.97 0 0.012 0.021) *
##                     1023) Industry.my.fctrAgriculture, forestry, fishing, and hunting< 0.5 17581  2248 Retired (0.056 0.041 0.028 0.87 0.0024)  
##                       2046) Industry.my.fctrConstruction>=0.5 225    18 Employed (0 0.92 0.0044 0.013 0.062) *
##                       2047) Industry.my.fctrConstruction< 0.5 17356  2026 Retired (0.056 0.03 0.029 0.88 0.0017)  
##                         4094) Industry.my.fctrTransportation and utilities>=0.5 225    17 Employed (0 0.92 0.0044 0.022 0.049) *
##                         4095) Industry.my.fctrTransportation and utilities< 0.5 17131  1806 Retired (0.057 0.018 0.029 0.89 0.0011)  
##                           8190) Industry.my.fctrPublic administration>=0.5 219    14 Employed (0 0.94 0.0046 0.018 0.041) *
##                           8191) Industry.my.fctrPublic administration< 0.5 16912  1591 Retired (0.058 0.0064 0.029 0.91 0.00053)  
##                            16382) Industry.my.fctrInformation>=0.5 93     7 Employed (0 0.92 0 0.022 0.054) *
##                            16383) Industry.my.fctrInformation< 0.5 16819  1500 Retired (0.058 0.0013 0.029 0.91 0.00024)  
##                              32766) Age< 66.5 2744   524 Retired (0.13 0.0026 0.062 0.81 0.00036)  
##                                65532) Industry.my.fctrMining>=0.5 9     2 Employed (0 0.78 0.11 0.11 0) *
##                                65533) Industry.my.fctrMining< 0.5 2735   516 Retired (0.13 0 0.062 0.81 0.00037) *
##                              32767) Age>=66.5 14075   976 Retired (0.045 0.0011 0.023 0.93 0.00021)  
##                                65534) Industry.my.fctrMining>=0.5 17     2 Employed (0 0.88 0 0 0.12) *
##                                65535) Industry.my.fctrMining< 0.5 14058   959 Retired (0.045 0 0.023 0.93 7.1e-05) *
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

```
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled               1917       60               2027    1708
##   Employed                  0    61733                  0       0
##   Not.in.Labor.Force     1342      472              12611     820
##   Retired                1126      176                404   16913
##   Unemployed                6     3772                400       6
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          1
##   Retired                     0
##   Unemployed                 19
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8832371             NA      0.8812842      0.8851690      0.5850748 
## AccuracyPValue  McnemarPValue 
##      0.0000000      0.0000000
```

```
## Warning in ni[1:m] * nj[1:m]: NAs produced by integer overflow
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](us_cps2_files/figure-html/fit.data.training_0-1.png) 

```
##      model_id model_method                 feats max.nTuningRuns
## 1 Final.rpart        rpart Industry.my.fctr, Age               1
##   min.elapsedtime.everything min.elapsedtime.final max.Accuracy.fit
## 1                     10.643                 5.023        0.8827917
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.8812842              0.885169       0.79671
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001221035     0.002066722
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 281.597 299.115  17.518
## 15 fit.data.training          8          1 299.115      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in `[<-.data.frame`(`*tmp*`, , paste0(rsp_var_out, ".prob"), value
## = structure(list(: provided 5 variables to replace 1 variables
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                                               Max.cor.Y.cv.0.cp.0.rpart.importance
## Age                                                                                     100.000000
## Industry.my.fctrOther services                                                           33.090343
## Industry.my.fctrEducational and health services                                          27.058509
## Industry.my.fctrTransportation and utilities                                             23.012814
## Industry.my.fctrManufacturing                                                            28.370178
## Industry.my.fctrProfessional and business services                                       25.591436
## Industry.my.fctrTrade                                                                    24.329022
## Industry.my.fctrConstruction                                                             22.685481
## Industry.my.fctrAgriculture, forestry, fishing, and hunting                              20.237759
## Industry.my.fctrPublic administration                                                    29.566192
## Industry.my.fctrFinancial                                                                21.645346
## Industry.my.fctrInformation                                                              15.923595
## Industry.my.fctrLeisure and hospitality                                                  22.062655
## Industry.my.fctrMining                                                                   11.722476
## Industry.my.fctrArmed forces                                                              0.784697
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`                             0.000000
## `Industry.my.fctrArmed forces`                                                            0.000000
## `Industry.my.fctrEducational and health services`                                         0.000000
## `Industry.my.fctrLeisure and hospitality`                                                 0.000000
## `Industry.my.fctrOther services`                                                          0.000000
## `Industry.my.fctrProfessional and business services`                                      0.000000
## `Industry.my.fctrPublic administration`                                                   0.000000
## `Industry.my.fctrTransportation and utilities`                                            0.000000
##                                                                importance
## Age                                                           100.0000000
## Industry.my.fctrOther services                                 27.4614104
## Industry.my.fctrEducational and health services                25.2415079
## Industry.my.fctrTransportation and utilities                   23.7839646
## Industry.my.fctrManufacturing                                  23.1388463
## Industry.my.fctrProfessional and business services             21.2185994
## Industry.my.fctrTrade                                          20.1374624
## Industry.my.fctrConstruction                                   20.0204644
## Industry.my.fctrAgriculture, forestry, fishing, and hunting    19.9699985
## Industry.my.fctrPublic administration                          19.9449139
## Industry.my.fctrFinancial                                      19.7491177
## Industry.my.fctrInformation                                    19.2223509
## Industry.my.fctrLeisure and hospitality                        16.1591357
## Industry.my.fctrMining                                         10.4806617
## Industry.my.fctrArmed forces                                    0.6223753
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`   0.0000000
## `Industry.my.fctrArmed forces`                                  0.0000000
## `Industry.my.fctrEducational and health services`               0.0000000
## `Industry.my.fctrLeisure and hospitality`                       0.0000000
## `Industry.my.fctrOther services`                                0.0000000
## `Industry.my.fctrProfessional and business services`            0.0000000
## `Industry.my.fctrPublic administration`                         0.0000000
## `Industry.my.fctrTransportation and utilities`                  0.0000000
##                                                               Final.rpart.importance
## Age                                                                      100.0000000
## Industry.my.fctrOther services                                            27.4614104
## Industry.my.fctrEducational and health services                           25.2415079
## Industry.my.fctrTransportation and utilities                              23.7839646
## Industry.my.fctrManufacturing                                             23.1388463
## Industry.my.fctrProfessional and business services                        21.2185994
## Industry.my.fctrTrade                                                     20.1374624
## Industry.my.fctrConstruction                                              20.0204644
## Industry.my.fctrAgriculture, forestry, fishing, and hunting               19.9699985
## Industry.my.fctrPublic administration                                     19.9449139
## Industry.my.fctrFinancial                                                 19.7491177
## Industry.my.fctrInformation                                               19.2223509
## Industry.my.fctrLeisure and hospitality                                   16.1591357
## Industry.my.fctrMining                                                    10.4806617
## Industry.my.fctrArmed forces                                               0.6223753
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`              0.0000000
## `Industry.my.fctrArmed forces`                                             0.0000000
## `Industry.my.fctrEducational and health services`                          0.0000000
## `Industry.my.fctrLeisure and hospitality`                                  0.0000000
## `Industry.my.fctrOther services`                                           0.0000000
## `Industry.my.fctrProfessional and business services`                       0.0000000
## `Industry.my.fctrPublic administration`                                    0.0000000
## `Industry.my.fctrTransportation and utilities`                             0.0000000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](us_cps2_files/figure-html/fit.data.training_1-1.png) ![](us_cps2_files/figure-html/fit.data.training_1-2.png) 

```
## [1] "Min/Max Boundaries: "
##       .rownames EmploymentStatus.fctr
## 10        93435    Not.in.Labor.Force
## 107       35055               Retired
## 910       80194              Disabled
## 4110     111997              Disabled
## 5360      55065              Disabled
## 5476      55388              Disabled
## 5740      57377    Not.in.Labor.Force
## 7089        664    Not.in.Labor.Force
## 3453      34149              Disabled
## 3898      33583              Disabled
## 12742     35001            Unemployed
## 13154     41299              Disabled
## 5003      57040    Not.in.Labor.Force
## 8217      20026    Not.in.Labor.Force
##       EmploymentStatus.fctr.predict.Final.rpart
## 10                           Not.in.Labor.Force
## 107                                     Retired
## 910                                     Retired
## 4110                                    Retired
## 5360                                    Retired
## 5476                                    Retired
## 5740                                    Retired
## 7089                                    Retired
## 3453                         Not.in.Labor.Force
## 3898                         Not.in.Labor.Force
## 12742                        Not.in.Labor.Force
## 13154                        Not.in.Labor.Force
## 5003                                   Employed
## 8217                                   Employed
##       EmploymentStatus.fctr.predict.Final.rpart.prob
## 10                                        0.01738411
## 107                                       0.04509888
## 910                                       0.04509888
## 4110                                      0.04509888
## 5360                                      0.04509888
## 5476                                      0.04509888
## 5740                                      0.04509888
## 7089                                      0.04509888
## 3453                                      0.01738411
## 3898                                      0.01738411
## 12742                                     0.01738411
## 13154                                     0.01738411
## 5003                                      0.00000000
## 8217                                      0.00000000
##       EmploymentStatus.fctr.predict.Final.rpart.accurate
## 10                                                  TRUE
## 107                                                 TRUE
## 910                                                FALSE
## 4110                                               FALSE
## 5360                                               FALSE
## 5476                                               FALSE
## 5740                                               FALSE
## 7089                                               FALSE
## 3453                                               FALSE
## 3898                                               FALSE
## 12742                                              FALSE
## 13154                                              FALSE
## 5003                                               FALSE
## 8217                                               FALSE
##       EmploymentStatus.fctr.predict.Final.rpart.error .label
## 10                                          0.0000000  93435
## 107                                         0.0000000  35055
## 910                                         0.9549011  80194
## 4110                                        0.9549011 111997
## 5360                                        0.9549011  55065
## 5476                                        0.9549011  55388
## 5740                                        0.9549011  57377
## 7089                                        0.9549011    664
## 3453                                        0.9826159  34149
## 3898                                        0.9826159  33583
## 12742                                       0.9826159  35001
## 13154                                       0.9826159  41299
## 5003                                        1.0000000  57040
## 8217                                        1.0000000  20026
## [1] "Inaccurate: "
##     .rownames EmploymentStatus.fctr
## 89      91014    Not.in.Labor.Force
## 93      92066    Not.in.Labor.Force
## 119     25235               Retired
## 131     34884               Retired
## 148     92331    Not.in.Labor.Force
## 161     24269               Retired
##     EmploymentStatus.fctr.predict.Final.rpart
## 89                                   Disabled
## 93                                   Disabled
## 119                                  Disabled
## 131                                  Disabled
## 148                                  Disabled
## 161                                  Disabled
##     EmploymentStatus.fctr.predict.Final.rpart.prob
## 89                                       0.4365748
## 93                                       0.4365748
## 119                                      0.4365748
## 131                                      0.4365748
## 148                                      0.4365748
## 161                                      0.4365748
##     EmploymentStatus.fctr.predict.Final.rpart.accurate
## 89                                               FALSE
## 93                                               FALSE
## 119                                              FALSE
## 131                                              FALSE
## 148                                              FALSE
## 161                                              FALSE
##     EmploymentStatus.fctr.predict.Final.rpart.error
## 89                                        0.5634252
## 93                                        0.5634252
## 119                                       0.5634252
## 131                                       0.5634252
## 148                                       0.5634252
## 161                                       0.5634252
##        .rownames EmploymentStatus.fctr
## 130472     81794               Retired
## 105398     95771              Disabled
## 82516      27086              Disabled
## 83977       2630            Unemployed
## 60583      25168              Disabled
## 47827     107492            Unemployed
##        EmploymentStatus.fctr.predict.Final.rpart
## 130472                                  Disabled
## 105398                        Not.in.Labor.Force
## 82516                                    Retired
## 83977                         Not.in.Labor.Force
## 60583                                    Retired
## 47827                                   Employed
##        EmploymentStatus.fctr.predict.Final.rpart.prob
## 130472                                    0.436574812
## 105398                                    0.335152211
## 82516                                     0.274302939
## 83977                                     0.127533784
## 60583                                     0.045098876
## 47827                                     0.001291489
##        EmploymentStatus.fctr.predict.Final.rpart.accurate
## 130472                                              FALSE
## 105398                                              FALSE
## 82516                                               FALSE
## 83977                                               FALSE
## 60583                                               FALSE
## 47827                                               FALSE
##        EmploymentStatus.fctr.predict.Final.rpart.error
## 130472                                       0.5634252
## 105398                                       0.6648478
## 82516                                        0.7256971
## 83977                                        0.8724662
## 60583                                        0.9549011
## 47827                                        0.9987085
##        .rownames EmploymentStatus.fctr
## 130654     79473            Unemployed
## 130684     95119            Unemployed
## 130736     20447            Unemployed
## 130840     97074            Unemployed
## 130934     33835            Unemployed
## 131021     17653            Unemployed
##        EmploymentStatus.fctr.predict.Final.rpart
## 130654                                  Employed
## 130684                                  Employed
## 130736                                  Employed
## 130840                                  Employed
## 130934                                  Employed
## 131021                                  Employed
##        EmploymentStatus.fctr.predict.Final.rpart.prob
## 130654                                              0
## 130684                                              0
## 130736                                              0
## 130840                                              0
## 130934                                              0
## 131021                                              0
##        EmploymentStatus.fctr.predict.Final.rpart.accurate
## 130654                                              FALSE
## 130684                                              FALSE
## 130736                                              FALSE
## 130840                                              FALSE
## 130934                                              FALSE
## 131021                                              FALSE
##        EmploymentStatus.fctr.predict.Final.rpart.error
## 130654                                               1
## 130684                                               1
## 130736                                               1
## 130840                                               1
## 130934                                               1
## 131021                                               1
```

![](us_cps2_files/figure-html/fit.data.training_1-3.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])
```

```
## [1] EmploymentStatus.fctr                         
## [2] EmploymentStatus.fctr.predict.Final.rpart     
## [3] EmploymentStatus.fctr.predict.Final.rpart.prob
## <0 rows> (or 0-length row.names)
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "EmploymentStatus.fctr.predict.Final.rpart"     
## [2] "EmploymentStatus.fctr.predict.Final.rpart.prob"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](us_cps2_files/figure-html/fit.data.training_1-4.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 299.115 345.244  46.129
## 16  predict.data.new          9          0 345.244      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in `[<-.data.frame`(`*tmp*`, , paste0(rsp_var_out, ".prob"), value
## = structure(list(: provided 5 variables to replace 1 variables
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in loop_apply(n, do.ply): no non-missing arguments to min;
## returning Inf
```

```
## Warning in loop_apply(n, do.ply): no non-missing arguments to max;
## returning -Inf
```

```
## Warning in loop_apply(n, do.ply): Removed 25789 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 25789 rows containing missing
## values (geom_point).
```

![](us_cps2_files/figure-html/predict.data.new-1.png) 

```
## Warning in loop_apply(n, do.ply): no non-missing arguments to min;
## returning Inf
```

```
## Warning in loop_apply(n, do.ply): no non-missing arguments to max;
## returning -Inf
```

```
## Warning in loop_apply(n, do.ply): Removed 25789 rows containing missing
## values (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 25789 rows containing missing
## values (geom_point).
```

![](us_cps2_files/figure-html/predict.data.new-2.png) 

```
## [1] "Min/Max Boundaries: "
##       .rownames EmploymentStatus.fctr
## 26        92346                  <NA>
## 12016    117703                  <NA>
##       EmploymentStatus.fctr.predict.Final.rpart
## 26                           Not.in.Labor.Force
## 12016                                   Retired
##       EmploymentStatus.fctr.predict.Final.rpart.prob
## 26                                        0.01738411
## 12016                                     0.27430294
##       EmploymentStatus.fctr.predict.Final.rpart.accurate
## 26                                                    NA
## 12016                                                 NA
##       EmploymentStatus.fctr.predict.Final.rpart.error .label
## 26                                                  0  92346
## 12016                                               0 117703
## [1] "Inaccurate: "
##      .rownames EmploymentStatus.fctr
## NA        <NA>                  <NA>
## NA.1      <NA>                  <NA>
## NA.2      <NA>                  <NA>
## NA.3      <NA>                  <NA>
## NA.4      <NA>                  <NA>
## NA.5      <NA>                  <NA>
##      EmploymentStatus.fctr.predict.Final.rpart
## NA                                        <NA>
## NA.1                                      <NA>
## NA.2                                      <NA>
## NA.3                                      <NA>
## NA.4                                      <NA>
## NA.5                                      <NA>
##      EmploymentStatus.fctr.predict.Final.rpart.prob
## NA                                               NA
## NA.1                                             NA
## NA.2                                             NA
## NA.3                                             NA
## NA.4                                             NA
## NA.5                                             NA
##      EmploymentStatus.fctr.predict.Final.rpart.accurate
## NA                                                   NA
## NA.1                                                 NA
## NA.2                                                 NA
## NA.3                                                 NA
## NA.4                                                 NA
## NA.5                                                 NA
##      EmploymentStatus.fctr.predict.Final.rpart.error
## NA                                                NA
## NA.1                                              NA
## NA.2                                              NA
## NA.3                                              NA
## NA.4                                              NA
## NA.5                                              NA
##          .rownames EmploymentStatus.fctr
## NA.651        <NA>                  <NA>
## NA.7634       <NA>                  <NA>
## NA.11771      <NA>                  <NA>
## NA.14528      <NA>                  <NA>
## NA.21351      <NA>                  <NA>
## NA.23439      <NA>                  <NA>
##          EmploymentStatus.fctr.predict.Final.rpart
## NA.651                                        <NA>
## NA.7634                                       <NA>
## NA.11771                                      <NA>
## NA.14528                                      <NA>
## NA.21351                                      <NA>
## NA.23439                                      <NA>
##          EmploymentStatus.fctr.predict.Final.rpart.prob
## NA.651                                               NA
## NA.7634                                              NA
## NA.11771                                             NA
## NA.14528                                             NA
## NA.21351                                             NA
## NA.23439                                             NA
##          EmploymentStatus.fctr.predict.Final.rpart.accurate
## NA.651                                                   NA
## NA.7634                                                  NA
## NA.11771                                                 NA
## NA.14528                                                 NA
## NA.21351                                                 NA
## NA.23439                                                 NA
##          EmploymentStatus.fctr.predict.Final.rpart.error
## NA.651                                                NA
## NA.7634                                               NA
## NA.11771                                              NA
## NA.14528                                              NA
## NA.21351                                              NA
## NA.23439                                              NA
##          .rownames EmploymentStatus.fctr
## NA.25783      <NA>                  <NA>
## NA.25784      <NA>                  <NA>
## NA.25785      <NA>                  <NA>
## NA.25786      <NA>                  <NA>
## NA.25787      <NA>                  <NA>
## NA.25788      <NA>                  <NA>
##          EmploymentStatus.fctr.predict.Final.rpart
## NA.25783                                      <NA>
## NA.25784                                      <NA>
## NA.25785                                      <NA>
## NA.25786                                      <NA>
## NA.25787                                      <NA>
## NA.25788                                      <NA>
##          EmploymentStatus.fctr.predict.Final.rpart.prob
## NA.25783                                             NA
## NA.25784                                             NA
## NA.25785                                             NA
## NA.25786                                             NA
## NA.25787                                             NA
## NA.25788                                             NA
##          EmploymentStatus.fctr.predict.Final.rpart.accurate
## NA.25783                                                 NA
## NA.25784                                                 NA
## NA.25785                                                 NA
## NA.25786                                                 NA
## NA.25787                                                 NA
## NA.25788                                                 NA
##          EmploymentStatus.fctr.predict.Final.rpart.error
## NA.25783                                              NA
## NA.25784                                              NA
## NA.25785                                              NA
## NA.25786                                              NA
## NA.25787                                              NA
## NA.25788                                              NA
```

```
## Warning in loop_apply(n, do.ply): Removed 25789 rows containing missing
## values (geom_point).
```

![](us_cps2_files/figure-html/predict.data.new-3.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Max.cor.Y.cv.0.cp.0.rpart"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rpart"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 77145    35
```

```r
print(dsp_models_df)
```

```
##                    model_id max.Accuracy.OOB max.Kappa.OOB
## 4 Max.cor.Y.cv.0.cp.0.rpart        0.8828257  0.7967286427
## 5           Max.cor.Y.rpart        0.7261351  0.4752624489
## 6           Low.cor.X.rpart        0.7261351  0.4752624489
## 7      All.X.no.rnorm.rpart        0.7261351  0.4752624489
## 1         MFO.myMFO_classfr        0.5850606  0.0000000000
## 3      Max.cor.Y.cv.0.rpart        0.5850606  0.0000000000
## 2   Random.myrandom_classfr        0.3982657 -0.0008038519
```

```r
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
}    
```

```
## [1] "Max.cor.Y.cv.0.cp.0.rpart OOB confusion matrix & accuracy: "
##                     Prediction
## Reference            Disabled Employed Not.in.Labor.Force Retired
##   Disabled                468       24                534     510
##   Employed                  0    16594                  1       2
##   Not.in.Labor.Force      348      133               3369     248
##   Retired                 244       41                111    4610
##   Unemployed                3     1006                117       1
##                     Prediction
## Reference            Unemployed
##   Disabled                    0
##   Employed                    0
##   Not.in.Labor.Force          1
##   Retired                     0
##   Unemployed                  3
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

if (glb_is_classification) {
    print("FN_OOB_ids:")
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        glb_txt_vars])
    print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_OOBobs_df),
                    grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
}
```

```
## [1] "FN_OOB_ids:"
## [1] EmploymentStatus.fctr                                           
## [2] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart         
## [3] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.prob    
## [4] EmploymentStatus.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.accurate
## <0 rows> (or 0-length row.names)
## data frame with 0 columns and 0 rows
##     MetroAreaCode PeopleInHousehold               Age          Hispanic 
##                 0                 0                 0                 0
```

```r
dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                                                               Max.cor.Y.cv.0.cp.0.rpart.importance
## Age                                                                                     100.000000
## Industry.my.fctrOther services                                                           33.090343
## Industry.my.fctrPublic administration                                                    29.566192
## Industry.my.fctrManufacturing                                                            28.370178
## Industry.my.fctrEducational and health services                                          27.058509
## Industry.my.fctrProfessional and business services                                       25.591436
## Industry.my.fctrTrade                                                                    24.329022
## Industry.my.fctrTransportation and utilities                                             23.012814
## Industry.my.fctrConstruction                                                             22.685481
## Industry.my.fctrLeisure and hospitality                                                  22.062655
## Industry.my.fctrFinancial                                                                21.645346
## Industry.my.fctrAgriculture, forestry, fishing, and hunting                              20.237759
## Industry.my.fctrInformation                                                              15.923595
## Industry.my.fctrMining                                                                   11.722476
## Industry.my.fctrArmed forces                                                              0.784697
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`                             0.000000
## `Industry.my.fctrArmed forces`                                                            0.000000
## `Industry.my.fctrEducational and health services`                                         0.000000
## `Industry.my.fctrLeisure and hospitality`                                                 0.000000
## `Industry.my.fctrOther services`                                                          0.000000
## `Industry.my.fctrProfessional and business services`                                      0.000000
## `Industry.my.fctrPublic administration`                                                   0.000000
## `Industry.my.fctrTransportation and utilities`                                            0.000000
##                                                                importance
## Age                                                           100.0000000
## Industry.my.fctrOther services                                 27.4614104
## Industry.my.fctrPublic administration                          19.9449139
## Industry.my.fctrManufacturing                                  23.1388463
## Industry.my.fctrEducational and health services                25.2415079
## Industry.my.fctrProfessional and business services             21.2185994
## Industry.my.fctrTrade                                          20.1374624
## Industry.my.fctrTransportation and utilities                   23.7839646
## Industry.my.fctrConstruction                                   20.0204644
## Industry.my.fctrLeisure and hospitality                        16.1591357
## Industry.my.fctrFinancial                                      19.7491177
## Industry.my.fctrAgriculture, forestry, fishing, and hunting    19.9699985
## Industry.my.fctrInformation                                    19.2223509
## Industry.my.fctrMining                                         10.4806617
## Industry.my.fctrArmed forces                                    0.6223753
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`   0.0000000
## `Industry.my.fctrArmed forces`                                  0.0000000
## `Industry.my.fctrEducational and health services`               0.0000000
## `Industry.my.fctrLeisure and hospitality`                       0.0000000
## `Industry.my.fctrOther services`                                0.0000000
## `Industry.my.fctrProfessional and business services`            0.0000000
## `Industry.my.fctrPublic administration`                         0.0000000
## `Industry.my.fctrTransportation and utilities`                  0.0000000
##                                                               Final.rpart.importance
## Age                                                                      100.0000000
## Industry.my.fctrOther services                                            27.4614104
## Industry.my.fctrPublic administration                                     19.9449139
## Industry.my.fctrManufacturing                                             23.1388463
## Industry.my.fctrEducational and health services                           25.2415079
## Industry.my.fctrProfessional and business services                        21.2185994
## Industry.my.fctrTrade                                                     20.1374624
## Industry.my.fctrTransportation and utilities                              23.7839646
## Industry.my.fctrConstruction                                              20.0204644
## Industry.my.fctrLeisure and hospitality                                   16.1591357
## Industry.my.fctrFinancial                                                 19.7491177
## Industry.my.fctrAgriculture, forestry, fishing, and hunting               19.9699985
## Industry.my.fctrInformation                                               19.2223509
## Industry.my.fctrMining                                                    10.4806617
## Industry.my.fctrArmed forces                                               0.6223753
## `Industry.my.fctrAgriculture, forestry, fishing, and hunting`              0.0000000
## `Industry.my.fctrArmed forces`                                             0.0000000
## `Industry.my.fctrEducational and health services`                          0.0000000
## `Industry.my.fctrLeisure and hospitality`                                  0.0000000
## `Industry.my.fctrOther services`                                           0.0000000
## `Industry.my.fctrProfessional and business services`                       0.0000000
## `Industry.my.fctrPublic administration`                                    0.0000000
## `Industry.my.fctrTransportation and utilities`                             0.0000000
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 345.244 382.729  37.485
## 17 display.session.info         10          0 382.729      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 10              fit.models          7          0  75.172 186.411 111.239
## 11              fit.models          7          1 186.412 244.918  58.506
## 15       fit.data.training          8          1 299.115 345.244  46.129
## 16        predict.data.new          9          0 345.244 382.729  37.485
## 12              fit.models          7          2 244.919 268.677  23.758
## 2             inspect.data          2          0  19.517  42.418  22.901
## 4              encode.data          2          2  45.384  63.659  18.275
## 14       fit.data.training          8          0 281.597 299.115  17.518
## 13              fit.models          7          3 268.677 281.596  12.920
## 8          select.features          5          0  66.701  74.324   7.624
## 1              import.data          1          0  12.518  19.517   6.999
## 3               scrub.data          2          1  42.419  45.384   2.965
## 6         extract.features          3          0  64.137  65.840   1.703
## 7             cluster.data          4          0  65.840  66.701   0.861
## 9  partition.data.training          6          0  74.325  75.172   0.847
## 5      manage.missing.data          2          3  63.659  64.136   0.478
##    duration
## 10  111.239
## 11   58.506
## 15   46.129
## 16   37.485
## 12   23.758
## 2    22.901
## 4    18.275
## 14   17.518
## 13   12.919
## 8     7.623
## 1     6.999
## 3     2.965
## 6     1.703
## 7     0.861
## 9     0.847
## 5     0.477
## [1] "Total Elapsed Time: 382.729 secs"
```

![](us_cps2_files/figure-html/display.session.info-1.png) 

```
##                label step_major step_minor     bgn     end elapsed
## 2 fit.models_1_rpart          2          0 190.336 244.878  54.542
## 1   fit.models_1_bgn          1          0 190.320 190.336   0.016
##   duration
## 2   54.542
## 1    0.016
## [1] "Total Elapsed Time: 244.878 secs"
```

![](us_cps2_files/figure-html/display.session.info-2.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1     rpart.plot_1.5.2 rpart_4.1-9      caTools_1.17.1  
##  [5] dplyr_0.4.1      plyr_1.8.2       sqldf_0.4-10     RSQLite_1.0.0   
##  [9] DBI_0.3.1        gsubfn_0.6-6     proto_0.3-10     reshape2_1.4.1  
## [13] doMC_1.3.3       iterators_1.0.7  foreach_1.4.2    doBy_4.5-13     
## [17] survival_2.38-1  caret_6.0-47     ggplot2_1.0.1    lattice_0.20-31 
## 
## loaded via a namespace (and not attached):
##  [1] gtools_3.5.0        splines_3.2.0       colorspace_1.2-6   
##  [4] htmltools_0.2.6     yaml_2.1.13         mgcv_1.8-6         
##  [7] chron_2.3-45        e1071_1.6-4         nloptr_1.0.4       
## [10] RColorBrewer_1.1-2  stringr_1.0.0       munsell_0.4.2      
## [13] gtable_0.1.2        codetools_0.2-11    evaluate_0.7       
## [16] labeling_0.3        knitr_1.10.5        SparseM_1.6        
## [19] quantreg_5.11       pbkrtest_0.4-2      class_7.3-12       
## [22] Rcpp_0.11.6         scales_0.2.4        formatR_1.2        
## [25] BradleyTerry2_1.0-6 lme4_1.1-7          digest_0.6.8       
## [28] stringi_0.4-1       brglm_0.5-9         tools_3.2.0        
## [31] bitops_1.0-6        magrittr_1.5        lazyeval_0.1.10    
## [34] car_2.0-25          MASS_7.3-40         Matrix_1.2-1       
## [37] assertthat_0.1      minqa_1.2.4         rmarkdown_0.6.1    
## [40] compiler_3.2.0      nnet_7.3-9          nlme_3.1-120
```
