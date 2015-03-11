# US-CPS: <Predicted attribute> <regression/classification>
bdanalytics  

**  **    
**Date: (Wed) Mar 11, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/CPSData.csv  
    New:        <prdct_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(plyr))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis specific global variables
glb_separate_predict_dataset <- FALSE

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/CPSData.csv", 
    comment="entity_df", print_diagn=TRUE)
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
##        PeopleInHousehold    Region         State MetroAreaCode Age
## 21845                  1 Northeast   Connecticut         73450  57
## 59936                  4 Northeast Massachusetts         77200  20
## 94657                  1     South      Oklahoma         46140  54
## 99917                  2 Northeast  Pennsylvania         37980  55
## 114990                 4     South         Texas         19100  35
## 116348                 4     South         Texas         29700  15
##              Married    Sex              Education        Race Hispanic
## 21845       Divorced Female            High school       White        0
## 59936  Never Married   Male            High school       White        0
## 94657       Divorced Female      Bachelor's degree Multiracial        0
## 99917        Married Female            High school       White        0
## 114990       Married   Male No high school diploma       White        1
## 116348     Separated Female No high school diploma       White        1
##        CountryOfBirthCode          Citizenship   EmploymentStatus
## 21845                  57      Citizen, Native           Employed
## 59936                  57      Citizen, Native         Unemployed
## 94657                  57      Citizen, Native           Employed
## 99917                  57      Citizen, Native           Employed
## 114990                303 Citizen, Naturalized           Employed
## 116348                 57      Citizen, Native Not in Labor Force
##                                  Industry
## 21845     Educational and health services
## 59936  Professional and business services
## 94657     Educational and health services
## 99917     Educational and health services
## 114990                       Construction
## 116348                               <NA>
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
##  - attr(*, "comment")= chr "entity_df"
## NULL
```

```r
if (glb_separate_predict_dataset) {
    predct_df <- myimport_data(
        url="<prdct_url>", 
        comment="predct_df", print_diagn=TRUE)
} else {
    predct_df <- entity_df[sample(1:nrow(entity_df), nrow(entity_df) / 1000),]
    comment(predct_df) <- "predct_df"
    myprint_df(predct_df)
    str(predct_df)
}         
```

```
##        PeopleInHousehold  Region       State MetroAreaCode Age
## 42686                  2 Midwest    Illinois         16580  62
## 66862                  3   South Mississippi            NA  59
## 95548                  6   South    Oklahoma            NA  16
## 129952                 5    West     Wyoming            NA   4
## 4535                   7   South    Arkansas         30780   6
## 20007                  1    West    Colorado         14500  57
##              Married    Sex              Education             Race
## 42686        Married   Male            High school            White
## 66862      Separated Female            High school            Black
## 95548  Never Married Female No high school diploma            White
## 129952          <NA>   Male                   <NA> Pacific Islander
## 4535            <NA> Female                   <NA>            Black
## 20007       Divorced   Male        Master's degree            White
##        Hispanic CountryOfBirthCode     Citizenship   EmploymentStatus
## 42686         0                 57 Citizen, Native           Employed
## 66862         0                 57 Citizen, Native           Disabled
## 95548         0                 57 Citizen, Native Not in Labor Force
## 129952        0                 57 Citizen, Native               <NA>
## 4535          0                 57 Citizen, Native               <NA>
## 20007         0                 57 Citizen, Native           Employed
##            Industry
## 42686  Construction
## 66862          <NA>
## 95548          <NA>
## 129952         <NA>
## 4535           <NA>
## 20007     Financial
##        PeopleInHousehold  Region          State MetroAreaCode Age
## 23497                  4   South       Delaware            NA  31
## 104105                 3   South South Carolina         16700  19
## 89508                  1 Midwest   North Dakota            NA  25
## 48943                  5 Midwest         Kansas         29940  16
## 93790                  4 Midwest           Ohio            NA  24
## 79963                  2    West     New Mexico         10740  70
##              Married    Sex               Education        Race Hispanic
## 23497        Married Female             High school       White        1
## 104105 Never Married Female Some college, no degree       White        0
## 89508  Never Married Female             High school       White        1
## 48943  Never Married Female  No high school diploma Multiracial        0
## 93790        Married Female             High school       White        0
## 79963        Married   Male             High school       White        1
##        CountryOfBirthCode          Citizenship   EmploymentStatus
## 23497                 303 Citizen, Naturalized Not in Labor Force
## 104105                 57      Citizen, Native Not in Labor Force
## 89508                  57      Citizen, Native           Employed
## 48943                  57      Citizen, Native Not in Labor Force
## 93790                  57      Citizen, Native           Employed
## 79963                  57      Citizen, Native            Retired
##                     Industry
## 23497                   <NA>
## 104105                  <NA>
## 89508                  Trade
## 48943                   <NA>
## 93790  Public administration
## 79963                   <NA>
##        PeopleInHousehold  Region          State MetroAreaCode Age
## 88141                  7   South North Carolina            NA  14
## 106616                 4 Midwest   South Dakota            NA   6
## 86991                  5   South North Carolina         39580  36
## 123891                 3    West     Washington         44060  46
## 114756                 1   South          Texas         26420  26
## 91476                  8 Midwest           Ohio         49660   5
##              Married    Sex               Education  Race Hispanic
## 88141           <NA> Female                    <NA> Black        0
## 106616          <NA> Female                    <NA> White        0
## 86991  Never Married Female  No high school diploma White        1
## 123891 Never Married   Male Some college, no degree White        0
## 114756     Separated   Male       Bachelor's degree White        1
## 91476           <NA>   Male                    <NA> White        0
##        CountryOfBirthCode     Citizenship   EmploymentStatus Industry
## 88141                  57 Citizen, Native               <NA>     <NA>
## 106616                 57 Citizen, Native               <NA>     <NA>
## 86991                 314     Non-Citizen Not in Labor Force     <NA>
## 123891                 57 Citizen, Native           Disabled     <NA>
## 114756                 57 Citizen, Native           Employed   Mining
## 91476                  57 Citizen, Native               <NA>     <NA>
## 'data.frame':	131 obs. of  14 variables:
##  $ PeopleInHousehold : int  2 3 6 5 7 1 3 3 4 2 ...
##  $ Region            : chr  "Midwest" "South" "South" "West" ...
##  $ State             : chr  "Illinois" "Mississippi" "Oklahoma" "Wyoming" ...
##  $ MetroAreaCode     : int  16580 NA NA NA 30780 14500 38900 33660 29180 19820 ...
##  $ Age               : int  62 59 16 4 6 57 29 2 5 48 ...
##  $ Married           : chr  "Married" "Separated" "Never Married" NA ...
##  $ Sex               : chr  "Male" "Female" "Female" "Male" ...
##  $ Education         : chr  "High school" "High school" "No high school diploma" NA ...
##  $ Race              : chr  "White" "Black" "White" "Pacific Islander" ...
##  $ Hispanic          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CountryOfBirthCode: int  57 57 57 57 57 57 57 57 57 57 ...
##  $ Citizenship       : chr  "Citizen, Native" "Citizen, Native" "Citizen, Native" "Citizen, Native" ...
##  $ EmploymentStatus  : chr  "Employed" "Disabled" "Not in Labor Force" NA ...
##  $ Industry          : chr  "Construction" NA NA NA ...
##  - attr(*, "comment")= chr "predct_df"
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=1))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 inspect_data                2                1
```

### Step `2`.`1`: inspect data

```r
#print(str(entity_df))
#View(entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Potential Enhancements:
#       One code chunk to cycle thru entity_df & predct_df ?
#           Use with / within ?
#           for (df in c(entity_df, predct_df)) cycles thru column names
#           for (df in list(entity_df, predct_df)) does not change the actual dataframes
#
#       Build splines   require(splines); bsBasis <- bs(training$age, df=3)

entity_df <- mutate(entity_df, 
#     <col_name>_fctr=as.factor(<col_name>),
#     
#     Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#     Year=year(Date.my),
#     Month=months(Date.my),
#     Weekday=weekdays(Date.my)
#     
    MetroAreaCode.NA=is.na(MetroAreaCode)
                    )
# 
# predct_df <- mutate(predct_df, 
#                     )

print(summary(entity_df))
```

```
##  PeopleInHousehold    Region             State           MetroAreaCode  
##  Min.   : 1.000    Length:131302      Length:131302      Min.   :10420  
##  1st Qu.: 2.000    Class :character   Class :character   1st Qu.:21780  
##  Median : 3.000    Mode  :character   Mode  :character   Median :34740  
##  Mean   : 3.284                                          Mean   :35075  
##  3rd Qu.: 4.000                                          3rd Qu.:41860  
##  Max.   :15.000                                          Max.   :79600  
##                                                          NA's   :34238  
##       Age          Married              Sex             Education        
##  Min.   : 0.00   Length:131302      Length:131302      Length:131302     
##  1st Qu.:19.00   Class :character   Class :character   Class :character  
##  Median :39.00   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :38.83                                                           
##  3rd Qu.:57.00                                                           
##  Max.   :85.00                                                           
##                                                                          
##      Race              Hispanic      CountryOfBirthCode Citizenship       
##  Length:131302      Min.   :0.0000   Min.   : 57.00     Length:131302     
##  Class :character   1st Qu.:0.0000   1st Qu.: 57.00     Class :character  
##  Mode  :character   Median :0.0000   Median : 57.00     Mode  :character  
##                     Mean   :0.1393   Mean   : 82.68                       
##                     3rd Qu.:0.0000   3rd Qu.: 57.00                       
##                     Max.   :1.0000   Max.   :555.00                       
##                                                                           
##  EmploymentStatus     Industry         MetroAreaCode.NA
##  Length:131302      Length:131302      Mode :logical   
##  Class :character   Class :character   FALSE:97064     
##  Mode  :character   Mode  :character   TRUE :34238     
##                                        NA's :0         
##                                                        
##                                                        
## 
```

```r
print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
```

```
##  PeopleInHousehold             Region              State 
##                  0                  0                  0 
##      MetroAreaCode                Age            Married 
##              34238                  0              25338 
##                Sex          Education               Race 
##                  0              25338                  0 
##           Hispanic CountryOfBirthCode        Citizenship 
##                  0                  0                  0 
##   EmploymentStatus           Industry   MetroAreaCode.NA 
##              25789              65060                  0
```

```r
print(summary(predct_df))
```

```
##  PeopleInHousehold    Region             State           MetroAreaCode  
##  Min.   : 1.000    Length:131         Length:131         Min.   :10740  
##  1st Qu.: 2.000    Class :character   Class :character   1st Qu.:22660  
##  Median : 3.000    Mode  :character   Mode  :character   Median :33340  
##  Mean   : 3.252                                          Mean   :32612  
##  3rd Qu.: 4.000                                          3rd Qu.:40660  
##  Max.   :12.000                                          Max.   :78100  
##                                                          NA's   :36     
##       Age          Married              Sex             Education        
##  Min.   : 0.00   Length:131         Length:131         Length:131        
##  1st Qu.:20.00   Class :character   Class :character   Class :character  
##  Median :42.00   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :40.56                                                           
##  3rd Qu.:60.50                                                           
##  Max.   :85.00                                                           
##                                                                          
##      Race              Hispanic      CountryOfBirthCode Citizenship       
##  Length:131         Min.   :0.0000   Min.   : 57.00     Length:131        
##  Class :character   1st Qu.:0.0000   1st Qu.: 57.00     Class :character  
##  Mode  :character   Median :0.0000   Median : 57.00     Mode  :character  
##                     Mean   :0.1374   Mean   : 83.15                       
##                     3rd Qu.:0.0000   3rd Qu.: 57.00                       
##                     Max.   :1.0000   Max.   :414.00                       
##                                                                           
##  EmploymentStatus     Industry        
##  Length:131         Length:131        
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
##                                       
##                                       
##                                       
## 
```

```r
print(sapply(names(predct_df), function(col) sum(is.na(predct_df[, col]))))
```

```
##  PeopleInHousehold             Region              State 
##                  0                  0                  0 
##      MetroAreaCode                Age            Married 
##                 36                  0                 26 
##                Sex          Education               Race 
##                  0                 26                  0 
##           Hispanic CountryOfBirthCode        Citizenship 
##                  0                  0                  0 
##   EmploymentStatus           Industry 
##                 26                 78
```

```r
#pairs(subset(entity_df, select=-c(col_symbol)))

#   Histogram of predictor in entity_df & predct_df
# Check for predct_df & entity_df features range mismatches

# Other diagnostics:
# print(subset(entity_df, <col1_name> == max(entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(entity_df$<col1_name>, na.rm=TRUE)))

print(sort(table(entity_df$Industry)))
```

```
## 
##                                Armed forces 
##                                          29 
##                                      Mining 
##                                         550 
## Agriculture, forestry, fishing, and hunting 
##                                        1307 
##                                 Information 
##                                        1328 
##                       Public administration 
##                                        3186 
##                              Other services 
##                                        3224 
##                Transportation and utilities 
##                                        3260 
##                                   Financial 
##                                        4347 
##                                Construction 
##                                        4387 
##                     Leisure and hospitality 
##                                        6364 
##                               Manufacturing 
##                                        6791 
##          Professional and business services 
##                                        7519 
##                                       Trade 
##                                        8933 
##             Educational and health services 
##                                       15017
```

```r
print(sort(table(entity_df$State)))
```

```
## 
##           New Mexico              Montana          Mississippi 
##                 1102                 1214                 1230 
##              Alabama        West Virginia             Arkansas 
##                 1376                 1409                 1421 
##            Louisiana                Idaho             Oklahoma 
##                 1450                 1518                 1523 
##              Arizona               Alaska              Wyoming 
##                 1528                 1590                 1624 
##         North Dakota       South Carolina            Tennessee 
##                 1645                 1658                 1784 
## District of Columbia             Kentucky                 Utah 
##                 1791                 1841                 1842 
##               Nevada              Vermont               Kansas 
##                 1856                 1890                 1935 
##               Oregon             Nebraska        Massachusetts 
##                 1943                 1949                 1987 
##         South Dakota              Indiana               Hawaii 
##                 2000                 2004                 2099 
##             Missouri         Rhode Island             Delaware 
##                 2145                 2209                 2214 
##                Maine           Washington                 Iowa 
##                 2263                 2366                 2528 
##           New Jersey       North Carolina        New Hampshire 
##                 2567                 2619                 2662 
##            Wisconsin              Georgia          Connecticut 
##                 2686                 2807                 2836 
##             Colorado             Virginia             Michigan 
##                 2925                 2953                 3063 
##            Minnesota             Maryland                 Ohio 
##                 3139                 3200                 3678 
##             Illinois         Pennsylvania              Florida 
##                 3912                 3930                 5149 
##             New York                Texas           California 
##                 5595                 7077                11570
```

```r
print(Citizenship_freq_entity_df <- as.data.frame(table(entity_df$Citizenship)))
```

```
##                   Var1   Freq
## 1      Citizen, Native 116639
## 2 Citizen, Naturalized   7073
## 3          Non-Citizen   7590
```

```r
names(Citizenship_freq_entity_df)[1] <- "Citizenship"; print(Citizenship_freq_entity_df)
```

```
##            Citizenship   Freq
## 1      Citizen, Native 116639
## 2 Citizen, Naturalized   7073
## 3          Non-Citizen   7590
```

```r
print(1 - (Citizenship_freq_entity_df[Citizenship_freq_entity_df$Citizenship == "Non-Citizen", "Freq"] * 1.0 / sum(Citizenship_freq_entity_df$Freq)))
```

```
## [1] 0.9421943
```

```r
print(table(is.na(entity_df$Married), entity_df$Region))
```

```
##        
##         Midwest Northeast South  West
##   FALSE   24609     21432 33535 26388
##   TRUE     6075      4507  7967  6789
```

```r
print(table(is.na(entity_df$Married), entity_df$Sex))
```

```
##        
##         Female  Male
##   FALSE  55264 50700
##   TRUE   12217 13121
```

```r
print(table(is.na(entity_df$Married), entity_df$Age))
```

```
##        
##            0    1    2    3    4    5    6    7    8    9   10   11   12
##   FALSE    0    0    0    0    0    0    0    0    0    0    0    0    0
##   TRUE  1283 1559 1574 1693 1695 1795 1721 1681 1729 1748 1750 1721 1797
##        
##           13   14   15   16   17   18   19   20   21   22   23   24   25
##   FALSE    0    0 1795 1751 1764 1596 1517 1398 1525 1536 1638 1627 1604
##   TRUE  1802 1790    0    0    0    0    0    0    0    0    0    0    0
##        
##           26   27   28   29   30   31   32   33   34   35   36   37   38
##   FALSE 1643 1657 1736 1645 1854 1762 1790 1804 1653 1716 1663 1531 1530
##   TRUE     0    0    0    0    0    0    0    0    0    0    0    0    0
##        
##           39   40   41   42   43   44   45   46   47   48   49   50   51
##   FALSE 1542 1571 1673 1711 1819 1764 1749 1665 1647 1791 1989 1966 1931
##   TRUE     0    0    0    0    0    0    0    0    0    0    0    0    0
##        
##           52   53   54   55   56   57   58   59   60   61   62   63   64
##   FALSE 1935 1994 1912 1895 1935 1827 1874 1758 1746 1735 1595 1596 1519
##   TRUE     0    0    0    0    0    0    0    0    0    0    0    0    0
##        
##           65   66   67   68   69   70   71   72   73   74   75   76   77
##   FALSE 1569 1577 1227 1130 1062 1195 1031  941  896  842  763  729  698
##   TRUE     0    0    0    0    0    0    0    0    0    0    0    0    0
##        
##           78   79   80   85
##   FALSE  659  661 2664 2446
##   TRUE     0    0    0    0
```

```r
print(prblm_2_4_df <- mycreate_xtab(entity_df, c("Region", "MetroAreaCode.NA")))
```

```
##      Region MetroAreaCode.NA.FALSE MetroAreaCode.NA.TRUE
## 1   Midwest                  20010                 10674
## 2 Northeast                  20330                  5609
## 3     South                  31631                  9871
## 4      West                  25093                  8084
```

```r
prblm_2_4_df[is.na(prblm_2_4_df)] <- 0
print(prblm_2_4_df <- mutate(prblm_2_4_df, MetroCode.NA.ratio = MetroAreaCode.NA.TRUE * 1.0 / (MetroAreaCode.NA.FALSE + MetroAreaCode.NA.TRUE)))
```

```
##      Region MetroAreaCode.NA.FALSE MetroAreaCode.NA.TRUE
## 1   Midwest                  20010                 10674
## 2 Northeast                  20330                  5609
## 3     South                  31631                  9871
## 4      West                  25093                  8084
##   MetroCode.NA.ratio
## 1          0.3478686
## 2          0.2162381
## 3          0.2378440
## 4          0.2436628
```

```r
print(prblm_2_5_arr <- sort(tapply(entity_df$MetroAreaCode.NA, entity_df$State, mean, na.rm=TRUE)))
```

```
## District of Columbia           New Jersey         Rhode Island 
##           0.00000000           0.00000000           0.00000000 
##           California              Florida        Massachusetts 
##           0.02048401           0.03923092           0.06492199 
##             Maryland             New York          Connecticut 
##           0.06937500           0.08060769           0.08568406 
##             Illinois             Colorado              Arizona 
##           0.11221881           0.12991453           0.13154450 
##               Nevada                Texas            Louisiana 
##           0.13308190           0.14370496           0.16137931 
##         Pennsylvania             Michigan           Washington 
##           0.17430025           0.17825661           0.18131868 
##              Georgia             Virginia                 Utah 
##           0.19843249           0.19844226           0.21009772 
##               Oregon             Delaware           New Mexico 
##           0.21821925           0.23396567           0.24500907 
##               Hawaii                 Ohio              Alabama 
##           0.24916627           0.25122349           0.25872093 
##              Indiana            Wisconsin       South Carolina 
##           0.29141717           0.29932986           0.31302774 
##            Minnesota             Oklahoma             Missouri 
##           0.31506849           0.32764281           0.32867133 
##            Tennessee               Kansas       North Carolina 
##           0.35594170           0.36227390           0.37304315 
##                 Iowa             Arkansas                Idaho 
##           0.48694620           0.49049965           0.49868248 
##             Kentucky        New Hampshire             Nebraska 
##           0.50678979           0.56874530           0.58132376 
##                Maine              Vermont          Mississippi 
##           0.59832081           0.65238095           0.69430894 
##         South Dakota         North Dakota        West Virginia 
##           0.70250000           0.73738602           0.75585522 
##              Montana               Alaska              Wyoming 
##           0.83607908           1.00000000           1.00000000
```

```r
# print(which.min(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col_name>)))
# print(which.max(table(entity_df$<col1_name>, entity_df$<col2_name>)[, 2]))
# print(table(entity_df$<col1_name>, entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, entity_df))
print(xtab_entity_df <- mycreate_xtab(entity_df, c("Race", "Hispanic")))
```

```
##               Race Hispanic.0 Hispanic.1
## 1  American Indian       1129        304
## 2            Asian       6407        113
## 3            Black      13292        621
## 4      Multiracial       2449        448
## 5 Pacific Islander        541         77
## 6            White      89190      16731
```

```r
# print(xtab_entity_df <- mutate(xtab_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(entity_df$<col1_name>, entity_df$<col2_name>, min, na.rm=TRUE)))

# Other plots:
# print(myplot_histogram(entity_df, "<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(entity_df, "<col1_name>", "<col2_name>"))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##        chunk_label chunk_step_major chunk_step_minor
## 1      import_data                1                0
## 2     inspect_data                2                1
## 3 extract_features                3                0
```

## Step `3`: extract features

```r
# script_df <- rbind(script_df, 
#                    data.frame(chunk_label="extract_features", 
#                               chunk_step_major=max(script_df$chunk_step_major)+1, 
#                               chunk_step_minor=0))
print(script_df)
```

```
##        chunk_label chunk_step_major chunk_step_minor
## 1      import_data                1                0
## 2     inspect_data                2                1
## 3 extract_features                3                0
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.

## remove nearZeroVar features (not much variance)
#require(reshape)
#var_features_df <- melt(summaryBy(. ~ factor(0), data=entity_df[, features_lst], 
#                             FUN=var, keep.names=TRUE), 
#                             variable_name=c("feature"))
#names(var_features_df)[2] <- "var"
#print(var_features_df[order(var_features_df$var), ])
# summaryBy ignores factors whereas nearZeroVar inspects factors

# k_fold <- 5
# entity_df[order(entity_df$classe, 
#                   entity_df$user_name, 
#                   entity_df$my.rnorm),"my.cv_ix"] <- 
#     rep(1:k_fold, length.out=nrow(entity_df))
# summaryBy(X ~ my.cv_ix, data=entity_df, FUN=length)
# tapply(entity_df$X, list(entity_df$classe, entity_df$user_name, 
#                            entity_df$my.cv_ix), length)

#require(DAAG)
#entity_df$classe.proper <- as.numeric(entity_df$classe == "A")
#rnorm.glm <- glm(classe.proper ~ rnorm, family=binomial, data=entity_df)
#cv.binary(rnorm.glm, nfolds=k_fold, print.details=TRUE)
#result <- cv.lm(df=entity_df, form.lm=formula(classe ~ rnorm), 
#                    m=k_fold, seed=12345, printit=TRUE)

#plot(mdl_1$finalModel, uniform=TRUE, main="base")
#text(mdl_1$finalModel, use.n=TRUE, all=TRUE, cex=0.8)



```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13     survival_2.38-1
## [5] ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] codetools_0.2-10 colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5  
##  [5] formatR_1.0      grid_3.1.2       gtable_0.1.2     htmltools_0.2.6 
##  [9] knitr_1.9        lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1 
## [17] scales_0.2.4     splines_3.1.2    stringr_0.6.2    tcltk_3.1.2     
## [21] tools_3.1.2      yaml_2.1.13
```
