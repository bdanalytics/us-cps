# US-CPS: <Predicted attribute> <regression/classification>
bdanalytics  

**  **    
**Date: (Thu) Mar 12, 2015**    

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

print(Industry_freq_entity_df <- mycreate_tbl_df(entity_df, "Industry"))
```

```
##                                       Industry .freq
## 1                                 Armed forces    29
## 2                                       Mining   550
## 3  Agriculture, forestry, fishing, and hunting  1307
## 4                                  Information  1328
## 5                        Public administration  3186
## 6                               Other services  3224
## 7                 Transportation and utilities  3260
## 8                                    Financial  4347
## 9                                 Construction  4387
## 10                     Leisure and hospitality  6364
## 11                               Manufacturing  6791
## 12          Professional and business services  7519
## 13                                       Trade  8933
## 14             Educational and health services 15017
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
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[which.max(script_df$chunk_step_major), 
                                   "chunk_step_minor"]+1))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[which.max(script_df$chunk_step_major), 
                                   "chunk_step_minor"]+1))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
```

### Step `2`.`2`: encode data

```r
map_metroarea_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/MetroAreaCodes.csv", 
    comment="map_metroarea_df", print_diagn=TRUE)
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
##      Code                      MetroArea
## 40  15180      Brownsville-Harlingen, TX
## 133 29940                   Lawrence, KS
## 171 36500                    Olympia, WA
## 245 47300        Visalia-Porterville, CA
## 258 71650 Boston-Cambridge-Quincy, MA-NH
## 266 76750    Portland-South Portland, ME
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
##  - attr(*, "comment")= chr "map_metroarea_df"
## NULL
```

```r
entity_df <- mymap_codes(entity_df, "MetroAreaCode", "MetroArea", 
    					map_metroarea_df, map_join_col_name="Code")
```

```
## Loading required package: tcltk
```

```
##   MetroAreaCode                                          MetroArea    _n
## 1            NA                                               <NA> 34238
## 2         35620 New York-Northern New Jersey-Long Island, NY-NJ-PA  5409
## 3         47900       Washington-Arlington-Alexandria, DC-VA-MD-WV  4177
## 4         31100               Los Angeles-Long Beach-Santa Ana, CA  4102
## 5         37980           Philadelphia-Camden-Wilmington, PA-NJ-DE  2855
## 6         16980                Chicago-Naperville-Joliet, IN-IN-WI  2772
##     MetroAreaCode                          MetroArea  _n
## 55          40060                       Richmond, VA 490
## 92          27140                        Jackson, MS 222
## 99          13740                       Billings, MT 199
## 148         16580               Champaign-Urbana, IL 122
## 209         46940                     Vero Beach, FL  79
## 219         40980 Saginaw-Saginaw Township North, MI  74
##     MetroAreaCode             MetroArea _n
## 260         46660          Valdosta, GA 42
## 261         47580     Warner Robins, GA 42
## 262         14060 Bloomington-Normal IL 40
## 263         44220       Springfield, OH 34
## 264         36140        Ocean City, NJ 30
## 265         14540     Bowling Green, KY 29
```

![](us_cps_files/figure-html/encode_data_1-1.png) 

```r
print(MetroArea_freq_entity_df <- mycreate_tbl_df(entity_df, "MetroArea"))
```

```
##                                              MetroArea .freq
## 1                                    Bowling Green, KY    29
## 2                                       Ocean City, NJ    30
## 3                                      Springfield, OH    34
## 4                                Bloomington-Normal IL    40
## 5                                         Valdosta, GA    42
## 6                                    Warner Robins, GA    42
## 7                                      Tallahassee, FL    43
## 8                                         Columbia, MO    47
## 9                                      Punta Gorda, FL    48
## 10                                         Midland, TX    51
## 11                             Niles-Benton Harbor, MI    51
## 12                                    Johnson City, TN    52
## 13                                        Santa Fe, NM    52
## 14                                        Prescott, AZ    54
## 15                    Vineland-Millville-Bridgeton, NJ    54
## 16                       Hickory-Morgantown-Lenoir, NC    57
## 17                                          Madera, CA    57
## 18                                     Columbus, GA-AL    59
## 19                                          Joplin, MO    59
## 20                          Panama City-Lynn Haven, FL    59
## 21                                           Chico, CA    60
## 22                                 Anniston-Oxford, AL    61
## 23                                            Napa, CA    61
## 24                                        Anderson, IN    62
## 25                                        Florence, AL    63
## 26                                    Jacksonville, NC    63
## 27                                       Johnstown, PA    63
## 28                                         Lubbock, TX    63
## 29                                          Monroe, MI    63
## 30                                        Anderson, SC    64
## 31                                      Farmington, NM    64
## 32                             Athens-Clark County, GA    65
## 33                                 Gulfport-Biloxi, MS    65
## 34                                        Longview, TX    65
## 35                                           Macon, GA    65
## 36                    Leominster-Fitchburg-Gardner, MA    66
## 37                                         Roanoke, VA    66
## 38                          Santa-Cruz-Watsonville, CA    66
## 39                            Kingsport-Bristol, TN-VA    67
## 40                                          Albany, GA    68
## 41                                      Bellingham, WA    70
## 42                                     Gainesville, FL    70
## 43                                         Jackson, MI    70
## 44                                      Binghamton, NY    73
## 45                                       Lynchburg, VA    73
## 46                  Saginaw-Saginaw Township North, MI    74
## 47                                       Salisbury, MD    74
## 48                                 Barnstable Town, MA    75
## 49                                           Ocala, FL    76
## 50                                     Springfield, IL    76
## 51                                    Fayetteville, NC    77
## 52                          Michigan City-La Porte, IN    77
## 53                     San Luis Obispo-Paso Robles, CA    77
## 54                             Holland-Grand Haven, MI    78
## 55                                      Tuscaloosa, AL    78
## 56                           Brownsville-Harlingen, TX    79
## 57                                      Vero Beach, FL    79
## 58                                            Waco, TX    79
## 59              Fort Walton Beach-Crestview-Destin, FL    80
## 60                                      Utica-Rome, NY    80
## 61                                         Decatur, IL    81
## 62                                    Lake Charles, LA    81
## 63                         South Bend-Mishawaka, IN-MI    81
## 64                                         Altoona, PA    82
## 65                        Huntington-Ashland, WV-KY-OH    82
## 66                                         Medford, OR    82
## 67                             Naples-Marco Island, FL    82
## 68                                       St. Cloud, MN    82
## 69                                       Ann Arbor, MI    85
## 70                                  Oshkosh-Neenah, WI    85
## 71                       Hagerstown-Martinsburg, MD-WV    86
## 72                            Bremerton-Silverdale, WA    87
## 73                                            Erie, PA    87
## 74                                Kankakee-Bradley, IL    87
## 75                                        Kingston, NY    87
## 76                                        Amarillo, TX    88
## 77                                          Laredo, TX    89
## 78                                    Harrisonburg, VA    90
## 79                          Muskegon-Norton Shores, MI    90
## 80                                   Trenton-Ewing, NJ    91
## 81                                         Decatur, Al    96
## 82                                          Wausau, WI    96
## 83                                          Lawton, OK    97
## 84                                        Lawrence, KS    98
## 85                                       El Centro, CA    99
## 86                                   Evansville, IN-KY    99
## 87                                      Janesville, WI    99
## 88                                         Olympia, WA    99
## 89                                     Spartanburg, SC    99
## 90                        Killeen-Temple-Fort Hood, TX   101
## 91                                           Flint, MI   102
## 92          Myrtle Beach-Conway-North Myrtle Beach, SC   102
## 93                                      Montgomery, AL   103
## 94                                     Bloomington, IN   104
## 95                                         Salinas, CA   104
## 96                                   Fort Smith, AR-OK   105
## 97                                          Merced, CA   106
## 98                                      Las Cruses, NM   107
## 99                      Pensacola-Ferry Pass-Brent, FL   107
## 100                     Port St. Lucie-Fort Pierce, FL   109
## 101                                     Eau Claire, WI   110
## 102                                         Mobile, AL   110
## 103                                  Atlantic City, NJ   111
## 104                                        Danbury, CT   112
## 105                                         Peoria, IL   112
## 106                                         Yakima, WA   112
## 107                                      La Crosse, WI   114
## 108                                       Rockford, IL   114
## 109                                      Asheville, NC   116
## 110                                       Victoria, TX   116
## 111                                  Coeur d'Alene, ID   117
## 112                                     Huntsville, AL   117
## 113                                   York-Hanover, PA   117
## 114                               Canton-Massillon, OH   118
## 115                           Lansing-East Lansing, MI   119
## 116                                         Racine, WI   119
## 117                            Visalia-Porterville, CA   121
## 118                               Champaign-Urbana, IL   122
## 119                           Beaumont-Port Author, TX   123
## 120                                        Appleton,WI   125
## 121                                      Duluth, MN-WI   126
## 122                              Kalamazoo-Portage, MI   127
## 123                                  Winston-Salem, NC   127
## 124                            Santa Rosa-Petaluma, CA   129
## 125                                         Pueblo, CO   130
## 126                                      Iowa City, IA   131
## 127                                 Corpus Christi, TX   132
## 128               Santa Barbara-Santa Maria-Goleta, CA   132
## 129                              Vallejo-Fairfield, CA   133
## 130                                     Fort Wayne, IN   136
## 131                                      Green Bay, WI   136
## 132                                           Bend, OR   140
## 133             Deltona-Daytona Beach-Ormond Beach, FL   140
## 134                                        Reading, PA   142
## 135                                   Worcester, MA-CT   144
## 136                          Cape Coral-Fort Myers, FL   146
## 137                        Shreveport-Bossier City, LA   146
## 138                          Lakeland-Winter Haven, FL   149
## 139                     Youngstown-Warren-Boardman, OH   153
## 140                                 Springfield, MA-CT   155
## 141                                      Lancaster, PA   156
## 142                                        Spokane, WA   156
## 143                           Waterloo-Cedar Falls, IA   156
## 144                                      Waterbury, CT   157
## 145                                        Modesto, CA   158
## 146                     Augusta-Richmond County, GA-SC   161
## 147                                    Springfield, MO   161
## 148                                        Greeley, CO   162
## 149                                 Chattanooga, TN-GA   167
## 150                                      Knoxville, TN   168
## 151                  Palm Bay-Melbourne-Titusville, FL   168
## 152                                          Salem, OR   170
## 153                                        Boulder, CO   171
## 154                            Harrisburg-Carlisle, PA   174
## 155                          Scranton-Wilkes Barre, PA   176
## 156                                         Monroe, LA   179
## 157                                      Lafayette, LA   181
## 158                                         Topeka, KS   182
## 159                                     Greenville, SC   185
## 160                                         Durham, NC   189
## 161                      Sarasota-Bradenton-Venice, FL   192
## 162                                       Stockton, CA   193
## 163                         McAllen-Edinburg-Pharr, TX   195
## 164                                   Cedar Rapids, IA   196
## 165                             Eugene-Springfield, OR   196
## 166                              Lexington-Fayette, KY   198
## 167                                       Billings, MT   199
## 168               Poughkeepsie-Newburgh-Middletown, NY   201
## 169                                       Savannah, GA   202
## 170                          Norwich-New London, CT-RI   203
## 171                          Fort Collins-Loveland, CO   206
## 172                                         Bangor, ME   208
## 173              Fayetteville-Springdale-Rogers, AR-MO   215
## 174                                        Jackson, MS   222
## 175                                       Syracuse, NY   223
## 176                                          Akron, OH   231
## 177                    Charleston-North Charleston, SC   232
## 178                                         Toledo, OH   235
## 179                Davenport-Moline-Rock Island, IA-IL   240
## 180                                        El Paso, TX   244
## 181                                    Bakersfield, CA   245
## 182                          Greensboro-High Point, NC   251
## 183                                    Baton Rouge, LA   262
## 184                                     Charleston, WV   262
## 185                             Rochester-Dover, NH-ME   262
## 186                   Oxnard-Thousand Oaks-Ventura, CA   267
## 187                        Albany-Schenectady-Troy, NY   268
## 188                                         Dayton, OH   268
## 189                                        Madison, WI   284
## 190                                       Columbia, SC   291
## 191                                         Tucson, AZ   302
## 192                                         Fresno, CA   303
## 193                           Grand Rapids-Wyoming, MI   304
## 194                                      Rochester, NY   307
## 195                                     Provo-Orem, UT   309
## 196                                    Reno-Sparks, NV   310
## 197                                          Tulsa, OK   323
## 198                  Allentown-Bethlehem-Easton, PA-NJ   334
## 199                                   Raleigh-Cary, NC   336
## 200                          Buffalo-Niagara Falls, NY   344
## 201                                  Memphis, TN-MS-AR   348
## 202                    New Orleans-Metairie-Kenner, LA   367
## 203                               Colorado Springs, CO   372
## 204                              Birmingham-Hoover, AL   392
## 205                                   Jacksonville, FL   393
## 206                  Little Rock-North Little Rock, AR   404
## 207                               Ogden-Clearfield, UT   423
## 208                                        Wichita, KS   427
## 209                                       Fargo, ND-MN   432
## 210                                          Dover, DE   456
## 211                                       Richmond, VA   490
## 212                                     Des Moines, IA   501
## 213                Nashville-Davidson-Murfreesboro, TN   505
## 214                                      New Haven, CT   506
## 215                              Austin-Round Rock, TX   516
## 216                  Charlotte-Gastonia-Concord, NC-SC   517
## 217                                  Louisville, KY-IN   519
## 218                                       Columbus, OH   551
## 219                                   Indianapolis, IN   570
## 220                                    Sioux Falls, SD   595
## 221         Virginia Beach-Norfolk-Newport News, VA-NC   597
## 222                                  Oklahoma City, OK   604
## 223                                    San Antonio, TX   607
## 224                                    Albuquerque, NM   609
## 225                                        Orlando, FL   610
## 226                               Boise City-Nampa, ID   644
## 227                    Burlington-South Burlington, VT   657
## 228              Sacramento-Arden-Arcade-Roseville, CA   667
## 229                 San Jose-Sunnyvale-Santa Clara, CA   670
## 230                        Cleveland-Elyria-Mentor, OH   681
## 231                        Portland-South Portland, ME   701
## 232                  Milwaukee-Waukesha-West Allis, WI   714
## 233                    Cincinnati-Middletown, OH-KY-IN   719
## 234                                 Salt Lake City, UT   723
## 235                    Bridgeport-Stamford-Norwalk, CT   730
## 236                                     Pittsburgh, PA   732
## 237                Tampa-St. Petersburg-Clearwater, FL   842
## 238           Hartford-West Hartford-East Hartford, CT   885
## 239                  San Diego-Carlsbad-San Marcos, CA   907
## 240                                   St. Louis, MO-IL   956
## 241                        Omaha-Council Bluffs, NE-IA   957
## 242                                 Kansas City, MO-KS   962
## 243                        Phoenix-Mesa-Scottsdale, AZ   971
## 244                Portland-Vancouver-Beaverton, OR-WA  1089
## 245                        Seattle-Tacoma-Bellevue, WA  1255
## 246                       Riverside-San Bernardino, CA  1290
## 247                             Las Vegas-Paradise, NV  1299
## 248                         Detroit-Warren-Livonia, MI  1354
## 249                  San Francisco-Oakland-Fremont, CA  1386
## 250                               Baltimore-Towson, MD  1483
## 251                                  Denver-Aurora, CO  1504
## 252                 Atlanta-Sandy Springs-Marietta, GA  1552
## 253              Miami-Fort Lauderdale-Miami Beach, FL  1554
## 254                                       Honolulu, HI  1576
## 255                     Houston-Baytown-Sugar Land, TX  1649
## 256                    Dallas-Fort Worth-Arlington, TX  1863
## 257             Minneapolis-St Paul-Bloomington, MN-WI  1942
## 258                     Boston-Cambridge-Quincy, MA-NH  2229
## 259               Providence-Fall River-Warwick, MA-RI  2284
## 260                Chicago-Naperville-Joliet, IN-IN-WI  2772
## 261           Philadelphia-Camden-Wilmington, PA-NJ-DE  2855
## 262               Los Angeles-Long Beach-Santa Ana, CA  4102
## 263       Washington-Arlington-Alexandria, DC-VA-MD-WV  4177
## 264 New York-Northern New Jersey-Long Island, NY-NJ-PA  5409
```

```r
print(map_metroarea_df[grep("^Atlanta", map_metroarea_df$MetroArea),])
```

```
##     Code                          MetroArea
## 21 12060 Atlanta-Sandy Springs-Marietta, GA
```

```r
print(nrow(subset(entity_df, MetroAreaCode == "12060")))
```

```
## [1] 1552
```

```r
print(MetroArea_freq_entity_df[grep("^Atlanta", MetroArea_freq_entity_df$MetroArea),])
```

```
##                              MetroArea .freq
## 252 Atlanta-Sandy Springs-Marietta, GA  1552
```

```r
print(map_metroarea_df[grep("^Baltimore", map_metroarea_df$MetroArea),])
```

```
##     Code            MetroArea
## 26 12580 Baltimore-Towson, MD
```

```r
print(nrow(subset(entity_df, MetroAreaCode == "12580")))
```

```
## [1] 1483
```

```r
print(MetroArea_freq_entity_df[grep("^Baltimore", MetroArea_freq_entity_df$MetroArea),])
```

```
##                MetroArea .freq
## 250 Baltimore-Towson, MD  1483
```

```r
print(map_metroarea_df[grep("^Boston", map_metroarea_df$MetroArea),])
```

```
##      Code                      MetroArea
## 258 71650 Boston-Cambridge-Quincy, MA-NH
```

```r
print(nrow(subset(entity_df, MetroAreaCode == "71650")))
```

```
## [1] 2229
```

```r
print(MetroArea_freq_entity_df[grep("^Boston", MetroArea_freq_entity_df$MetroArea),])
```

```
##                          MetroArea .freq
## 258 Boston-Cambridge-Quincy, MA-NH  2229
```

```r
print(map_metroarea_df[grep("^San Francisco", map_metroarea_df$MetroArea),])
```

```
##      Code                         MetroArea
## 209 41860 San Francisco-Oakland-Fremont, CA
```

```r
print(nrow(subset(entity_df, MetroAreaCode == "41860")))
```

```
## [1] 1386
```

```r
print(MetroArea_freq_entity_df[grep("^San Francisco", MetroArea_freq_entity_df$MetroArea),])
```

```
##                             MetroArea .freq
## 249 San Francisco-Oakland-Fremont, CA  1386
```

```r
print(prblm_3_4_arr <- sort(tapply(entity_df$Hispanic, entity_df$MetroArea, mean, na.rm=TRUE)))
```

```
##                                       Anderson, SC 
##                                        0.000000000 
##                                      Ann Arbor, MI 
##                                        0.000000000 
##                                Barnstable Town, MA 
##                                        0.000000000 
##                              Bloomington-Normal IL 
##                                        0.000000000 
##                                    Bloomington, IN 
##                                        0.000000000 
##                                  Bowling Green, KY 
##                                        0.000000000 
##                                        Decatur, IL 
##                                        0.000000000 
##                                     Eau Claire, WI 
##                                        0.000000000 
##                                       Florence, AL 
##                                        0.000000000 
##                      Hagerstown-Martinsburg, MD-WV 
##                                        0.000000000 
##                                   Harrisonburg, VA 
##                                        0.000000000 
##                       Huntington-Ashland, WV-KY-OH 
##                                        0.000000000 
##                                     Huntsville, AL 
##                                        0.000000000 
##                                        Jackson, MI 
##                                        0.000000000 
##                                      Johnstown, PA 
##                                        0.000000000 
##                                          Macon, GA 
##                                        0.000000000 
##                                         Mobile, AL 
##                                        0.000000000 
##                                      Salisbury, MD 
##                                        0.000000000 
##                                       Savannah, GA 
##                                        0.000000000 
##                                  Warner Robins, GA 
##                                        0.000000000 
##                                         Dayton, OH 
##                                        0.003731343 
##                                         Monroe, LA 
##                                        0.005586592 
##                                      Knoxville, TN 
##                                        0.005952381 
##                                     Charleston, WV 
##                                        0.007633588 
##                                        Appleton,WI 
##                                        0.008000000 
##                                        Jackson, MS 
##                                        0.009009009 
##                    Burlington-South Burlington, VT 
##                                        0.009132420 
##                                     Montgomery, AL 
##                                        0.009708738 
##                                         Wausau, WI 
##                                        0.010416667 
##                        Portland-South Portland, ME 
##                                        0.011412268 
##                                 Oshkosh-Neenah, WI 
##                                        0.011764706 
##                                        Altoona, PA 
##                                        0.012195122 
##                                      St. Cloud, MN 
##                                        0.012195122 
##                            Holland-Grand Haven, MI 
##                                        0.012820513 
##                                          Akron, OH 
##                                        0.012987013 
##                                    Springfield, IL 
##                                        0.013157895 
##                                     Bellingham, WA 
##                                        0.014285714 
##                                         Bangor, ME 
##                                        0.014423077 
##                           Kingsport-Bristol, TN-VA 
##                                        0.014925373 
##                                   Cedar Rapids, IA 
##                                        0.015306122 
##                                Gulfport-Biloxi, MS 
##                                        0.015384615 
##                                      Duluth, MN-WI 
##                                        0.015873016 
##                                     Pittsburgh, PA 
##                                        0.016393443 
##                                         Joplin, MO 
##                                        0.016949153 
##                    Charleston-North Charleston, SC 
##                                        0.017241379 
##                          Buffalo-Niagara Falls, NY 
##                                        0.017441860 
##                                      La Crosse, WI 
##                                        0.017543860 
##                            Niles-Benton Harbor, MI 
##                                        0.019607843 
##                                  Evansville, IN-KY 
##                                        0.020202020 
##                                    Spartanburg, SC 
##                                        0.020202020 
##                                           Bend, OR 
##                                        0.021428571 
##                         Muskegon-Norton Shores, MI 
##                                        0.022222222 
##                                           Erie, PA 
##                                        0.022988506 
##                            Harrisburg-Carlisle, PA 
##                                        0.022988506 
##                                        Madison, WI 
##                                        0.024647887 
##                                   Lake Charles, LA 
##                                        0.024691358 
##                                       Fargo, ND-MN 
##                                        0.025462963 
##                                  Coeur d'Alene, ID 
##                                        0.025641026 
##                                        Spokane, WA 
##                                        0.025641026 
##                 Saginaw-Saginaw Township North, MI 
##                                        0.027027027 
##                                      Lynchburg, VA 
##                                        0.027397260 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                        0.028037383 
##                                  Memphis, TN-MS-AR 
##                                        0.028735632 
##                                    Springfield, OH 
##                                        0.029411765 
##                                       Billings, MT 
##                                        0.030150754 
##                                     Janesville, WI 
##                                        0.030303030 
##                                        Roanoke, VA 
##                                        0.030303030 
##                                   St. Louis, MO-IL 
##                                        0.030334728 
##                                      Iowa City, IA 
##                                        0.030534351 
##                             Rochester-Dover, NH-ME 
##                                        0.030534351 
##                              Kalamazoo-Portage, MI 
##                                        0.031496063 
##                     Youngstown-Warren-Boardman, OH 
##                                        0.032679739 
##                               Champaign-Urbana, IL 
##                                        0.032786885 
##                                         Toledo, OH 
##                                        0.034042553 
##                                     Fort Wayne, IN 
##                                        0.036764706 
##                  Little Rock-North Little Rock, AR 
##                                        0.037128713 
##                         Detroit-Warren-Livonia, MI 
##                                        0.037666174 
##                                     Greenville, SC 
##                                        0.037837838 
##                                    Baton Rouge, LA 
##                                        0.038167939 
##                                   Johnson City, TN 
##                                        0.038461538 
##                                  Louisville, KY-IN 
##                                        0.038535645 
##                         Michigan City-La Porte, IN 
##                                        0.038961039 
##                                          Flint, MI 
##                                        0.039215686 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                        0.040333797 
##                              Lexington-Fayette, KY 
##                                        0.040404040 
##                                       Lawrence, KS 
##                                        0.040816327 
##                        Albany-Schenectady-Troy, NY 
##                                        0.041044776 
##                                     Binghamton, NY 
##                                        0.041095890 
##                                    Punta Gorda, FL 
##                                        0.041666667 
##                                    Sioux Falls, SD 
##                                        0.042016807 
##                                       Columbia, MO 
##                                        0.042553191 
##                                   York-Hanover, PA 
##                                        0.042735043 
##                                    Gainesville, FL 
##                                        0.042857143 
##                                       Richmond, VA 
##                                        0.042857143 
##                                    Springfield, MO 
##                                        0.043478261 
##                                       Columbus, OH 
##                                        0.043557169 
##                                       Rockford, IL 
##                                        0.043859649 
##                                         Albany, GA 
##                                        0.044117647 
##                      Sarasota-Bradenton-Venice, FL 
##                                        0.046875000 
##                                       Valdosta, GA 
##                                        0.047619048 
##                                Anniston-Oxford, AL 
##                                        0.049180328 
##                        South Bend-Mishawaka, IN-MI 
##                                        0.049382716 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                        0.050251256 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                        0.052008239 
##                                        Decatur, Al 
##                                        0.052083333 
##                              Birmingham-Hoover, AL 
##                                        0.053571429 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                        0.053571429 
##                                  Winston-Salem, NC 
##                                        0.055118110 
##                                          Dover, DE 
##                                        0.057017544 
##                           Bremerton-Silverdale, WA 
##                                        0.057471264 
##                                      Rochester, NY 
##                                        0.058631922 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                        0.058823529 
##                                         Racine, WI 
##                                        0.058823529 
##                                       Honolulu, HI 
##                                        0.059644670 
##                        Cleveland-Elyria-Mentor, OH 
##                                        0.060205580 
##                                      Asheville, NC 
##                                        0.060344828 
##                                      Lafayette, LA 
##                                        0.060773481 
##                                         Peoria, IL 
##                                        0.062500000 
##                                         Monroe, MI 
##                                        0.063492063 
##                                       Anderson, IN 
##                                        0.064516129 
##                                     Provo-Orem, UT 
##                                        0.064724919 
##                                     Ocean City, NJ 
##                                        0.066666667 
##                         Panama City-Lynn Haven, FL 
##                                        0.067796610 
##                                       Kingston, NY 
##                                        0.068965517 
##                Nashville-Davidson-Murfreesboro, TN 
##                                        0.069306931 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                        0.069537909 
##                                    Tallahassee, FL 
##                                        0.069767442 
##                        Omaha-Council Bluffs, NE-IA 
##                                        0.070010449 
##                                   Indianapolis, IN 
##                                        0.071929825 
##                                      New Haven, CT 
##                                        0.073122530 
##                                     Des Moines, IA 
##                                        0.073852295 
##                                     Utica-Rome, NY 
##                                        0.075000000 
##                          Greensboro-High Point, NC 
##                                        0.075697211 
##                                     Vero Beach, FL 
##                                        0.075949367 
##                               Canton-Massillon, OH 
##                                        0.076271186 
##                             Eugene-Springfield, OR 
##                                        0.076530612 
##                                 Chattanooga, TN-GA 
##                                        0.077844311 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                        0.078458844 
##                                       Columbia, SC 
##                                        0.079037801 
##                                       Syracuse, NY 
##                                        0.080717489 
##                        Shreveport-Bossier City, LA 
##                                        0.082191781 
##                               Baltimore-Towson, MD 
##                                        0.082265678 
##                                   Worcester, MA-CT 
##                                        0.083333333 
##                           Lansing-East Lansing, MI 
##                                        0.084033613 
##                                        Medford, OR 
##                                        0.085365854 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                        0.085434174 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                        0.085695876 
##                                  Fort Smith, AR-OK 
##                                        0.085714286 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                        0.086826347 
##                      Hickory-Morgantown-Lenoir, NC 
##                                        0.087719298 
##                        Seattle-Tacoma-Bellevue, WA 
##                                        0.088446215 
##                                  Atlantic City, NJ 
##                                        0.090090090 
##                   Leominster-Fitchburg-Gardner, MA 
##                                        0.090909091 
##                                   Jacksonville, FL 
##                                        0.091603053 
##                Davenport-Moline-Rock Island, IA-IL 
##                                        0.091666667 
##                     Augusta-Richmond County, GA-SC 
##                                        0.093167702 
##                               Boise City-Nampa, ID 
##                                        0.093167702 
##                                         Topeka, KS 
##                                        0.093406593 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                        0.094582185 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                        0.100000000 
##                     Port St. Lucie-Fort Pierce, FL 
##                                        0.100917431 
##                                      Lancaster, PA 
##                                        0.102564103 
##                                     Tuscaloosa, AL 
##                                        0.102564103 
##                          Norwich-New London, CT-RI 
##                                        0.103448276 
##           Hartford-West Hartford-East Hartford, CT 
##                                        0.105084746 
##                                  Oklahoma City, OK 
##                                        0.107615894 
##                           Waterloo-Cedar Falls, IA 
##                                        0.108974359 
##                                         Durham, NC 
##                                        0.111111111 
##                    New Orleans-Metairie-Kenner, LA 
##                                        0.111716621 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                        0.112328767 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                        0.112500000 
##               Providence-Fall River-Warwick, MA-RI 
##                                        0.114273205 
##                                          Tulsa, OK 
##                                        0.114551084 
##                               Kankakee-Bradley, IL 
##                                        0.114942529 
##                                          Chico, CA 
##                                        0.116666667 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                        0.117988395 
##                                   Raleigh-Cary, NC 
##                                        0.119047619 
##                               Colorado Springs, CO 
##                                        0.120967742 
##                                        Olympia, WA 
##                                        0.121212121 
##                          Fort Collins-Loveland, CO 
##                                        0.121359223 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                        0.121378980 
##                                 Kansas City, MO-KS 
##                                        0.121621622 
##                            Athens-Clark County, GA 
##                                        0.123076923 
##                                         Lawton, OK 
##                                        0.123711340 
##                                      Green Bay, WI 
##                                        0.125000000 
##                                   Jacksonville, NC 
##                                        0.126984127 
##                                       Prescott, AZ 
##                                        0.129629630 
##                                  Trenton-Ewing, NJ 
##                                        0.131868132 
##                                        Wichita, KS 
##                                        0.133489461 
##                          Lakeland-Winter Haven, FL 
##                                        0.134228188 
##                          Scranton-Wilkes Barre, PA 
##                                        0.136363636 
##                           Grand Rapids-Wyoming, MI 
##                                        0.138157895 
##                               Ogden-Clearfield, UT 
##                                        0.144208038 
##                                        Boulder, CO 
##                                        0.146198830 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                        0.148837209 
##                         Santa-Cruz-Watsonville, CA 
##                                        0.151515152 
##                                 Salt Lake City, UT 
##                                        0.154910097 
##                                   Fayetteville, NC 
##                                        0.155844156 
##                                          Ocala, FL 
##                                        0.157894737 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                        0.159144893 
##                                        Greeley, CO 
##                                        0.160493827 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                        0.167388167 
##                            Naples-Marco Island, FL 
##                                        0.182926829 
##                                    Reno-Sparks, NV 
##                                        0.196774194 
##                  San Francisco-Oakland-Fremont, CA 
##                                        0.199855700 
##                                    Columbus, GA-AL 
##                                        0.203389831 
##                              Vallejo-Fairfield, CA 
##                                        0.210526316 
##                                        Reading, PA 
##                                        0.211267606 
##                                          Salem, OR 
##                                        0.211764706 
##                                        Orlando, FL 
##                                        0.213114754 
##                                 Springfield, MA-CT 
##                                        0.219354839 
##                           Beaumont-Port Author, TX 
##                                        0.227642276 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                        0.228508042 
##                                           Napa, CA 
##                                        0.229508197 
##                                  Denver-Aurora, CO 
##                                        0.232047872 
##                            Santa Rosa-Petaluma, CA 
##                                        0.232558140 
##                                     Farmington, NM 
##                                        0.234375000 
##                    San Luis Obispo-Paso Robles, CA 
##                                        0.246753247 
##                                      Waterbury, CT 
##                                        0.248407643 
##                             Las Vegas-Paradise, NV 
##                                        0.251732102 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                        0.254376931 
##                                       Amarillo, TX 
##                                        0.261363636 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                        0.263868066 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                        0.269018743 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                        0.273631841 
##                    Dallas-Fort Worth-Arlington, TX 
##                                        0.283950617 
##                                        Lubbock, TX 
##                                        0.285714286 
##                                       Longview, TX 
##                                        0.292307692 
##                                         Pueblo, CO 
##                                        0.307692308 
##                              Austin-Round Rock, TX 
##                                        0.310077519 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                        0.316417910 
##                                       Stockton, CA 
##                                        0.321243523 
##                                           Waco, TX 
##                                        0.329113924 
##                                        Danbury, CT 
##                                        0.339285714 
##                                        Modesto, CA 
##                                        0.341772152 
##                                        Midland, TX 
##                                        0.352941176 
##                                         Yakima, WA 
##                                        0.357142857 
##                     Houston-Baytown-Sugar Land, TX 
##                                        0.359005458 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                        0.359550562 
##                       Killeen-Temple-Fort Hood, TX 
##                                        0.386138614 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                        0.401515152 
##                   Vineland-Millville-Bridgeton, NJ 
##                                        0.407407407 
##                                         Fresno, CA 
##                                        0.409240924 
##                            Visalia-Porterville, CA 
##                                        0.438016529 
##                          Cape Coral-Fort Myers, FL 
##                                        0.438356164 
##                                    Albuquerque, NM 
##                                        0.441707718 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                        0.460263286 
##                                       Santa Fe, NM 
##                                        0.461538462 
##                                       Victoria, TX 
##                                        0.465517241 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                        0.467824968 
##                                    Bakersfield, CA 
##                                        0.489795918 
##                       Riverside-San Bernardino, CA 
##                                        0.502325581 
##                                         Tucson, AZ 
##                                        0.506622517 
##                                     Las Cruses, NM 
##                                        0.542056075 
##                                        Salinas, CA 
##                                        0.557692308 
##                                         Merced, CA 
##                                        0.566037736 
##                                 Corpus Christi, TX 
##                                        0.606060606 
##                                         Madera, CA 
##                                        0.614035088 
##                                    San Antonio, TX 
##                                        0.644151565 
##                                      El Centro, CA 
##                                        0.686868687 
##                                        El Paso, TX 
##                                        0.790983607 
##                          Brownsville-Harlingen, TX 
##                                        0.797468354 
##                         McAllen-Edinburg-Pharr, TX 
##                                        0.948717949 
##                                         Laredo, TX 
##                                        0.966292135
```

```r
print(prblm_3_5_arr <- sort(tapply(entity_df$Race == "Asian", entity_df$MetroArea, mean, na.rm=TRUE)))
```

```
##                                         Albany, GA 
##                                        0.000000000 
##                                        Altoona, PA 
##                                        0.000000000 
##                                       Amarillo, TX 
##                                        0.000000000 
##                                       Anderson, IN 
##                                        0.000000000 
##                                        Appleton,WI 
##                                        0.000000000 
##                                      Asheville, NC 
##                                        0.000000000 
##                                Barnstable Town, MA 
##                                        0.000000000 
##                           Beaumont-Port Author, TX 
##                                        0.000000000 
##                                       Billings, MT 
##                                        0.000000000 
##                                     Binghamton, NY 
##                                        0.000000000 
##                                    Bloomington, IN 
##                                        0.000000000 
##                                  Bowling Green, KY 
##                                        0.000000000 
##                               Canton-Massillon, OH 
##                                        0.000000000 
##                                     Charleston, WV 
##                                        0.000000000 
##                                          Chico, CA 
##                                        0.000000000 
##                                    Columbus, GA-AL 
##                                        0.000000000 
##                                        Decatur, IL 
##                                        0.000000000 
##                                         Durham, NC 
##                                        0.000000000 
##                                     Eau Claire, WI 
##                                        0.000000000 
##                                        El Paso, TX 
##                                        0.000000000 
##                                           Erie, PA 
##                                        0.000000000 
##                                     Farmington, NM 
##                                        0.000000000 
##                                       Florence, AL 
##                                        0.000000000 
##                      Hagerstown-Martinsburg, MD-WV 
##                                        0.000000000 
##                                     Huntsville, AL 
##                                        0.000000000 
##                                        Jackson, MI 
##                                        0.000000000 
##                                        Jackson, MS 
##                                        0.000000000 
##                                     Janesville, WI 
##                                        0.000000000 
##                                   Johnson City, TN 
##                                        0.000000000 
##                                         Joplin, MO 
##                                        0.000000000 
##                               Kankakee-Bradley, IL 
##                                        0.000000000 
##                       Killeen-Temple-Fort Hood, TX 
##                                        0.000000000 
##                           Kingsport-Bristol, TN-VA 
##                                        0.000000000 
##                                      Knoxville, TN 
##                                        0.000000000 
##                                      Lafayette, LA 
##                                        0.000000000 
##                           Lansing-East Lansing, MI 
##                                        0.000000000 
##                                         Laredo, TX 
##                                        0.000000000 
##                   Leominster-Fitchburg-Gardner, MA 
##                                        0.000000000 
##                                       Longview, TX 
##                                        0.000000000 
##                                        Lubbock, TX 
##                                        0.000000000 
##                                      Lynchburg, VA 
##                                        0.000000000 
##                                          Macon, GA 
##                                        0.000000000 
##                                         Madera, CA 
##                                        0.000000000 
##                         McAllen-Edinburg-Pharr, TX 
##                                        0.000000000 
##                         Michigan City-La Porte, IN 
##                                        0.000000000 
##                                        Midland, TX 
##                                        0.000000000 
##                                         Monroe, MI 
##                                        0.000000000 
##                         Muskegon-Norton Shores, MI 
##                                        0.000000000 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                        0.000000000 
##                            Niles-Benton Harbor, MI 
##                                        0.000000000 
##                                     Ocean City, NJ 
##                                        0.000000000 
##                                 Oshkosh-Neenah, WI 
##                                        0.000000000 
##                     Port St. Lucie-Fort Pierce, FL 
##                                        0.000000000 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                        0.000000000 
##                                         Pueblo, CO 
##                                        0.000000000 
##                                    Punta Gorda, FL 
##                                        0.000000000 
##                                         Racine, WI 
##                                        0.000000000 
##                                        Reading, PA 
##                                        0.000000000 
##                                        Roanoke, VA 
##                                        0.000000000 
##                                       Rockford, IL 
##                                        0.000000000 
##                 Saginaw-Saginaw Township North, MI 
##                                        0.000000000 
##                                          Salem, OR 
##                                        0.000000000 
##                                      Salisbury, MD 
##                                        0.000000000 
##                                       Santa Fe, NM 
##                                        0.000000000 
##                         Santa-Cruz-Watsonville, CA 
##                                        0.000000000 
##                          Scranton-Wilkes Barre, PA 
##                                        0.000000000 
##                        Shreveport-Bossier City, LA 
##                                        0.000000000 
##                        South Bend-Mishawaka, IN-MI 
##                                        0.000000000 
##                                    Spartanburg, SC 
##                                        0.000000000 
##                                 Springfield, MA-CT 
##                                        0.000000000 
##                                    Springfield, OH 
##                                        0.000000000 
##                                      St. Cloud, MN 
##                                        0.000000000 
##                                    Tallahassee, FL 
##                                        0.000000000 
##                                     Tuscaloosa, AL 
##                                        0.000000000 
##                                     Utica-Rome, NY 
##                                        0.000000000 
##                                       Valdosta, GA 
##                                        0.000000000 
##                                     Vero Beach, FL 
##                                        0.000000000 
##                                       Victoria, TX 
##                                        0.000000000 
##                   Vineland-Millville-Bridgeton, NJ 
##                                        0.000000000 
##                                           Waco, TX 
##                                        0.000000000 
##                                      Waterbury, CT 
##                                        0.000000000 
##                                         Wausau, WI 
##                                        0.000000000 
##                                   St. Louis, MO-IL 
##                                        0.002092050 
##                    New Orleans-Metairie-Kenner, LA 
##                                        0.002724796 
##                                    San Antonio, TX 
##                                        0.003294893 
##                    Charleston-North Charleston, SC 
##                                        0.004310345 
##                                         Monroe, LA 
##                                        0.005586592 
##                                 Chattanooga, TN-GA 
##                                        0.005988024 
##                                        Modesto, CA 
##                                        0.006329114 
##                                           Bend, OR 
##                                        0.007142857 
##                                         Dayton, OH 
##                                        0.007462687 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                        0.007575758 
##                            Santa Rosa-Petaluma, CA 
##                                        0.007751938 
##                                         Toledo, OH 
##                                        0.008510638 
##                                  Coeur d'Alene, ID 
##                                        0.008547009 
##                                   York-Hanover, PA 
##                                        0.008547009 
##                                         Yakima, WA 
##                                        0.008928571 
##                           Grand Rapids-Wyoming, MI 
##                                        0.009868421 
##                                    Sioux Falls, SD 
##                                        0.010084034 
##                                  Evansville, IN-KY 
##                                        0.010101010 
##                                       Lawrence, KS 
##                                        0.010204082 
##                        Cleveland-Elyria-Mentor, OH 
##                                        0.010279001 
##                                         Lawton, OK 
##                                        0.010309278 
##                               Boise City-Nampa, ID 
##                                        0.010869565 
##                            Harrisburg-Carlisle, PA 
##                                        0.011494253 
##                                       Kingston, NY 
##                                        0.011494253 
##                                  Louisville, KY-IN 
##                                        0.011560694 
##                                        Medford, OR 
##                                        0.012195122 
##                                        Greeley, CO 
##                                        0.012345679 
##                                    Springfield, MO 
##                                        0.012422360 
##                              Birmingham-Hoover, AL 
##                                        0.012755102 
##                           Waterloo-Cedar Falls, IA 
##                                        0.012820513 
##                                     Provo-Orem, UT 
##                                        0.012944984 
##                     Youngstown-Warren-Boardman, OH 
##                                        0.013071895 
##                                          Ocala, FL 
##                                        0.013157895 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                        0.014970060 
##                                 Corpus Christi, TX 
##                                        0.015151515 
##                                          Dover, DE 
##                                        0.015350877 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                        0.015473888 
##                      Sarasota-Bradenton-Venice, FL 
##                                        0.015625000 
##                              Kalamazoo-Portage, MI 
##                                        0.015748031 
##                                  Winston-Salem, NC 
##                                        0.015748031 
##                                      Johnstown, PA 
##                                        0.015873016 
##                               Colorado Springs, CO 
##                                        0.016129032 
##                               Champaign-Urbana, IL 
##                                        0.016393443 
##                                           Napa, CA 
##                                        0.016393443 
##                         Panama City-Lynn Haven, FL 
##                                        0.016949153 
##                                  Memphis, TN-MS-AR 
##                                        0.017241379 
##                                       Columbus, OH 
##                                        0.018148820 
##                                       Prescott, AZ 
##                                        0.018518519 
##                                     Las Cruses, NM 
##                                        0.018691589 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                        0.018691589 
##                                        Spokane, WA 
##                                        0.019230769 
##                          Fort Collins-Loveland, CO 
##                                        0.019417476 
##                                          Flint, MI 
##                                        0.019607843 
##                                       Savannah, GA 
##                                        0.019801980 
##                                         Tucson, AZ 
##                                        0.019867550 
##                                      El Centro, CA 
##                                        0.020202020 
##                             Eugene-Springfield, OR 
##                                        0.020408163 
##                Davenport-Moline-Rock Island, IA-IL 
##                                        0.020833333 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                        0.021428571 
##                                         Topeka, KS 
##                                        0.021978022 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                        0.022253129 
##                  Little Rock-North Little Rock, AR 
##                                        0.022277228 
##                        Albany-Schenectady-Troy, NY 
##                                        0.022388060 
##                                    Baton Rouge, LA 
##                                        0.022900763 
##                           Bremerton-Silverdale, WA 
##                                        0.022988506 
##                                         Bangor, ME 
##                                        0.024038462 
##                            Naples-Marco Island, FL 
##                                        0.024390244 
##                                   Indianapolis, IN 
##                                        0.024561404 
##                     Augusta-Richmond County, GA-SC 
##                                        0.024844720 
##                            Holland-Grand Haven, MI 
##                                        0.025641026 
##                                   Fayetteville, NC 
##                                        0.025974026 
##                               Ogden-Clearfield, UT 
##                                        0.026004728 
##                             Rochester-Dover, NH-ME 
##                                        0.026717557 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                        0.026800670 
##                          Lakeland-Winter Haven, FL 
##                                        0.026845638 
##                                       Columbia, SC 
##                                        0.027491409 
##                                       Fargo, ND-MN 
##                                        0.027777778 
##                                     Bellingham, WA 
##                                        0.028571429 
##                                     Montgomery, AL 
##                                        0.029126214 
##                        Omaha-Council Bluffs, NE-IA 
##                                        0.029258098 
##                                          Akron, OH 
##                                        0.030303030 
##                                        Wichita, KS 
##                                        0.030444965 
##                            Athens-Clark County, GA 
##                                        0.030769231 
##                                Gulfport-Biloxi, MS 
##                                        0.030769231 
##                                       Anderson, SC 
##                                        0.031250000 
##                                  Denver-Aurora, CO 
##                                        0.031914894 
##                                     Greenville, SC 
##                                        0.032432432 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                        0.032924694 
##                                   Harrisonburg, VA 
##                                        0.033333333 
##                          Cape Coral-Fort Myers, FL 
##                                        0.034246575 
##                                 Kansas City, MO-KS 
##                                        0.034303534 
##                                   Worcester, MA-CT 
##                                        0.034722222 
##                                  Oklahoma City, OK 
##                                        0.034768212 
##                      Hickory-Morgantown-Lenoir, NC 
##                                        0.035087719 
##                              Lexington-Fayette, KY 
##                                        0.035353535 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                        0.035392535 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                        0.035714286 
##                                 Salt Lake City, UT 
##                                        0.035961272 
##                                         Mobile, AL 
##                                        0.036363636 
##                       Huntington-Ashland, WV-KY-OH 
##                                        0.036585366 
##                                       Richmond, VA 
##                                        0.036734694 
##                                     Fort Wayne, IN 
##                                        0.036764706 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                        0.037500000 
##                                     Des Moines, IA 
##                                        0.037924152 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                        0.038105046 
##                                     Pittsburgh, PA 
##                                        0.038251366 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                        0.038356164 
##               Providence-Fall River-Warwick, MA-RI 
##                                        0.038966725 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                        0.039192399 
##                                      Duluth, MN-WI 
##                                        0.039682540 
##                                       Syracuse, NY 
##                                        0.040358744 
##                                    Albuquerque, NM 
##                                        0.041050903 
##                                        Decatur, Al 
##                                        0.041666667 
##                        Portland-South Portland, ME 
##                                        0.042796006 
##                                    Gainesville, FL 
##                                        0.042857143 
##                         Detroit-Warren-Livonia, MI 
##                                        0.043574594 
##                                  Trenton-Ewing, NJ 
##                                        0.043956044 
##                                      New Haven, CT 
##                                        0.047430830 
##                                   Jacksonville, NC 
##                                        0.047619048 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                        0.047619048 
##                                   Jacksonville, FL 
##                                        0.048346056 
##                    Burlington-South Burlington, VT 
##                                        0.048706240 
##                                Anniston-Oxford, AL 
##                                        0.049180328 
##                                          Tulsa, OK 
##                                        0.049535604 
##                                   Raleigh-Cary, NC 
##                                        0.050595238 
##                                        Orlando, FL 
##                                        0.050819672 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                        0.051162791 
##                    San Luis Obispo-Paso Robles, CA 
##                                        0.051948052 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                        0.052041274 
##                              Austin-Round Rock, TX 
##                                        0.052325581 
##                          Buffalo-Niagara Falls, NY 
##                                        0.052325581 
##                                    Springfield, IL 
##                                        0.052631579 
##                                      Iowa City, IA 
##                                        0.053435115 
##                                         Peoria, IL 
##                                        0.053571429 
##                                        Madison, WI 
##                                        0.056338028 
##                                         Merced, CA 
##                                        0.056603774 
##                                  Fort Smith, AR-OK 
##                                        0.057142857 
##                Nashville-Davidson-Murfreesboro, TN 
##                                        0.057425743 
##                                      Lancaster, PA 
##                                        0.057692308 
##                               Baltimore-Towson, MD 
##                                        0.057990560 
##                                    Reno-Sparks, NV 
##                                        0.058064516 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                        0.058441558 
##                                        Boulder, CO 
##                                        0.058479532 
##                     Houston-Baytown-Sugar Land, TX 
##                                        0.061249242 
##                       Riverside-San Bernardino, CA 
##                                        0.062015504 
##                                        Danbury, CT 
##                                        0.062500000 
##                    Dallas-Fort Worth-Arlington, TX 
##                                        0.062801932 
##                                       Columbia, MO 
##                                        0.063829787 
##                                      Rochester, NY 
##                                        0.065146580 
##                                   Cedar Rapids, IA 
##                                        0.066326531 
##           Hartford-West Hartford-East Hartford, CT 
##                                        0.066666667 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                        0.069788797 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                        0.070624850 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                        0.072809278 
##                          Norwich-New London, CT-RI 
##                                        0.073891626 
##                                   Lake Charles, LA 
##                                        0.074074074 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                        0.074906367 
##                              Bloomington-Normal IL 
##                                        0.075000000 
##                          Brownsville-Harlingen, TX 
##                                        0.075949367 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                        0.076725026 
##                             Las Vegas-Paradise, NV 
##                                        0.078521940 
##                          Greensboro-High Point, NC 
##                                        0.079681275 
##                                    Bakersfield, CA 
##                                        0.081632653 
##                                      Ann Arbor, MI 
##                                        0.082352941 
##                                      La Crosse, WI 
##                                        0.087719298 
##                                      Green Bay, WI 
##                                        0.088235294 
##                            Visalia-Porterville, CA 
##                                        0.090909091 
##                        Seattle-Tacoma-Bellevue, WA 
##                                        0.099601594 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                        0.104270660 
##                                        Salinas, CA 
##                                        0.125000000 
##                                        Olympia, WA 
##                                        0.131313131 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                        0.135056070 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                        0.142227122 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                        0.142428786 
##                                  Atlantic City, NJ 
##                                        0.144144144 
##                                       Stockton, CA 
##                                        0.155440415 
##                                  Warner Robins, GA 
##                                        0.166666667 
##                                         Fresno, CA 
##                                        0.184818482 
##                              Vallejo-Fairfield, CA 
##                                        0.203007519 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                        0.241791045 
##                  San Francisco-Oakland-Fremont, CA 
##                                        0.246753247 
##                                       Honolulu, HI 
##                                        0.501903553
```

```r
print(prblm_3_6_arr <- sort(tapply(entity_df$Education == "No high school diploma", entity_df$MetroArea, mean, na.rm=TRUE), decreasing=TRUE))
```

```
##                                          Macon, GA 
##                                         0.40816327 
##                                       Longview, TX 
##                                         0.38297872 
##                         McAllen-Edinburg-Pharr, TX 
##                                         0.38297872 
##                           Kingsport-Bristol, TN-VA 
##                                         0.36363636 
##                                         Laredo, TX 
##                                         0.34426230 
##                                        Salinas, CA 
##                                         0.34090909 
##                                         Madera, CA 
##                                         0.33333333 
##                                       Florence, AL 
##                                         0.32075472 
##                                    Springfield, OH 
##                                         0.31034483 
##                                        El Paso, TX 
##                                         0.30219780 
##                                 Corpus Christi, TX 
##                                         0.29702970 
##                                         Merced, CA 
##                                         0.28358209 
##                                         Lawton, OK 
##                                         0.28000000 
##                   Vineland-Millville-Bridgeton, NJ 
##                                         0.27500000 
##                                    Bakersfield, CA 
##                                         0.27218935 
##                                      Lancaster, PA 
##                                         0.26771654 
##                          Brownsville-Harlingen, TX 
##                                         0.25396825 
##                                       Stockton, CA 
##                                         0.25333333 
##                                         Joplin, MO 
##                                         0.25000000 
##                                      Lafayette, LA 
##                                         0.24822695 
##                                         Tucson, AZ 
##                                         0.24603175 
##                                     Montgomery, AL 
##                                         0.24137931 
##                                           Waco, TX 
##                                         0.24074074 
##                       Killeen-Temple-Fort Hood, TX 
##                                         0.24050633 
##                                     Farmington, NM 
##                                         0.23913043 
##                       Riverside-San Bernardino, CA 
##                                         0.23780488 
##                     Youngstown-Warren-Boardman, OH 
##                                         0.23622047 
##                           Beaumont-Port Author, TX 
##                                         0.23469388 
##                               Kankakee-Bradley, IL 
##                                         0.23437500 
##                                   Harrisonburg, VA 
##                                         0.23287671 
##                         Panama City-Lynn Haven, FL 
##                                         0.22916667 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                         0.22882883 
##                      Hickory-Morgantown-Lenoir, NC 
##                                         0.22448980 
##                      Hagerstown-Martinsburg, MD-WV 
##                                         0.22222222 
##                                         Yakima, WA 
##                                         0.22222222 
##                                    San Antonio, TX 
##                                         0.22004357 
##                            Visalia-Porterville, CA 
##                                         0.21782178 
##                                   Lake Charles, LA 
##                                         0.21739130 
##                                         Fresno, CA 
##                                         0.21120690 
##                    New Orleans-Metairie-Kenner, LA 
##                                         0.21088435 
##                                        Midland, TX 
##                                         0.21052632 
##                                       Anderson, SC 
##                                         0.20689655 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                         0.20657277 
##                     Houston-Baytown-Sugar Land, TX 
##                                         0.20439739 
##                                        Modesto, CA 
##                                         0.20325203 
##                                    Springfield, MO 
##                                         0.20000000 
##                  Little Rock-North Little Rock, AR 
##                                         0.19939577 
##                                      Waterbury, CT 
##                                         0.19852941 
##                     Port St. Lucie-Fort Pierce, FL 
##                                         0.19767442 
##                                       Kingston, NY 
##                                         0.19696970 
##                                    Columbus, GA-AL 
##                                         0.19607843 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                         0.19393939 
##                        South Bend-Mishawaka, IN-MI 
##                                         0.19354839 
##                                          Dover, DE 
##                                         0.19220056 
##                                         Monroe, LA 
##                                         0.19205298 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                         0.19191919 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                         0.19136961 
##                    Dallas-Fort Worth-Arlington, TX 
##                                         0.19077135 
##                                    Spartanburg, SC 
##                                         0.18987342 
##                                         Toledo, OH 
##                                         0.18965517 
##                                   Raleigh-Cary, NC 
##                                         0.18959108 
##                        Shreveport-Bossier City, LA 
##                                         0.18918919 
##                                     Charleston, WV 
##                                         0.18834081 
##                     Augusta-Richmond County, GA-SC 
##                                         0.18796992 
##                                         Albany, GA 
##                                         0.18604651 
##                                        Decatur, Al 
##                                         0.18421053 
##                          Greensboro-High Point, NC 
##                                         0.18357488 
##                           Lansing-East Lansing, MI 
##                                         0.18348624 
##                                     Janesville, WI 
##                                         0.18292683 
##                                   Johnson City, TN 
##                                         0.18181818 
##                                        Wichita, KS 
##                                         0.18181818 
##                                   York-Hanover, PA 
##                                         0.18181818 
##                Nashville-Davidson-Murfreesboro, TN 
##                                         0.18112245 
##                                          Salem, OR 
##                                         0.17985612 
##                              Vallejo-Fairfield, CA 
##                                         0.17924528 
##                                        Reading, PA 
##                                         0.17857143 
##                                 Springfield, MA-CT 
##                                         0.17829457 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                         0.17773788 
##                                        Jackson, MI 
##                                         0.17741935 
##                                      El Centro, CA 
##                                         0.17567568 
##                                           Erie, PA 
##                                         0.17567568 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                         0.17444717 
##                                  Fort Smith, AR-OK 
##                                         0.17441860 
##                                     Huntsville, AL 
##                                         0.17391304 
##                                     Utica-Rome, NY 
##                                         0.17391304 
##                                Gulfport-Biloxi, MS 
##                                         0.17307692 
##                                     Las Cruses, NM 
##                                         0.17283951 
##                                       Rockford, IL 
##                                         0.17021277 
##                                     Tuscaloosa, AL 
##                                         0.16949153 
##                         Muskegon-Norton Shores, MI 
##                                         0.16923077 
##               Providence-Fall River-Warwick, MA-RI 
##                                         0.16915688 
##                                         Bangor, ME 
##                                         0.16860465 
##                                      Green Bay, WI 
##                                         0.16831683 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                         0.16687737 
##                                       Amarillo, TX 
##                                         0.16666667 
##                                Anniston-Oxford, AL 
##                                         0.16666667 
##                            Athens-Clark County, GA 
##                                         0.16666667 
##                                     Binghamton, NY 
##                                         0.16666667 
##                          Cape Coral-Fort Myers, FL 
##                                         0.16528926 
##                                    Albuquerque, NM 
##                                         0.16424116 
##                                   Indianapolis, IN 
##                                         0.16371681 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                         0.16356589 
##                                         Durham, NC 
##                                         0.16326531 
##                                       Columbia, MO 
##                                         0.16279070 
##                        Cleveland-Elyria-Mentor, OH 
##                                         0.16250000 
##                                  Warner Robins, GA 
##                                         0.16216216 
##                                        Orlando, FL 
##                                         0.16108787 
##                                  Memphis, TN-MS-AR 
##                                         0.15714286 
##                                       Columbus, OH 
##                                         0.15617716 
##                                       Columbia, SC 
##                                         0.15600000 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                         0.15573586 
##                                       Syracuse, NY 
##                                         0.15428571 
##                                 Chattanooga, TN-GA 
##                                         0.15217391 
##                                         Dayton, OH 
##                                         0.15207373 
##                                        Jackson, MS 
##                                         0.15168539 
##                       Huntington-Ashland, WV-KY-OH 
##                                         0.15151515 
##                                      St. Cloud, MN 
##                                         0.15151515 
##                                        Lubbock, TX 
##                                         0.15094340 
##                          Norwich-New London, CT-RI 
##                                         0.15060241 
##                                     Bellingham, WA 
##                                         0.15000000 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                         0.14922481 
##                              Lexington-Fayette, KY 
##                                         0.14838710 
##                         Santa-Cruz-Watsonville, CA 
##                                         0.14814815 
##                                  Trenton-Ewing, NJ 
##                                         0.14814815 
##                 Saginaw-Saginaw Township North, MI 
##                                         0.14754098 
##                              Birmingham-Hoover, AL 
##                                         0.14678899 
##                                     Greenville, SC 
##                                         0.14666667 
##                               Boise City-Nampa, ID 
##                                         0.14653465 
##                  San Francisco-Oakland-Fremont, CA 
##                                         0.14651368 
##                                        Greeley, CO 
##                                         0.14615385 
##           Hartford-West Hartford-East Hartford, CT 
##                                         0.14574899 
##                                  Denver-Aurora, CO 
##                                         0.14574558 
##                                         Monroe, MI 
##                                         0.14545455 
##                                        Decatur, IL 
##                                         0.14516129 
##                                    Springfield, IL 
##                                         0.14516129 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                         0.14421553 
##                                 Salt Lake City, UT 
##                                         0.14338235 
##                                  Atlantic City, NJ 
##                                         0.14285714 
##                            Holland-Grand Haven, MI 
##                                         0.14285714 
##                                        Medford, OR 
##                                         0.14285714 
##                            Naples-Marco Island, FL 
##                                         0.14285714 
##                                    Punta Gorda, FL 
##                                         0.14285714 
##                                       Victoria, TX 
##                                         0.14285714 
##                                  Winston-Salem, NC 
##                                         0.14285714 
##                                   Jacksonville, FL 
##                                         0.14244186 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                         0.14188267 
##                                  Oklahoma City, OK 
##                                         0.14137214 
##                   Leominster-Fitchburg-Gardner, MA 
##                                         0.14035088 
##                                          Ocala, FL 
##                                         0.13888889 
##                                    Sioux Falls, SD 
##                                         0.13832200 
##                                         Peoria, IL 
##                                         0.13829787 
##                                      Lynchburg, VA 
##                                         0.13793103 
##                                  Louisville, KY-IN 
##                                         0.13785047 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                         0.13737734 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                         0.13693694 
##                          Buffalo-Niagara Falls, NY 
##                                         0.13684211 
##                               Baltimore-Towson, MD 
##                                         0.13583333 
##                               Ogden-Clearfield, UT 
##                                         0.13571429 
##                            Niles-Benton Harbor, MI 
##                                         0.13513514 
##                                       Anderson, IN 
##                                         0.13461538 
##                                          Chico, CA 
##                                         0.13461538 
##                                   St. Louis, MO-IL 
##                                         0.13461538 
##                                     Provo-Orem, UT 
##                                         0.13366337 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                         0.13333333 
##                             Las Vegas-Paradise, NV 
##                                         0.13307985 
##                            Harrisburg-Carlisle, PA 
##                                         0.13286713 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                         0.13178295 
##                                          Tulsa, OK 
##                                         0.13178295 
##                                       Valdosta, GA 
##                                         0.13157895 
##                                         Wausau, WI 
##                                         0.13157895 
##                                    Bloomington, IN 
##                                         0.13095238 
##                                        Danbury, CT 
##                                         0.13043478 
##                                       Savannah, GA 
##                                         0.13013699 
##                                       Richmond, VA 
##                                         0.12990196 
##                        Omaha-Council Bluffs, NE-IA 
##                                         0.12972973 
##                         Detroit-Warren-Livonia, MI 
##                                         0.12964642 
##                                     Des Moines, IA 
##                                         0.12944162 
##                                         Racine, WI 
##                                         0.12903226 
##                                    Baton Rouge, LA 
##                                         0.12871287 
##                                         Pueblo, CO 
##                                         0.12844037 
##                           Waterloo-Cedar Falls, IA 
##                                         0.12800000 
##                Davenport-Moline-Rock Island, IA-IL 
##                                         0.12727273 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                         0.12717253 
##                                   Worcester, MA-CT 
##                                         0.12605042 
##                                   Fayetteville, NC 
##                                         0.12500000 
##                    San Luis Obispo-Paso Robles, CA 
##                                         0.12500000 
##                               Canton-Massillon, OH 
##                                         0.12371134 
##                                      New Haven, CT 
##                                         0.12354312 
##                                      Duluth, MN-WI 
##                                         0.12264151 
##                                           Napa, CA 
##                                         0.12244898 
##                                 Kansas City, MO-KS 
##                                         0.12172775 
##                        Seattle-Tacoma-Bellevue, WA 
##                                         0.12168793 
##                                      Rochester, NY 
##                                         0.12132353 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                         0.11929825 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                         0.11909651 
##                                       Santa Fe, NM 
##                                         0.11904762 
##                                        Appleton,WI 
##                                         0.11827957 
##                               Colorado Springs, CO 
##                                         0.11764706 
##                                        Olympia, WA 
##                                         0.11764706 
##                                    Reno-Sparks, NV 
##                                         0.11764706 
##                          Scranton-Wilkes Barre, PA 
##                                         0.11724138 
##                                         Topeka, KS 
##                                         0.11724138 
##                                         Mobile, AL 
##                                         0.11702128 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                         0.11683748 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                         0.11657143 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                         0.11638204 
##                                   Cedar Rapids, IA 
##                                         0.11564626 
##                                          Flint, MI 
##                                         0.11538462 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                         0.11475410 
##                      Sarasota-Bradenton-Venice, FL 
##                                         0.11464968 
##                                     Vero Beach, FL 
##                                         0.11428571 
##                                           Bend, OR 
##                                         0.11111111 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                         0.11080485 
##                             Eugene-Springfield, OR 
##                                         0.11038961 
##                         Michigan City-La Porte, IN 
##                                         0.10769231 
##                                       Honolulu, HI 
##                                         0.10739300 
##                        Portland-South Portland, ME 
##                                         0.10638298 
##                           Grand Rapids-Wyoming, MI 
##                                         0.10612245 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                         0.10579710 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                         0.10559006 
##                                        Spokane, WA 
##                                         0.10434783 
##                                  Evansville, IN-KY 
##                                         0.10389610 
##                            Santa Rosa-Petaluma, CA 
##                                         0.10280374 
##                                       Prescott, AZ 
##                                         0.10204082 
##                                        Roanoke, VA 
##                                         0.10169492 
##                                     Fort Wayne, IN 
##                                         0.09900990 
##                    Charleston-North Charleston, SC 
##                                         0.09890110 
##                                        Boulder, CO 
##                                         0.09701493 
##                                      La Crosse, WI 
##                                         0.09677419 
##                              Austin-Round Rock, TX 
##                                         0.09629630 
##                                      Johnstown, PA 
##                                         0.09615385 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                         0.09563758 
##                                Barnstable Town, MA 
##                                         0.09090909 
##                                     Pittsburgh, PA 
##                                         0.09060403 
##                                      Knoxville, TN 
##                                         0.08965517 
##                             Rochester-Dover, NH-ME 
##                                         0.08928571 
##                                 Oshkosh-Neenah, WI 
##                                         0.08823529 
##                                      Ann Arbor, MI 
##                                         0.08695652 
##                                      Asheville, NC 
##                                         0.08695652 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                         0.08695652 
##                                          Akron, OH 
##                                         0.08421053 
##                    Burlington-South Burlington, VT 
##                                         0.08394161 
##                                  Coeur d'Alene, ID 
##                                         0.08333333 
##                                       Billings, MT 
##                                         0.08280255 
##                          Lakeland-Winter Haven, FL 
##                                         0.08130081 
##                                     Ocean City, NJ 
##                                         0.08000000 
##                        Albany-Schenectady-Troy, NY 
##                                         0.07929515 
##                                       Fargo, ND-MN 
##                                         0.07902736 
##                                    Tallahassee, FL 
##                                         0.07500000 
##                                        Madison, WI 
##                                         0.07423581 
##                                        Altoona, PA 
##                                         0.07142857 
##                          Fort Collins-Loveland, CO 
##                                         0.06936416 
##                                    Gainesville, FL 
##                                         0.06896552 
##                                      Salisbury, MD 
##                                         0.06779661 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                         0.06666667 
##                                     Eau Claire, WI 
##                                         0.06250000 
##                                   Jacksonville, NC 
##                                         0.06122449 
##                              Bloomington-Normal IL 
##                                         0.06060606 
##                                       Lawrence, KS 
##                                         0.05952381 
##                           Bremerton-Silverdale, WA 
##                                         0.05405405 
##                               Champaign-Urbana, IL 
##                                         0.05154639 
##                              Kalamazoo-Portage, MI 
##                                         0.05050505 
##                                  Bowling Green, KY 
##                                         0.03703704 
##                                      Iowa City, IA 
##                                         0.02912621
```

```r
map_country_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/CountryCodes.csv", 
    comment="map_metroarea_df", print_diagn=TRUE)
```

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
##     Code                        Country
## 64   215                          Japan
## 88   303                         Mexico
## 102  330                        Grenada
## 107  340 St. Vincent and the Grenadines
## 117  369                       Paraguay
## 124  407                       Cameroon
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
##  - attr(*, "comment")= chr "map_metroarea_df"
## NULL
```

```r
# entity_tmp_df <- mymap_codes(entity_df, "CountryOfBirthCode", "CountryOfBirth", 
#         			map_country_df, map_join_col_name="Code", map_tgt_col_name="Country")
entity_df <- mymap_codes(entity_df, "CountryOfBirthCode", "Country", 
            		map_country_df, map_join_col_name="Code")
```

```
##   CountryOfBirthCode       Country     _n
## 1                 57 United States 115063
## 2                303        Mexico   3921
## 3                233   Philippines    839
## 4                210         India    770
## 5                207         China    581
## 6                 73   Puerto Rico    518
##     CountryOfBirthCode         Country _n
## 61                 205 Myanmar (Burma) 45
## 78                 311      Costa Rica 29
## 96                 246      Uzbekistan 19
## 130                310          Belize  9
## 137                106         Denmark  6
## 155                423            <NA>  3
##     CountryOfBirthCode          Country _n
## 156                425             <NA>  3
## 157                142 Northern Ireland  2
## 158                228             <NA>  2
## 159                453         Tanzania  2
## 160                430             <NA>  1
## 161                460             <NA>  1
```

![](us_cps_files/figure-html/encode_data_1-2.png) 

```r
print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
```

```
## CountryOfBirthCode      MetroAreaCode  PeopleInHousehold 
##                  0              34238                  0 
##             Region              State                Age 
##                  0                  0                  0 
##            Married                Sex          Education 
##              25338                  0              25338 
##               Race           Hispanic        Citizenship 
##                  0                  0                  0 
##   EmploymentStatus           Industry   MetroAreaCode.NA 
##              25789              65060                  0 
##          MetroArea            Country 
##              34238                176
```

```r
print(Country_freq_entity_df <- mycreate_tbl_df(entity_df, "Country"))
```

```
##                            Country  .freq
## 1                 Northern Ireland      2
## 2                         Tanzania      2
## 3                       Azerbaijan      3
## 4                   Czechoslovakia      3
## 5                 St. Kitts--Nevis      3
## 6                          Georgia      5
## 7                         Barbados      6
## 8                          Denmark      6
## 9                           Latvia      6
## 10                           Samoa      6
## 11                         Senegal      6
## 12                       Singapore      6
## 13                        Slovakia      6
## 14                           Tonga      6
## 15                        Zimbabwe      6
## 16    South America, not specified      7
## 17                       St. Lucia      7
## 18                         Algeria      9
## 19         Americas, not specified      9
## 20                          Belize      9
## 21                            Fiji      9
## 22  St. Vincent and the Grenadines      9
## 23                         Bahamas     10
## 24                         Finland     10
## 25                          Kuwait     10
## 26                       Lithuania     10
## 27                  Czech Republic     11
## 28                        Dominica     11
## 29                        Paraguay     11
## 30                         Croatia     12
## 31                       Macedonia     12
## 32                         Moldova     12
## 33             Antigua and Barbuda     13
## 34                         Belgium     13
## 35                         Bermuda     13
## 36                         Bolivia     13
## 37                         Grenada     13
## 38                           Sudan     13
## 39                      Cape Verde     15
## 40                         Eritrea     15
## 41                    Sierra Leone     15
## 42                          Uganda     15
## 43                         Austria     17
## 44                         Morocco     17
## 45                       Sri Lanka     17
## 46            U. S. Virgin Islands     17
## 47                         Uruguay     17
## 48                         Albania     18
## 49                          Norway     18
## 50           Europe, not specified     19
## 51                      Uzbekistan     19
## 52      West Indies, not specified     19
## 53                        Malaysia     20
## 54                          Serbia     20
## 55                          Azores     22
## 56                            USSR     22
## 57                     New Zealand     23
## 58                     Switzerland     23
## 59                           Yemen     23
## 60                         Belarus     24
## 61                        Scotland     24
## 62                      Yugoslavia     24
## 63                         Hungary     25
## 64                     Afghanistan     26
## 65                       Indonesia     26
## 66                     Netherlands     28
## 67                          Sweden     28
## 68                        Bulgaria     29
## 69                      Costa Rica     29
## 70                    Saudi Arabia     29
## 71                            Guam     31
## 72                        Cameroon     32
## 73                           Syria     32
## 74                         Armenia     35
## 75                          Jordan     36
## 76                           Chile     37
## 77             Asia, not specified     39
## 78                         Ireland     39
## 79                           Spain     41
## 80                      Bangladesh     42
## 81                       Australia     43
## 82                           Nepal     44
## 83                          Panama     44
## 84                         Lebanon     45
## 85                 Myanmar (Burma)     45
## 86                    South Africa     48
## 87                          Turkey     48
## 88                        Cambodia     49
## 89                         Liberia     52
## 90                           Kenya     55
## 91                         Romania     55
## 92                          Greece     56
## 93                          Israel     57
## 94             Trinidad and Tobago     60
## 95            Bosnia & Herzegovina     61
## 96                       Venezuela     61
## 97                       Argentina     64
## 98                       Hong Kong     64
## 99                        Portugal     64
## 100                          Egypt     65
## 101                        Somalia     72
## 102                         France     73
## 103                    South Korea     73
## 104                          Ghana     76
## 105                      Nicaragua     76
## 106                       Ethiopia     80
## 107                      Elsewhere     81
## 108                        Nigeria     85
## 109                           Iraq     97
## 110                           Laos     98
## 111                         Taiwan    102
## 112                        Ukraine    104
## 113                         Guyana    109
## 114                       Pakistan    109
## 115                 United Kingdom    111
## 116                       Thailand    128
## 117          Africa, not specified    129
## 118                        Ecuador    136
## 119                           Peru    136
## 120                           Iran    144
## 121                          Italy    149
## 122                         Brazil    159
## 123                         Poland    162
## 124                          Haiti    167
## 125                         Russia    173
## 126                        England    179
## 127                          Japan    187
## 128                       Honduras    189
## 129                       Columbia    206
## 130                        Jamaica    217
## 131                      Guatemala    309
## 132             Dominican Republic    330
## 133                          Korea    334
## 134                         Canada    410
## 135                           Cuba    426
## 136                        Germany    438
## 137                        Vietnam    458
## 138                    El Salvador    477
## 139                    Puerto Rico    518
## 140                          China    581
## 141                          India    770
## 142                    Philippines    839
## 143                         Mexico   3921
## 144                  United States 115063
```

```r
print(Country_na_by_MetroArea_arr <- 
    sort(tapply(entity_df$Country != "United States", entity_df$MetroArea, mean, na.rm=TRUE)))
```

```
##                                        Decatur, IL 
##                                        0.000000000 
##                                       Florence, AL 
##                                        0.000000000 
##                           Kingsport-Bristol, TN-VA 
##                                        0.000000000 
##                                        Lubbock, TX 
##                                        0.000000000 
##                                          Macon, GA 
##                                        0.000000000 
##                                         Monroe, MI 
##                                        0.000000000 
##                                     Ocean City, NJ 
##                                        0.000000000 
##                 Saginaw-Saginaw Township North, MI 
##                                        0.000000000 
##                                     Charleston, WV 
##                                        0.003816794 
##                                         Racine, WI 
##                                        0.008403361 
##                                        Jackson, MS 
##                                        0.009009009 
##                                     Eau Claire, WI 
##                                        0.009090909 
##                                       Billings, MT 
##                                        0.010050251 
##                                  Evansville, IN-KY 
##                                        0.010101010 
##                                     Janesville, WI 
##                                        0.010101010 
##                                         Wausau, WI 
##                                        0.010416667 
##                                      Knoxville, TN 
##                                        0.011904762 
##                                        Altoona, PA 
##                                        0.012195122 
##                         Michigan City-La Porte, IN 
##                                        0.012987013 
##                                         Albany, GA 
##                                        0.014705882 
##                                       Savannah, GA 
##                                        0.014851485 
##                                       Anderson, SC 
##                                        0.015625000 
##                              Kalamazoo-Portage, MI 
##                                        0.015748031 
##                                      Duluth, MN-WI 
##                                        0.015873016 
##                                      Johnstown, PA 
##                                        0.015873016 
##                                        Appleton,WI 
##                                        0.016000000 
##                                      Lafayette, LA 
##                                        0.016574586 
##                                         Monroe, LA 
##                                        0.016759777 
##                                       Victoria, TX 
##                                        0.017241379 
##                                    Bloomington, IN 
##                                        0.019230769 
##                            Niles-Benton Harbor, MI 
##                                        0.019607843 
##                                       Columbia, MO 
##                                        0.021276596 
##                                           Bend, OR 
##                                        0.021428571 
##                                   Harrisonburg, VA 
##                                        0.022222222 
##                                         Pueblo, CO 
##                                        0.023076923 
##                                       Valdosta, GA 
##                                        0.023809524 
##                        South Bend-Mishawaka, IN-MI 
##                                        0.024691358 
##                                   Cedar Rapids, IA 
##                                        0.025510204 
##                                   York-Hanover, PA 
##                                        0.025641026 
##                                         Dayton, OH 
##                                        0.026119403 
##                                      Salisbury, MD 
##                                        0.027027027 
##                                         Mobile, AL 
##                                        0.027272727 
##                                     Bellingham, WA 
##                                        0.028571429 
##                                        Jackson, MI 
##                                        0.028571429 
##                Davenport-Moline-Rock Island, IA-IL 
##                                        0.029166667 
##                                          Flint, MI 
##                                        0.029411765 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                        0.029411765 
##                                    Springfield, OH 
##                                        0.029411765 
##                                         Toledo, OH 
##                                        0.029787234 
##                                        Roanoke, VA 
##                                        0.030303030 
##                               Canton-Massillon, OH 
##                                        0.033898305 
##                                         Joplin, MO 
##                                        0.033898305 
##                         Panama City-Lynn Haven, FL 
##                                        0.033898305 
##                          Fort Collins-Loveland, CO 
##                                        0.033980583 
##                    Charleston-North Charleston, SC 
##                                        0.034482759 
##                                          Akron, OH 
##                                        0.034934498 
##                                 Oshkosh-Neenah, WI 
##                                        0.035294118 
##                              Birmingham-Hoover, AL 
##                                        0.035714286 
##                                     Pittsburgh, PA 
##                                        0.035714286 
##                       Huntington-Ashland, WV-KY-OH 
##                                        0.036585366 
##                                   Lake Charles, LA 
##                                        0.037037037 
##                                         Bangor, ME 
##                                        0.038461538 
##                            Holland-Grand Haven, MI 
##                                        0.038461538 
##                                   Johnson City, TN 
##                                        0.038461538 
##                                       Santa Fe, NM 
##                                        0.038461538 
##                                     Montgomery, AL 
##                                        0.038834951 
##                            Harrisburg-Carlisle, PA 
##                                        0.040229885 
##                                    Sioux Falls, SD 
##                                        0.040609137 
##                                   St. Louis, MO-IL 
##                                        0.040794979 
##                                       Lawrence, KS 
##                                        0.040816327 
##                                     Binghamton, NY 
##                                        0.041095890 
##                                      Lynchburg, VA 
##                                        0.041095890 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                        0.041876047 
##                                     Huntsville, AL 
##                                        0.042735043 
##                                    Springfield, MO 
##                                        0.043478261 
##                                      La Crosse, WI 
##                                        0.043859649 
##                                       Rockford, IL 
##                                        0.043859649 
##                                         Topeka, KS 
##                                        0.043956044 
##                  Little Rock-North Little Rock, AR 
##                                        0.044554455 
##                           Waterloo-Cedar Falls, IA 
##                                        0.044871795 
##                             Eugene-Springfield, OR 
##                                        0.045918367 
##                              Lexington-Fayette, KY 
##                                        0.045918367 
##                               Kankakee-Bradley, IL 
##                                        0.045977011 
##                      Hagerstown-Martinsburg, MD-WV 
##                                        0.046511628 
##                                    Tallahassee, FL 
##                                        0.046511628 
##                                     Farmington, NM 
##                                        0.046875000 
##                                       Anderson, IN 
##                                        0.048387097 
##                                    Baton Rouge, LA 
##                                        0.049618321 
##                          Scranton-Wilkes Barre, PA 
##                                        0.051136364 
##                                         Lawton, OK 
##                                        0.051546392 
##                             Rochester-Dover, NH-ME 
##                                        0.053435115 
##                                         Peoria, IL 
##                                        0.053571429 
##                               Colorado Springs, CO 
##                                        0.053763441 
##                                          Dover, DE 
##                                        0.054824561 
##                                     Provo-Orem, UT 
##                                        0.055016181 
##                                        Greeley, CO 
##                                        0.055555556 
##                                       Prescott, AZ 
##                                        0.055555556 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                        0.056074766 
##                                       Amarillo, TX 
##                                        0.056818182 
##                      Sarasota-Bradenton-Venice, FL 
##                                        0.057291667 
##                               Champaign-Urbana, IL 
##                                        0.057377049 
##                                           Erie, PA 
##                                        0.057471264 
##                                       Kingston, NY 
##                                        0.057471264 
##                                       Fargo, ND-MN 
##                                        0.058411215 
##                        Omaha-Council Bluffs, NE-IA 
##                                        0.058761805 
##                                  Coeur d'Alene, ID 
##                                        0.059829060 
##                   Leominster-Fitchburg-Gardner, MA 
##                                        0.060606061 
##                         Santa-Cruz-Watsonville, CA 
##                                        0.060606061 
##                                       Richmond, VA 
##                                        0.061224490 
##                                   Indianapolis, IN 
##                                        0.061403509 
##                                Gulfport-Biloxi, MS 
##                                        0.061538462 
##                     Augusta-Richmond County, GA-SC 
##                                        0.062111801 
##                                        Decatur, Al 
##                                        0.062500000 
##                                   Jacksonville, NC 
##                                        0.063492063 
##                               Boise City-Nampa, ID 
##                                        0.063664596 
##                        Cleveland-Elyria-Mentor, OH 
##                                        0.064610866 
##                    Burlington-South Burlington, VT 
##                                        0.064914992 
##                           Grand Rapids-Wyoming, MI 
##                                        0.065789474 
##                                     Fort Wayne, IN 
##                                        0.066176471 
##                                  Fort Smith, AR-OK 
##                                        0.066666667 
##                                       Syracuse, NY 
##                                        0.067264574 
##                                  Louisville, KY-IN 
##                                        0.067829457 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                        0.068150209 
##                                 Corpus Christi, TX 
##                                        0.068181818 
##                                      Asheville, NC 
##                                        0.068965517 
##                                  Bowling Green, KY 
##                                        0.068965517 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                        0.070028011 
##                      Hickory-Morgantown-Lenoir, NC 
##                                        0.070175439 
##                          Buffalo-Niagara Falls, NY 
##                                        0.071641791 
##                                 Chattanooga, TN-GA 
##                                        0.071856287 
##                                        Medford, OR 
##                                        0.073170732 
##                     Port St. Lucie-Fort Pierce, FL 
##                                        0.073394495 
##                               Ogden-Clearfield, UT 
##                                        0.074162679 
##                        Portland-South Portland, ME 
##                                        0.074391989 
##                              Bloomington-Normal IL 
##                                        0.075000000 
##                        Shreveport-Bossier City, LA 
##                                        0.075342466 
##                           Lansing-East Lansing, MI 
##                                        0.075630252 
##                                      Iowa City, IA 
##                                        0.076335878 
##                                  Memphis, TN-MS-AR 
##                                        0.077586207 
##                         Muskegon-Norton Shores, MI 
##                                        0.077777778 
##                                   Fayetteville, NC 
##                                        0.077922078 
##                                  Winston-Salem, NC 
##                                        0.078740157 
##                                    Springfield, IL 
##                                        0.078947368 
##                           Bremerton-Silverdale, WA 
##                                        0.080459770 
##                                          Tulsa, OK 
##                                        0.080495356 
##                                    Spartanburg, SC 
##                                        0.080808081 
##                        Albany-Schenectady-Troy, NY 
##                                        0.082089552 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                        0.083832335 
##                     Youngstown-Warren-Boardman, OH 
##                                        0.084967320 
##                                       Columbus, OH 
##                                        0.086080586 
##                    New Orleans-Metairie-Kenner, LA 
##                                        0.087193460 
##                                     Vero Beach, FL 
##                                        0.088607595 
##                                     Des Moines, IA 
##                                        0.089820359 
##                           Beaumont-Port Author, TX 
##                                        0.090163934 
##                                 Kansas City, MO-KS 
##                                        0.090436590 
##                                        Madison, WI 
##                                        0.091549296 
##                                         Yakima, WA 
##                                        0.091743119 
##                                       Columbia, SC 
##                                        0.092783505 
##                                Barnstable Town, MA 
##                                        0.093333333 
##                                     Las Cruses, NM 
##                                        0.093457944 
##                                  Oklahoma City, OK 
##                                        0.094370861 
##                Nashville-Davidson-Murfreesboro, TN 
##                                        0.094758065 
##                                        Wichita, KS 
##                                        0.096018735 
##                                Anniston-Oxford, AL 
##                                        0.098360656 
##                                     Greenville, SC 
##                                        0.098360656 
##                          Greensboro-High Point, NC 
##                                        0.099601594 
##                                          Chico, CA 
##                                        0.100000000 
##                                          Salem, OR 
##                                        0.100000000 
##                                    Albuquerque, NM 
##                                        0.100164204 
##                          Lakeland-Winter Haven, FL 
##                                        0.100671141 
##                                      New Haven, CT 
##                                        0.100790514 
##                                        Spokane, WA 
##                                        0.102564103 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                        0.104448743 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                        0.105853488 
##                            Athens-Clark County, GA 
##                                        0.107692308 
##                            Santa Rosa-Petaluma, CA 
##                                        0.108527132 
##                                         Durham, NC 
##                                        0.111111111 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                        0.111971104 
##                                     Utica-Rome, NY 
##                                        0.112500000 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                        0.114285714 
##                                    Gainesville, FL 
##                                        0.114285714 
##                                   Jacksonville, FL 
##                                        0.114503817 
##                                     Tuscaloosa, AL 
##                                        0.115384615 
##                                 Springfield, MA-CT 
##                                        0.116129032 
##                    San Luis Obispo-Paso Robles, CA 
##                                        0.116883117 
##                                      Ann Arbor, MI 
##                                        0.117647059 
##                                        Midland, TX 
##                                        0.117647059 
##                          Norwich-New London, CT-RI 
##                                        0.118226601 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                        0.119402985 
##                               Baltimore-Towson, MD 
##                                        0.120189061 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                        0.122754491 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                        0.125000000 
##                                      Green Bay, WI 
##                                        0.125000000 
##                                   Raleigh-Cary, NC 
##                                        0.125000000 
##                                           Waco, TX 
##                                        0.126582278 
##                                      Lancaster, PA 
##                                        0.128205128 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                        0.128757290 
##                                 Salt Lake City, UT 
##                                        0.128808864 
##                                         Tucson, AZ 
##                                        0.129139073 
##                                      Rochester, NY 
##                                        0.130293160 
##                         Detroit-Warren-Livonia, MI 
##                                        0.130723781 
##                                          Ocala, FL 
##                                        0.131578947 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                        0.136029412 
##                                  Denver-Aurora, CO 
##                                        0.136303191 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                        0.138095238 
##                                        Boulder, CO 
##                                        0.140350877 
##                                         Madera, CA 
##                                        0.140350877 
##                                  Warner Robins, GA 
##                                        0.142857143 
##                                    San Antonio, TX 
##                                        0.144975288 
##                                    Punta Gorda, FL 
##                                        0.145833333 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                        0.146278317 
##                                      St. Cloud, MN 
##                                        0.146341463 
##                                           Napa, CA 
##                                        0.147540984 
##                   Vineland-Millville-Bridgeton, NJ 
##                                        0.148148148 
##               Providence-Fall River-Warwick, MA-RI 
##                                        0.148880105 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                        0.152420185 
##                                   Worcester, MA-CT 
##                                        0.152777778 
##                              Austin-Round Rock, TX 
##                                        0.155038760 
##           Hartford-West Hartford-East Hartford, CT 
##                                        0.161581921 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                        0.166468490 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                        0.166787004 
##                                  Atlantic City, NJ 
##                                        0.171171171 
##                        Seattle-Tacoma-Bellevue, WA 
##                                        0.173982442 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                        0.174242424 
##                                        Reading, PA 
##                                        0.176056338 
##                                        Orlando, FL 
##                                        0.177049180 
##                    Dallas-Fort Worth-Arlington, TX 
##                                        0.180010747 
##                              Vallejo-Fairfield, CA 
##                                        0.180451128 
##                                        Olympia, WA 
##                                        0.181818182 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                        0.183520599 
##                                        Modesto, CA 
##                                        0.183544304 
##                                    Reno-Sparks, NV 
##                                        0.187096774 
##                       Killeen-Temple-Fort Hood, TX 
##                                        0.188118812 
##                                         Fresno, CA 
##                                        0.198019802 
##                                       Honolulu, HI 
##                                        0.200000000 
##                             Las Vegas-Paradise, NV 
##                                        0.200617284 
##                                      Waterbury, CT 
##                                        0.203821656 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                        0.203898051 
##                     Houston-Baytown-Sugar Land, TX 
##                                        0.207443563 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                        0.210980580 
##                            Visalia-Porterville, CA 
##                                        0.214876033 
##                                       Longview, TX 
##                                        0.215384615 
##                       Riverside-San Bernardino, CA 
##                                        0.217829457 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                        0.220082531 
##                                    Columbus, GA-AL 
##                                        0.220338983 
##                                         Merced, CA 
##                                        0.226415094 
##                            Naples-Marco Island, FL 
##                                        0.243902439 
##                                    Bakersfield, CA 
##                                        0.248979592 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                        0.256890849 
##                         McAllen-Edinburg-Pharr, TX 
##                                        0.266666667 
##                                        Salinas, CA 
##                                        0.269230769 
##                                        Danbury, CT 
##                                        0.279279279 
##                  San Francisco-Oakland-Fremont, CA 
##                                        0.293352601 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                        0.308660252 
##                                       Stockton, CA 
##                                        0.310880829 
##                                         Laredo, TX 
##                                        0.314606742 
##                                        El Paso, TX 
##                                        0.315573770 
##                                  Trenton-Ewing, NJ 
##                                        0.318681319 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                        0.329850746 
##                                      El Centro, CA 
##                                        0.333333333 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                        0.334878049 
##                          Cape Coral-Fort Myers, FL 
##                                        0.349315068 
##                          Brownsville-Harlingen, TX 
##                                        0.392405063 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                        0.409909910
```

```r
print(India_by_MetroArea_arr <- 
    sort(tapply(entity_df$Country == "India", entity_df$MetroArea, sum, na.rm=TRUE)))
```

```
##                                          Akron, OH 
##                                                  0 
##                        Albany-Schenectady-Troy, NY 
##                                                  0 
##                                         Albany, GA 
##                                                  0 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                                  0 
##                                        Altoona, PA 
##                                                  0 
##                                       Amarillo, TX 
##                                                  0 
##                                       Anderson, IN 
##                                                  0 
##                                      Ann Arbor, MI 
##                                                  0 
##                                Anniston-Oxford, AL 
##                                                  0 
##                                        Appleton,WI 
##                                                  0 
##                                      Asheville, NC 
##                                                  0 
##                            Athens-Clark County, GA 
##                                                  0 
##                     Augusta-Richmond County, GA-SC 
##                                                  0 
##                                         Bangor, ME 
##                                                  0 
##                                Barnstable Town, MA 
##                                                  0 
##                                    Baton Rouge, LA 
##                                                  0 
##                           Beaumont-Port Author, TX 
##                                                  0 
##                                     Bellingham, WA 
##                                                  0 
##                                           Bend, OR 
##                                                  0 
##                                       Billings, MT 
##                                                  0 
##                                     Binghamton, NY 
##                                                  0 
##                                    Bloomington, IN 
##                                                  0 
##                                        Boulder, CO 
##                                                  0 
##                                  Bowling Green, KY 
##                                                  0 
##                           Bremerton-Silverdale, WA 
##                                                  0 
##                          Buffalo-Niagara Falls, NY 
##                                                  0 
##                               Canton-Massillon, OH 
##                                                  0 
##                          Cape Coral-Fort Myers, FL 
##                                                  0 
##                                   Cedar Rapids, IA 
##                                                  0 
##                               Champaign-Urbana, IL 
##                                                  0 
##                                     Charleston, WV 
##                                                  0 
##                                 Chattanooga, TN-GA 
##                                                  0 
##                                          Chico, CA 
##                                                  0 
##                                  Coeur d'Alene, ID 
##                                                  0 
##                               Colorado Springs, CO 
##                                                  0 
##                                       Columbia, MO 
##                                                  0 
##                                    Columbus, GA-AL 
##                                                  0 
##                                       Columbus, OH 
##                                                  0 
##                                 Corpus Christi, TX 
##                                                  0 
##                                        Danbury, CT 
##                                                  0 
##                Davenport-Moline-Rock Island, IA-IL 
##                                                  0 
##                                         Dayton, OH 
##                                                  0 
##                                        Decatur, Al 
##                                                  0 
##                                        Decatur, IL 
##                                                  0 
##                                  Denver-Aurora, CO 
##                                                  0 
##                                          Dover, DE 
##                                                  0 
##                                      Duluth, MN-WI 
##                                                  0 
##                                         Durham, NC 
##                                                  0 
##                                     Eau Claire, WI 
##                                                  0 
##                                      El Centro, CA 
##                                                  0 
##                                        El Paso, TX 
##                                                  0 
##                                           Erie, PA 
##                                                  0 
##                             Eugene-Springfield, OR 
##                                                  0 
##                                  Evansville, IN-KY 
##                                                  0 
##                                       Fargo, ND-MN 
##                                                  0 
##                                     Farmington, NM 
##                                                  0 
##                                   Fayetteville, NC 
##                                                  0 
##                                          Flint, MI 
##                                                  0 
##                                       Florence, AL 
##                                                  0 
##                          Fort Collins-Loveland, CO 
##                                                  0 
##                                  Fort Smith, AR-OK 
##                                                  0 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                                  0 
##                                    Gainesville, FL 
##                                                  0 
##                           Grand Rapids-Wyoming, MI 
##                                                  0 
##                                        Greeley, CO 
##                                                  0 
##                                      Green Bay, WI 
##                                                  0 
##                          Greensboro-High Point, NC 
##                                                  0 
##                                Gulfport-Biloxi, MS 
##                                                  0 
##                      Hagerstown-Martinsburg, MD-WV 
##                                                  0 
##                                   Harrisonburg, VA 
##                                                  0 
##                      Hickory-Morgantown-Lenoir, NC 
##                                                  0 
##                            Holland-Grand Haven, MI 
##                                                  0 
##                       Huntington-Ashland, WV-KY-OH 
##                                                  0 
##                                     Huntsville, AL 
##                                                  0 
##                                        Jackson, MI 
##                                                  0 
##                                        Jackson, MS 
##                                                  0 
##                                   Jacksonville, NC 
##                                                  0 
##                                     Janesville, WI 
##                                                  0 
##                                   Johnson City, TN 
##                                                  0 
##                                      Johnstown, PA 
##                                                  0 
##                                         Joplin, MO 
##                                                  0 
##                              Kalamazoo-Portage, MI 
##                                                  0 
##                               Kankakee-Bradley, IL 
##                                                  0 
##                       Killeen-Temple-Fort Hood, TX 
##                                                  0 
##                           Kingsport-Bristol, TN-VA 
##                                                  0 
##                                       Kingston, NY 
##                                                  0 
##                                      Knoxville, TN 
##                                                  0 
##                                      La Crosse, WI 
##                                                  0 
##                                      Lafayette, LA 
##                                                  0 
##                                   Lake Charles, LA 
##                                                  0 
##                          Lakeland-Winter Haven, FL 
##                                                  0 
##                                      Lancaster, PA 
##                                                  0 
##                           Lansing-East Lansing, MI 
##                                                  0 
##                                         Laredo, TX 
##                                                  0 
##                                     Las Cruses, NM 
##                                                  0 
##                                         Lawton, OK 
##                                                  0 
##                   Leominster-Fitchburg-Gardner, MA 
##                                                  0 
##                              Lexington-Fayette, KY 
##                                                  0 
##                                       Longview, TX 
##                                                  0 
##                                  Louisville, KY-IN 
##                                                  0 
##                                        Lubbock, TX 
##                                                  0 
##                                      Lynchburg, VA 
##                                                  0 
##                                          Macon, GA 
##                                                  0 
##                                         Madera, CA 
##                                                  0 
##                         McAllen-Edinburg-Pharr, TX 
##                                                  0 
##                                        Medford, OR 
##                                                  0 
##                                         Merced, CA 
##                                                  0 
##                         Michigan City-La Porte, IN 
##                                                  0 
##                                        Midland, TX 
##                                                  0 
##                                         Mobile, AL 
##                                                  0 
##                                        Modesto, CA 
##                                                  0 
##                                         Monroe, LA 
##                                                  0 
##                                         Monroe, MI 
##                                                  0 
##                                     Montgomery, AL 
##                                                  0 
##                         Muskegon-Norton Shores, MI 
##                                                  0 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                                  0 
##                                           Napa, CA 
##                                                  0 
##                            Niles-Benton Harbor, MI 
##                                                  0 
##                                          Ocala, FL 
##                                                  0 
##                                     Ocean City, NJ 
##                                                  0 
##                                 Oshkosh-Neenah, WI 
##                                                  0 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                                  0 
##                         Panama City-Lynn Haven, FL 
##                                                  0 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                                  0 
##                     Port St. Lucie-Fort Pierce, FL 
##                                                  0 
##                        Portland-South Portland, ME 
##                                                  0 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                                  0 
##                                       Prescott, AZ 
##                                                  0 
##                                         Pueblo, CO 
##                                                  0 
##                                    Punta Gorda, FL 
##                                                  0 
##                                         Racine, WI 
##                                                  0 
##                                   Raleigh-Cary, NC 
##                                                  0 
##                                        Reading, PA 
##                                                  0 
##                                       Richmond, VA 
##                                                  0 
##                       Riverside-San Bernardino, CA 
##                                                  0 
##                                        Roanoke, VA 
##                                                  0 
##                                       Rockford, IL 
##                                                  0 
##                 Saginaw-Saginaw Township North, MI 
##                                                  0 
##                                          Salem, OR 
##                                                  0 
##                                        Salinas, CA 
##                                                  0 
##                                      Salisbury, MD 
##                                                  0 
##                                    San Antonio, TX 
##                                                  0 
##                    San Luis Obispo-Paso Robles, CA 
##                                                  0 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                                  0 
##                                       Santa Fe, NM 
##                                                  0 
##                            Santa Rosa-Petaluma, CA 
##                                                  0 
##                         Santa-Cruz-Watsonville, CA 
##                                                  0 
##                      Sarasota-Bradenton-Venice, FL 
##                                                  0 
##                                       Savannah, GA 
##                                                  0 
##                          Scranton-Wilkes Barre, PA 
##                                                  0 
##                        Shreveport-Bossier City, LA 
##                                                  0 
##                                    Sioux Falls, SD 
##                                                  0 
##                        South Bend-Mishawaka, IN-MI 
##                                                  0 
##                                    Spartanburg, SC 
##                                                  0 
##                                        Spokane, WA 
##                                                  0 
##                                 Springfield, MA-CT 
##                                                  0 
##                                    Springfield, MO 
##                                                  0 
##                                    Springfield, OH 
##                                                  0 
##                                      St. Cloud, MN 
##                                                  0 
##                                   St. Louis, MO-IL 
##                                                  0 
##                                       Stockton, CA 
##                                                  0 
##                                    Tallahassee, FL 
##                                                  0 
##                                         Toledo, OH 
##                                                  0 
##                                         Topeka, KS 
##                                                  0 
##                                     Tuscaloosa, AL 
##                                                  0 
##                                     Utica-Rome, NY 
##                                                  0 
##                                       Valdosta, GA 
##                                                  0 
##                              Vallejo-Fairfield, CA 
##                                                  0 
##                                     Vero Beach, FL 
##                                                  0 
##                                       Victoria, TX 
##                                                  0 
##                   Vineland-Millville-Bridgeton, NJ 
##                                                  0 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                                  0 
##                                           Waco, TX 
##                                                  0 
##                                      Waterbury, CT 
##                                                  0 
##                           Waterloo-Cedar Falls, IA 
##                                                  0 
##                                         Wausau, WI 
##                                                  0 
##                                        Wichita, KS 
##                                                  0 
##                                   Worcester, MA-CT 
##                                                  0 
##                                         Yakima, WA 
##                                                  0 
##                                   York-Hanover, PA 
##                                                  0 
##                     Youngstown-Warren-Boardman, OH 
##                                                  0 
##                                       Anderson, SC 
##                                                  1 
##                              Bloomington-Normal IL 
##                                                  1 
##                               Boise City-Nampa, ID 
##                                                  1 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                                  1 
##                                       Columbia, SC 
##                                                  1 
##                                     Greenville, SC 
##                                                  1 
##                            Harrisburg-Carlisle, PA 
##                                                  1 
##                                   Jacksonville, FL 
##                                                  1 
##                                       Lawrence, KS 
##                                                  1 
##                            Naples-Marco Island, FL 
##                                                  1 
##                    New Orleans-Metairie-Kenner, LA 
##                                                  1 
##                                        Olympia, WA 
##                                                  1 
##                                     Provo-Orem, UT 
##                                                  1 
##                                       Syracuse, NY 
##                                                  1 
##                                         Tucson, AZ 
##                                                  1 
##                                  Atlantic City, NJ 
##                                                  2 
##                                    Bakersfield, CA 
##                                                  2 
##                              Birmingham-Hoover, AL 
##                                                  2 
##                    Burlington-South Burlington, VT 
##                                                  2 
##                    Charleston-North Charleston, SC 
##                                                  2 
##                        Cleveland-Elyria-Mentor, OH 
##                                                  2 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                                  2 
##                                     Fort Wayne, IN 
##                                                  2 
##                             Las Vegas-Paradise, NV 
##                                                  2 
##                                  Memphis, TN-MS-AR 
##                                                  2 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                                  2 
##                Nashville-Davidson-Murfreesboro, TN 
##                                                  2 
##                               Ogden-Clearfield, UT 
##                                                  2 
##                                  Oklahoma City, OK 
##                                                  2 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                                  2 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                                  2 
##                                      Rochester, NY 
##                                                  2 
##                                 Salt Lake City, UT 
##                                                  2 
##                                    Springfield, IL 
##                                                  2 
##                                  Winston-Salem, NC 
##                                                  2 
##                                    Albuquerque, NM 
##                                                  3 
##                                      Iowa City, IA 
##                                                  3 
##                                        Madison, WI 
##                                                  3 
##                          Norwich-New London, CT-RI 
##                                                  3 
##                                    Reno-Sparks, NV 
##                                                  3 
##                            Visalia-Porterville, CA 
##                                                  3 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                                  4 
##                                   Indianapolis, IN 
##                                                  4 
##                        Omaha-Council Bluffs, NE-IA 
##                                                  4 
##                                         Peoria, IL 
##                                                  4 
##                             Rochester-Dover, NH-ME 
##                                                  4 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                                  4 
##                                  Trenton-Ewing, NJ 
##                                                  4 
##                                          Tulsa, OK 
##                                                  4 
##                                        Orlando, FL 
##                                                  5 
##                        Seattle-Tacoma-Bellevue, WA 
##                                                  5 
##                              Austin-Round Rock, TX 
##                                                  6 
##                          Brownsville-Harlingen, TX 
##                                                  6 
##                                     Des Moines, IA 
##                                                  6 
##                  Little Rock-North Little Rock, AR 
##                                                  6 
##                                      New Haven, CT 
##                                                  6 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                                  6 
##                                  Warner Robins, GA 
##                                                  6 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                                  7 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                                  8 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                                  8 
##                                       Honolulu, HI 
##                                                  9 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                                 11 
##                                 Kansas City, MO-KS 
##                                                 11 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                                 12 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                                 12 
##               Providence-Fall River-Warwick, MA-RI 
##                                                 14 
##                     Houston-Baytown-Sugar Land, TX 
##                                                 15 
##                               Baltimore-Towson, MD 
##                                                 16 
##                                         Fresno, CA 
##                                                 16 
##                                     Pittsburgh, PA 
##                                                 16 
##                    Dallas-Fort Worth-Arlington, TX 
##                                                 18 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                                 19 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                                 19 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                                 23 
##           Hartford-West Hartford-East Hartford, CT 
##                                                 26 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                                 27 
##                  San Francisco-Oakland-Fremont, CA 
##                                                 27 
##                         Detroit-Warren-Livonia, MI 
##                                                 30 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                                 31 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                                 32 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                                 50 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                                 96
```

```r
print(Brazil_by_MetroArea_arr <- 
    sort(tapply(entity_df$Country == "Brazil", entity_df$MetroArea, sum, na.rm=TRUE)))
```

```
##                        Albany-Schenectady-Troy, NY 
##                                                  0 
##                                         Albany, GA 
##                                                  0 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                                  0 
##                                        Altoona, PA 
##                                                  0 
##                                       Amarillo, TX 
##                                                  0 
##                                       Anderson, IN 
##                                                  0 
##                                       Anderson, SC 
##                                                  0 
##                                      Ann Arbor, MI 
##                                                  0 
##                                Anniston-Oxford, AL 
##                                                  0 
##                                        Appleton,WI 
##                                                  0 
##                                      Asheville, NC 
##                                                  0 
##                            Athens-Clark County, GA 
##                                                  0 
##                                  Atlantic City, NJ 
##                                                  0 
##                     Augusta-Richmond County, GA-SC 
##                                                  0 
##                              Austin-Round Rock, TX 
##                                                  0 
##                                    Bakersfield, CA 
##                                                  0 
##                               Baltimore-Towson, MD 
##                                                  0 
##                                         Bangor, ME 
##                                                  0 
##                                    Baton Rouge, LA 
##                                                  0 
##                           Beaumont-Port Author, TX 
##                                                  0 
##                                     Bellingham, WA 
##                                                  0 
##                                           Bend, OR 
##                                                  0 
##                                       Billings, MT 
##                                                  0 
##                                     Binghamton, NY 
##                                                  0 
##                              Birmingham-Hoover, AL 
##                                                  0 
##                              Bloomington-Normal IL 
##                                                  0 
##                                    Bloomington, IN 
##                                                  0 
##                               Boise City-Nampa, ID 
##                                                  0 
##                                        Boulder, CO 
##                                                  0 
##                                  Bowling Green, KY 
##                                                  0 
##                          Brownsville-Harlingen, TX 
##                                                  0 
##                          Buffalo-Niagara Falls, NY 
##                                                  0 
##                    Burlington-South Burlington, VT 
##                                                  0 
##                                   Cedar Rapids, IA 
##                                                  0 
##                               Champaign-Urbana, IL 
##                                                  0 
##                    Charleston-North Charleston, SC 
##                                                  0 
##                                     Charleston, WV 
##                                                  0 
##                                 Chattanooga, TN-GA 
##                                                  0 
##                        Cleveland-Elyria-Mentor, OH 
##                                                  0 
##                                  Coeur d'Alene, ID 
##                                                  0 
##                               Colorado Springs, CO 
##                                                  0 
##                                       Columbia, MO 
##                                                  0 
##                                    Columbus, GA-AL 
##                                                  0 
##                                       Columbus, OH 
##                                                  0 
##                                 Corpus Christi, TX 
##                                                  0 
##                                         Dayton, OH 
##                                                  0 
##                                        Decatur, Al 
##                                                  0 
##                                        Decatur, IL 
##                                                  0 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                                  0 
##                                     Des Moines, IA 
##                                                  0 
##                         Detroit-Warren-Livonia, MI 
##                                                  0 
##                                          Dover, DE 
##                                                  0 
##                                      Duluth, MN-WI 
##                                                  0 
##                                         Durham, NC 
##                                                  0 
##                                     Eau Claire, WI 
##                                                  0 
##                                      El Centro, CA 
##                                                  0 
##                                        El Paso, TX 
##                                                  0 
##                                           Erie, PA 
##                                                  0 
##                             Eugene-Springfield, OR 
##                                                  0 
##                                  Evansville, IN-KY 
##                                                  0 
##                                       Fargo, ND-MN 
##                                                  0 
##                                     Farmington, NM 
##                                                  0 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                                  0 
##                                   Fayetteville, NC 
##                                                  0 
##                                          Flint, MI 
##                                                  0 
##                                       Florence, AL 
##                                                  0 
##                          Fort Collins-Loveland, CO 
##                                                  0 
##                                  Fort Smith, AR-OK 
##                                                  0 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                                  0 
##                                     Fort Wayne, IN 
##                                                  0 
##                                         Fresno, CA 
##                                                  0 
##                                    Gainesville, FL 
##                                                  0 
##                           Grand Rapids-Wyoming, MI 
##                                                  0 
##                                        Greeley, CO 
##                                                  0 
##                                      Green Bay, WI 
##                                                  0 
##                          Greensboro-High Point, NC 
##                                                  0 
##                                     Greenville, SC 
##                                                  0 
##                                Gulfport-Biloxi, MS 
##                                                  0 
##                      Hagerstown-Martinsburg, MD-WV 
##                                                  0 
##                            Harrisburg-Carlisle, PA 
##                                                  0 
##                                   Harrisonburg, VA 
##                                                  0 
##                      Hickory-Morgantown-Lenoir, NC 
##                                                  0 
##                            Holland-Grand Haven, MI 
##                                                  0 
##                                       Honolulu, HI 
##                                                  0 
##                     Houston-Baytown-Sugar Land, TX 
##                                                  0 
##                       Huntington-Ashland, WV-KY-OH 
##                                                  0 
##                                     Huntsville, AL 
##                                                  0 
##                                   Indianapolis, IN 
##                                                  0 
##                                      Iowa City, IA 
##                                                  0 
##                                        Jackson, MI 
##                                                  0 
##                                        Jackson, MS 
##                                                  0 
##                                   Jacksonville, NC 
##                                                  0 
##                                     Janesville, WI 
##                                                  0 
##                                   Johnson City, TN 
##                                                  0 
##                                      Johnstown, PA 
##                                                  0 
##                                         Joplin, MO 
##                                                  0 
##                              Kalamazoo-Portage, MI 
##                                                  0 
##                               Kankakee-Bradley, IL 
##                                                  0 
##                       Killeen-Temple-Fort Hood, TX 
##                                                  0 
##                           Kingsport-Bristol, TN-VA 
##                                                  0 
##                                       Kingston, NY 
##                                                  0 
##                                      Knoxville, TN 
##                                                  0 
##                                      La Crosse, WI 
##                                                  0 
##                                      Lafayette, LA 
##                                                  0 
##                                   Lake Charles, LA 
##                                                  0 
##                          Lakeland-Winter Haven, FL 
##                                                  0 
##                                      Lancaster, PA 
##                                                  0 
##                           Lansing-East Lansing, MI 
##                                                  0 
##                                         Laredo, TX 
##                                                  0 
##                                     Las Cruses, NM 
##                                                  0 
##                             Las Vegas-Paradise, NV 
##                                                  0 
##                                       Lawrence, KS 
##                                                  0 
##                                         Lawton, OK 
##                                                  0 
##                              Lexington-Fayette, KY 
##                                                  0 
##                  Little Rock-North Little Rock, AR 
##                                                  0 
##                                       Longview, TX 
##                                                  0 
##                                        Lubbock, TX 
##                                                  0 
##                                      Lynchburg, VA 
##                                                  0 
##                                          Macon, GA 
##                                                  0 
##                                         Madera, CA 
##                                                  0 
##                                        Madison, WI 
##                                                  0 
##                         McAllen-Edinburg-Pharr, TX 
##                                                  0 
##                                        Medford, OR 
##                                                  0 
##                                  Memphis, TN-MS-AR 
##                                                  0 
##                                         Merced, CA 
##                                                  0 
##                         Michigan City-La Porte, IN 
##                                                  0 
##                                        Midland, TX 
##                                                  0 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                                  0 
##                                         Mobile, AL 
##                                                  0 
##                                        Modesto, CA 
##                                                  0 
##                                         Monroe, MI 
##                                                  0 
##                         Muskegon-Norton Shores, MI 
##                                                  0 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                                  0 
##                                           Napa, CA 
##                                                  0 
##                            Naples-Marco Island, FL 
##                                                  0 
##                Nashville-Davidson-Murfreesboro, TN 
##                                                  0 
##                                      New Haven, CT 
##                                                  0 
##                    New Orleans-Metairie-Kenner, LA 
##                                                  0 
##                            Niles-Benton Harbor, MI 
##                                                  0 
##                          Norwich-New London, CT-RI 
##                                                  0 
##                                          Ocala, FL 
##                                                  0 
##                                     Ocean City, NJ 
##                                                  0 
##                               Ogden-Clearfield, UT 
##                                                  0 
##                                  Oklahoma City, OK 
##                                                  0 
##                                        Olympia, WA 
##                                                  0 
##                        Omaha-Council Bluffs, NE-IA 
##                                                  0 
##                                 Oshkosh-Neenah, WI 
##                                                  0 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                                  0 
##                         Panama City-Lynn Haven, FL 
##                                                  0 
##                                         Peoria, IL 
##                                                  0 
##                                     Pittsburgh, PA 
##                                                  0 
##                     Port St. Lucie-Fort Pierce, FL 
##                                                  0 
##                        Portland-South Portland, ME 
##                                                  0 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                                  0 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                                  0 
##                                       Prescott, AZ 
##                                                  0 
##                                     Provo-Orem, UT 
##                                                  0 
##                                         Pueblo, CO 
##                                                  0 
##                                    Punta Gorda, FL 
##                                                  0 
##                                   Raleigh-Cary, NC 
##                                                  0 
##                                        Reading, PA 
##                                                  0 
##                                    Reno-Sparks, NV 
##                                                  0 
##                                       Richmond, VA 
##                                                  0 
##                       Riverside-San Bernardino, CA 
##                                                  0 
##                                        Roanoke, VA 
##                                                  0 
##                             Rochester-Dover, NH-ME 
##                                                  0 
##                                       Rockford, IL 
##                                                  0 
##                 Saginaw-Saginaw Township North, MI 
##                                                  0 
##                                        Salinas, CA 
##                                                  0 
##                                      Salisbury, MD 
##                                                  0 
##                                    San Antonio, TX 
##                                                  0 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                                  0 
##                    San Luis Obispo-Paso Robles, CA 
##                                                  0 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                                  0 
##                                       Santa Fe, NM 
##                                                  0 
##                            Santa Rosa-Petaluma, CA 
##                                                  0 
##                         Santa-Cruz-Watsonville, CA 
##                                                  0 
##                      Sarasota-Bradenton-Venice, FL 
##                                                  0 
##                                       Savannah, GA 
##                                                  0 
##                          Scranton-Wilkes Barre, PA 
##                                                  0 
##                        Shreveport-Bossier City, LA 
##                                                  0 
##                                    Sioux Falls, SD 
##                                                  0 
##                        South Bend-Mishawaka, IN-MI 
##                                                  0 
##                                    Spartanburg, SC 
##                                                  0 
##                                        Spokane, WA 
##                                                  0 
##                                    Springfield, IL 
##                                                  0 
##                                 Springfield, MA-CT 
##                                                  0 
##                                    Springfield, MO 
##                                                  0 
##                                    Springfield, OH 
##                                                  0 
##                                      St. Cloud, MN 
##                                                  0 
##                                   St. Louis, MO-IL 
##                                                  0 
##                                       Stockton, CA 
##                                                  0 
##                                       Syracuse, NY 
##                                                  0 
##                                    Tallahassee, FL 
##                                                  0 
##                                         Toledo, OH 
##                                                  0 
##                                         Topeka, KS 
##                                                  0 
##                                         Tucson, AZ 
##                                                  0 
##                                          Tulsa, OK 
##                                                  0 
##                                     Tuscaloosa, AL 
##                                                  0 
##                                     Utica-Rome, NY 
##                                                  0 
##                                       Valdosta, GA 
##                                                  0 
##                              Vallejo-Fairfield, CA 
##                                                  0 
##                                     Vero Beach, FL 
##                                                  0 
##                                       Victoria, TX 
##                                                  0 
##                   Vineland-Millville-Bridgeton, NJ 
##                                                  0 
##                            Visalia-Porterville, CA 
##                                                  0 
##                                           Waco, TX 
##                                                  0 
##                                  Warner Robins, GA 
##                                                  0 
##                           Waterloo-Cedar Falls, IA 
##                                                  0 
##                                         Wausau, WI 
##                                                  0 
##                                  Winston-Salem, NC 
##                                                  0 
##                                   Worcester, MA-CT 
##                                                  0 
##                                         Yakima, WA 
##                                                  0 
##                                   York-Hanover, PA 
##                                                  0 
##                     Youngstown-Warren-Boardman, OH 
##                                                  0 
##                                          Akron, OH 
##                                                  1 
##                                    Albuquerque, NM 
##                                                  1 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                                  1 
##                           Bremerton-Silverdale, WA 
##                                                  1 
##                          Cape Coral-Fort Myers, FL 
##                                                  1 
##                                          Chico, CA 
##                                                  1 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                                  1 
##                                  Denver-Aurora, CO 
##                                                  1 
##           Hartford-West Hartford-East Hartford, CT 
##                                                  1 
##                                 Kansas City, MO-KS 
##                                                  1 
##                   Leominster-Fitchburg-Gardner, MA 
##                                                  1 
##                                  Louisville, KY-IN 
##                                                  1 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                                  1 
##                                         Monroe, LA 
##                                                  1 
##                                     Montgomery, AL 
##                                                  1 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                                  1 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                                  1 
##                                         Racine, WI 
##                                                  1 
##                                      Rochester, NY 
##                                                  1 
##                                          Salem, OR 
##                                                  1 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                                  1 
##                        Seattle-Tacoma-Bellevue, WA 
##                                                  1 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                                  1 
##                                  Trenton-Ewing, NJ 
##                                                  1 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                                  1 
##                                      Waterbury, CT 
##                                                  1 
##                                        Wichita, KS 
##                                                  1 
##                                Barnstable Town, MA 
##                                                  2 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                                  2 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                                  2 
##                                       Columbia, SC 
##                                                  2 
##                    Dallas-Fort Worth-Arlington, TX 
##                                                  2 
##                                   Jacksonville, FL 
##                                                  2 
##                                        Orlando, FL 
##                                                  2 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                                  2 
##                               Canton-Massillon, OH 
##                                                  3 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                                  3 
##               Providence-Fall River-Warwick, MA-RI 
##                                                  3 
##                                 Salt Lake City, UT 
##                                                  3 
##                Davenport-Moline-Rock Island, IA-IL 
##                                                  4 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                                  4 
##                                        Danbury, CT 
##                                                  5 
##                  San Francisco-Oakland-Fremont, CA 
##                                                  6 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                                  7 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                                  7 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                                  8 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                                  9 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                                 16 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                                 18
```

```r
print(Somalia_by_MetroArea_arr <- 
    sort(tapply(entity_df$Country == "Somalia", entity_df$MetroArea, sum, na.rm=TRUE)))
```

```
##                                          Akron, OH 
##                                                  0 
##                        Albany-Schenectady-Troy, NY 
##                                                  0 
##                                         Albany, GA 
##                                                  0 
##                                    Albuquerque, NM 
##                                                  0 
##                  Allentown-Bethlehem-Easton, PA-NJ 
##                                                  0 
##                                        Altoona, PA 
##                                                  0 
##                                       Amarillo, TX 
##                                                  0 
##                                       Anderson, IN 
##                                                  0 
##                                       Anderson, SC 
##                                                  0 
##                                      Ann Arbor, MI 
##                                                  0 
##                                Anniston-Oxford, AL 
##                                                  0 
##                                        Appleton,WI 
##                                                  0 
##                                      Asheville, NC 
##                                                  0 
##                            Athens-Clark County, GA 
##                                                  0 
##                 Atlanta-Sandy Springs-Marietta, GA 
##                                                  0 
##                                  Atlantic City, NJ 
##                                                  0 
##                     Augusta-Richmond County, GA-SC 
##                                                  0 
##                              Austin-Round Rock, TX 
##                                                  0 
##                                    Bakersfield, CA 
##                                                  0 
##                               Baltimore-Towson, MD 
##                                                  0 
##                                         Bangor, ME 
##                                                  0 
##                                Barnstable Town, MA 
##                                                  0 
##                                    Baton Rouge, LA 
##                                                  0 
##                           Beaumont-Port Author, TX 
##                                                  0 
##                                     Bellingham, WA 
##                                                  0 
##                                           Bend, OR 
##                                                  0 
##                                       Billings, MT 
##                                                  0 
##                                     Binghamton, NY 
##                                                  0 
##                              Birmingham-Hoover, AL 
##                                                  0 
##                              Bloomington-Normal IL 
##                                                  0 
##                                    Bloomington, IN 
##                                                  0 
##                               Boise City-Nampa, ID 
##                                                  0 
##                     Boston-Cambridge-Quincy, MA-NH 
##                                                  0 
##                                        Boulder, CO 
##                                                  0 
##                                  Bowling Green, KY 
##                                                  0 
##                           Bremerton-Silverdale, WA 
##                                                  0 
##                    Bridgeport-Stamford-Norwalk, CT 
##                                                  0 
##                          Brownsville-Harlingen, TX 
##                                                  0 
##                          Buffalo-Niagara Falls, NY 
##                                                  0 
##                               Canton-Massillon, OH 
##                                                  0 
##                          Cape Coral-Fort Myers, FL 
##                                                  0 
##                                   Cedar Rapids, IA 
##                                                  0 
##                               Champaign-Urbana, IL 
##                                                  0 
##                    Charleston-North Charleston, SC 
##                                                  0 
##                                     Charleston, WV 
##                                                  0 
##                  Charlotte-Gastonia-Concord, NC-SC 
##                                                  0 
##                                 Chattanooga, TN-GA 
##                                                  0 
##                Chicago-Naperville-Joliet, IN-IN-WI 
##                                                  0 
##                                          Chico, CA 
##                                                  0 
##                    Cincinnati-Middletown, OH-KY-IN 
##                                                  0 
##                        Cleveland-Elyria-Mentor, OH 
##                                                  0 
##                                  Coeur d'Alene, ID 
##                                                  0 
##                               Colorado Springs, CO 
##                                                  0 
##                                       Columbia, MO 
##                                                  0 
##                                       Columbia, SC 
##                                                  0 
##                                    Columbus, GA-AL 
##                                                  0 
##                                 Corpus Christi, TX 
##                                                  0 
##                    Dallas-Fort Worth-Arlington, TX 
##                                                  0 
##                                        Danbury, CT 
##                                                  0 
##                Davenport-Moline-Rock Island, IA-IL 
##                                                  0 
##                                        Decatur, Al 
##                                                  0 
##                                        Decatur, IL 
##                                                  0 
##             Deltona-Daytona Beach-Ormond Beach, FL 
##                                                  0 
##                                  Denver-Aurora, CO 
##                                                  0 
##                                     Des Moines, IA 
##                                                  0 
##                         Detroit-Warren-Livonia, MI 
##                                                  0 
##                                          Dover, DE 
##                                                  0 
##                                      Duluth, MN-WI 
##                                                  0 
##                                         Durham, NC 
##                                                  0 
##                                     Eau Claire, WI 
##                                                  0 
##                                      El Centro, CA 
##                                                  0 
##                                        El Paso, TX 
##                                                  0 
##                                           Erie, PA 
##                                                  0 
##                             Eugene-Springfield, OR 
##                                                  0 
##                                  Evansville, IN-KY 
##                                                  0 
##                                     Farmington, NM 
##                                                  0 
##              Fayetteville-Springdale-Rogers, AR-MO 
##                                                  0 
##                                   Fayetteville, NC 
##                                                  0 
##                                          Flint, MI 
##                                                  0 
##                                       Florence, AL 
##                                                  0 
##                          Fort Collins-Loveland, CO 
##                                                  0 
##                                  Fort Smith, AR-OK 
##                                                  0 
##             Fort Walton Beach-Crestview-Destin, FL 
##                                                  0 
##                                     Fort Wayne, IN 
##                                                  0 
##                                         Fresno, CA 
##                                                  0 
##                                    Gainesville, FL 
##                                                  0 
##                           Grand Rapids-Wyoming, MI 
##                                                  0 
##                                        Greeley, CO 
##                                                  0 
##                                      Green Bay, WI 
##                                                  0 
##                          Greensboro-High Point, NC 
##                                                  0 
##                                     Greenville, SC 
##                                                  0 
##                                Gulfport-Biloxi, MS 
##                                                  0 
##                      Hagerstown-Martinsburg, MD-WV 
##                                                  0 
##                            Harrisburg-Carlisle, PA 
##                                                  0 
##                                   Harrisonburg, VA 
##                                                  0 
##           Hartford-West Hartford-East Hartford, CT 
##                                                  0 
##                      Hickory-Morgantown-Lenoir, NC 
##                                                  0 
##                            Holland-Grand Haven, MI 
##                                                  0 
##                                       Honolulu, HI 
##                                                  0 
##                       Huntington-Ashland, WV-KY-OH 
##                                                  0 
##                                     Huntsville, AL 
##                                                  0 
##                                   Indianapolis, IN 
##                                                  0 
##                                      Iowa City, IA 
##                                                  0 
##                                        Jackson, MI 
##                                                  0 
##                                        Jackson, MS 
##                                                  0 
##                                   Jacksonville, FL 
##                                                  0 
##                                   Jacksonville, NC 
##                                                  0 
##                                     Janesville, WI 
##                                                  0 
##                                   Johnson City, TN 
##                                                  0 
##                                      Johnstown, PA 
##                                                  0 
##                                         Joplin, MO 
##                                                  0 
##                              Kalamazoo-Portage, MI 
##                                                  0 
##                               Kankakee-Bradley, IL 
##                                                  0 
##                                 Kansas City, MO-KS 
##                                                  0 
##                       Killeen-Temple-Fort Hood, TX 
##                                                  0 
##                           Kingsport-Bristol, TN-VA 
##                                                  0 
##                                       Kingston, NY 
##                                                  0 
##                                      Knoxville, TN 
##                                                  0 
##                                      La Crosse, WI 
##                                                  0 
##                                      Lafayette, LA 
##                                                  0 
##                                   Lake Charles, LA 
##                                                  0 
##                          Lakeland-Winter Haven, FL 
##                                                  0 
##                                      Lancaster, PA 
##                                                  0 
##                           Lansing-East Lansing, MI 
##                                                  0 
##                                         Laredo, TX 
##                                                  0 
##                                     Las Cruses, NM 
##                                                  0 
##                             Las Vegas-Paradise, NV 
##                                                  0 
##                                       Lawrence, KS 
##                                                  0 
##                                         Lawton, OK 
##                                                  0 
##                   Leominster-Fitchburg-Gardner, MA 
##                                                  0 
##                              Lexington-Fayette, KY 
##                                                  0 
##                  Little Rock-North Little Rock, AR 
##                                                  0 
##                                       Longview, TX 
##                                                  0 
##               Los Angeles-Long Beach-Santa Ana, CA 
##                                                  0 
##                                  Louisville, KY-IN 
##                                                  0 
##                                        Lubbock, TX 
##                                                  0 
##                                      Lynchburg, VA 
##                                                  0 
##                                          Macon, GA 
##                                                  0 
##                                         Madera, CA 
##                                                  0 
##                                        Madison, WI 
##                                                  0 
##                         McAllen-Edinburg-Pharr, TX 
##                                                  0 
##                                        Medford, OR 
##                                                  0 
##                                  Memphis, TN-MS-AR 
##                                                  0 
##                                         Merced, CA 
##                                                  0 
##              Miami-Fort Lauderdale-Miami Beach, FL 
##                                                  0 
##                         Michigan City-La Porte, IN 
##                                                  0 
##                                        Midland, TX 
##                                                  0 
##                  Milwaukee-Waukesha-West Allis, WI 
##                                                  0 
##                                         Mobile, AL 
##                                                  0 
##                                        Modesto, CA 
##                                                  0 
##                                         Monroe, LA 
##                                                  0 
##                                         Monroe, MI 
##                                                  0 
##                                     Montgomery, AL 
##                                                  0 
##                         Muskegon-Norton Shores, MI 
##                                                  0 
##         Myrtle Beach-Conway-North Myrtle Beach, SC 
##                                                  0 
##                                           Napa, CA 
##                                                  0 
##                            Naples-Marco Island, FL 
##                                                  0 
##                Nashville-Davidson-Murfreesboro, TN 
##                                                  0 
##                                      New Haven, CT 
##                                                  0 
##                    New Orleans-Metairie-Kenner, LA 
##                                                  0 
## New York-Northern New Jersey-Long Island, NY-NJ-PA 
##                                                  0 
##                            Niles-Benton Harbor, MI 
##                                                  0 
##                          Norwich-New London, CT-RI 
##                                                  0 
##                                          Ocala, FL 
##                                                  0 
##                                     Ocean City, NJ 
##                                                  0 
##                               Ogden-Clearfield, UT 
##                                                  0 
##                                  Oklahoma City, OK 
##                                                  0 
##                                        Olympia, WA 
##                                                  0 
##                        Omaha-Council Bluffs, NE-IA 
##                                                  0 
##                                        Orlando, FL 
##                                                  0 
##                                 Oshkosh-Neenah, WI 
##                                                  0 
##                   Oxnard-Thousand Oaks-Ventura, CA 
##                                                  0 
##                  Palm Bay-Melbourne-Titusville, FL 
##                                                  0 
##                         Panama City-Lynn Haven, FL 
##                                                  0 
##                     Pensacola-Ferry Pass-Brent, FL 
##                                                  0 
##                                         Peoria, IL 
##                                                  0 
##           Philadelphia-Camden-Wilmington, PA-NJ-DE 
##                                                  0 
##                                     Pittsburgh, PA 
##                                                  0 
##                     Port St. Lucie-Fort Pierce, FL 
##                                                  0 
##               Poughkeepsie-Newburgh-Middletown, NY 
##                                                  0 
##                                       Prescott, AZ 
##                                                  0 
##               Providence-Fall River-Warwick, MA-RI 
##                                                  0 
##                                     Provo-Orem, UT 
##                                                  0 
##                                         Pueblo, CO 
##                                                  0 
##                                    Punta Gorda, FL 
##                                                  0 
##                                         Racine, WI 
##                                                  0 
##                                   Raleigh-Cary, NC 
##                                                  0 
##                                        Reading, PA 
##                                                  0 
##                                    Reno-Sparks, NV 
##                                                  0 
##                       Riverside-San Bernardino, CA 
##                                                  0 
##                                        Roanoke, VA 
##                                                  0 
##                             Rochester-Dover, NH-ME 
##                                                  0 
##                                      Rochester, NY 
##                                                  0 
##                                       Rockford, IL 
##                                                  0 
##              Sacramento-Arden-Arcade-Roseville, CA 
##                                                  0 
##                 Saginaw-Saginaw Township North, MI 
##                                                  0 
##                                          Salem, OR 
##                                                  0 
##                                        Salinas, CA 
##                                                  0 
##                                      Salisbury, MD 
##                                                  0 
##                                 Salt Lake City, UT 
##                                                  0 
##                                    San Antonio, TX 
##                                                  0 
##                  San Diego-Carlsbad-San Marcos, CA 
##                                                  0 
##                  San Francisco-Oakland-Fremont, CA 
##                                                  0 
##                 San Jose-Sunnyvale-Santa Clara, CA 
##                                                  0 
##                    San Luis Obispo-Paso Robles, CA 
##                                                  0 
##               Santa Barbara-Santa Maria-Goleta, CA 
##                                                  0 
##                                       Santa Fe, NM 
##                                                  0 
##                            Santa Rosa-Petaluma, CA 
##                                                  0 
##                         Santa-Cruz-Watsonville, CA 
##                                                  0 
##                      Sarasota-Bradenton-Venice, FL 
##                                                  0 
##                                       Savannah, GA 
##                                                  0 
##                          Scranton-Wilkes Barre, PA 
##                                                  0 
##                        Shreveport-Bossier City, LA 
##                                                  0 
##                        South Bend-Mishawaka, IN-MI 
##                                                  0 
##                                    Spartanburg, SC 
##                                                  0 
##                                        Spokane, WA 
##                                                  0 
##                                    Springfield, IL 
##                                                  0 
##                                 Springfield, MA-CT 
##                                                  0 
##                                    Springfield, MO 
##                                                  0 
##                                    Springfield, OH 
##                                                  0 
##                                   St. Louis, MO-IL 
##                                                  0 
##                                       Stockton, CA 
##                                                  0 
##                                       Syracuse, NY 
##                                                  0 
##                                    Tallahassee, FL 
##                                                  0 
##                Tampa-St. Petersburg-Clearwater, FL 
##                                                  0 
##                                         Toledo, OH 
##                                                  0 
##                                         Topeka, KS 
##                                                  0 
##                                  Trenton-Ewing, NJ 
##                                                  0 
##                                         Tucson, AZ 
##                                                  0 
##                                          Tulsa, OK 
##                                                  0 
##                                     Tuscaloosa, AL 
##                                                  0 
##                                     Utica-Rome, NY 
##                                                  0 
##                                       Valdosta, GA 
##                                                  0 
##                              Vallejo-Fairfield, CA 
##                                                  0 
##                                     Vero Beach, FL 
##                                                  0 
##                                       Victoria, TX 
##                                                  0 
##                   Vineland-Millville-Bridgeton, NJ 
##                                                  0 
##         Virginia Beach-Norfolk-Newport News, VA-NC 
##                                                  0 
##                            Visalia-Porterville, CA 
##                                                  0 
##                                           Waco, TX 
##                                                  0 
##                                  Warner Robins, GA 
##                                                  0 
##       Washington-Arlington-Alexandria, DC-VA-MD-WV 
##                                                  0 
##                                      Waterbury, CT 
##                                                  0 
##                           Waterloo-Cedar Falls, IA 
##                                                  0 
##                                         Wausau, WI 
##                                                  0 
##                                        Wichita, KS 
##                                                  0 
##                                  Winston-Salem, NC 
##                                                  0 
##                                   Worcester, MA-CT 
##                                                  0 
##                                         Yakima, WA 
##                                                  0 
##                                   York-Hanover, PA 
##                                                  0 
##                     Youngstown-Warren-Boardman, OH 
##                                                  0 
##                                         Dayton, OH 
##                                                  1 
##                                       Richmond, VA 
##                                                  1 
##                     Houston-Baytown-Sugar Land, TX 
##                                                  2 
##                                    Sioux Falls, SD 
##                                                  2 
##                    Burlington-South Burlington, VT 
##                                                  3 
##                        Portland-South Portland, ME 
##                                                  3 
##                Portland-Vancouver-Beaverton, OR-WA 
##                                                  3 
##                                       Columbus, OH 
##                                                  5 
##                                       Fargo, ND-MN 
##                                                  5 
##                        Phoenix-Mesa-Scottsdale, AZ 
##                                                  7 
##                        Seattle-Tacoma-Bellevue, WA 
##                                                  7 
##                                      St. Cloud, MN 
##                                                  7 
##             Minneapolis-St Paul-Bloomington, MN-WI 
##                                                 17
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##           chunk_label chunk_step_major chunk_step_minor
## 1         import_data                1                0
## 2        inspect_data                2                1
## 3 manage_missing_data                2                2
## 4         encode_data                2                2
## 5    extract_features                3                0
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
## 3      encode_data                2                2
## 4 extract_features                3                0
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
## [1] tcltk     stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] sqldf_0.4-10    RSQLite_1.0.0   DBI_0.3.1       gsubfn_0.6-6   
##  [5] proto_0.3-10    reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13    
##  [9] survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] chron_2.3-45     codetools_0.2-10 colorspace_1.2-5 digest_0.6.8    
##  [5] evaluate_0.5.5   formatR_1.0      grid_3.1.2       gtable_0.1.2    
##  [9] htmltools_0.2.6  knitr_1.9        labeling_0.3     lattice_0.20-30 
## [13] MASS_7.3-39      Matrix_1.1-5     munsell_0.4.2    Rcpp_0.11.4     
## [17] rmarkdown_0.5.1  scales_0.2.4     splines_3.1.2    stringr_0.6.2   
## [21] tools_3.1.2      yaml_2.1.13
```
