Faculty Classification
================
Chad Evans

Built with R version 3.3.2. Last run on 2018-02-06.

Contents
--------

-   [Configure](#configure)
    -   Directories
    -   Libraries
-   [Munge](#munge)
    -   Subset
    -   Missing Data
    -   Imputation
-   [Cluster Analysis of Full-time Faculty](#cluster-analysis-of-full-time-faculty)
    -   Determining the Number of Clusters
    -   K-means Clustering
    -   [Full-time Faculty Crosstabulations](#full-time-faculty-crosstabulations)
        -   Demography Table
        -   Institution Table
        -   Department Table
        -   Employment Table
    -   [Full-time Faculty Typology](#full-time-faculty-typology)
-   [Cluster Analysis of Part-time Faculty](#cluster-analysis-of-part-time-faculty)
    -   Determining the Number of Clusters
    -   K-means Clustering
    -   [Part-time Faculty Crosstabulations](#part-time-faculty-crosstabulations)
        -   Demography Table
        -   Institution Table
        -   Department Table
        -   General Employment Table
        -   Part-time Employment Table
    -   [Part-time Faculty Typology](#part-time-faculty-typology)
-   [Conclusion](#conclusion)
-   [Appendix](#appendix)
    -   Gappa and Leslie (1993) classification
    -   Furstenberg (2015) classification

Configure
---------

Munge
-----

``` r
load(file.path(Private_Cache,"HERI_Class.RData"))
source(file.path(Munge, "01_Merge_the_data.R"))
source(file.path(Munge, "02_Clean_the_data.R"))
source(file.path(Munge, "03_Recode_4_Impute.R")) # necessary to avoid subscript out of bounds
source(file.path(Munge, "HERI_vars.R"))
# source(file.path(Munge, "03_Recode_HERI.R"))
```

### Create Full-time and Part-time Datasets

First we create the dataframes that will be used for imputation and k-means clustering. There will be one dataframe for the Full-time faculty and one for the part-time faculty. For each of these data frames, we will remove administrative variables that contribute no value to imputation. We'll also remove the composite variables which simply summarize information on variables already contained in our dataset (although we keep STRESS because the data HERI gave us do not contain all the stress items in the full dataset). We also remove the variable indicating full-time status, as the two datasets correspond to that dimension. Finally, we remove all variables with high missingness (more than 25%). Missingness removed only a few variables from each of the datasets.

``` r
FTdf<-df %>% filter(FULLSTAT=="Yes") %>% select(-one_of(c(ADMINVARS, PTVARS,"PRODUCTIVITY","SATIS_WORKPLACE","SATIS_COMPENSATION","FULLSTAT","DEPT","DEPTDISC"))) # Get rid of admin vars, part-time only variables, and composite variables (b/c we have the items)  We are keeping STRESS because was lack signficant number of the STRESS items.  Also, DEPT and DEPTDISC have tons of levels and their are collapsed versions (e.g., DEPTA) already in the data.  Finally, all FT faculty are not on tenure track, but institution has tenure system.  So we must remove that variable as there is no variation.
miss_pct<-FTdf %>% map_dbl(function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
HIGHMISS<-labels(which(miss_pct>25))
FTdf<-FTdf %>% select(-one_of(HIGHMISS)) # Get rid of additional features with large amounts of missing observations.
dim(FTdf)
```

    ## [1] 4527  108

``` r
PTdf<-df %>% filter(FULLSTAT=="No") %>% select(-one_of(c(ADMINVARS,"PRODUCTIVITY","SATIS_WORKPLACE","SATIS_COMPENSATION","FULLSTAT","DEPT","DEPTDISC"))) # %>% names() # Get rid of admin vars, and composite variables (b/c we have the items)  We are keeping STRESS because was lack signficant number of the STRESS items.
miss_pct<-PTdf %>% map_dbl(function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
HIGHMISS<-labels(which(miss_pct>25))
PTdf<-PTdf %>% select(-one_of(HIGHMISS)) # Get rid of additional features with large amounts of missing observations.
dim(PTdf)
```

    ## [1] 4453  134

### Missing Data

``` r
miss_pct_plot(FTdf)
```

![](graphs/unnamed-chunk-2-1.png)

``` r
miss_pct_plot(PTdf)
```

![](graphs/unnamed-chunk-2-2.png)

### Imputation of Missing Values

As expected, listwise deletion across all features is impossible. We thus must consider a method to deal with the missingness. I opt for single imputiation. In some cases, like regression, it would probably be worth multiply imputing to get standard errors correct. However, this study is not using standard errors and is merely a procedure to find coherence in the data. I thus opt to singly impute the data for reasons of simplicity.

The mice() package allows for many different algorithms to impute data. I opted for using regression trees (CART). This was my choice because my data contain a large number of unbalanced factor variables. The stochasitc method of predictive mean matching (pmm) failed with a computationally singular error. The CART algorithm imputes each target column by using information from all of the other columns in the data. As most other columns have missingness as well, the algorthm uses the most recent round of imputations for each.

I set the maximum iterations to 5. This will give the chained equations multiple attempts to converge on a good imputed value for each cell.

The imputation of the full-time dataframe utilized 108 features. The imputation of the part-time dataframe used 134 features.

``` r
#Imputing takes 2-3 hours
FTdfi<-FTdf %>% mice(m=1,maxit=5,seed=500, method='cart') %>% complete(1)
save(FTdfi, file=file.path(Private_Cache,"FTdfi.RData"))

PTdfi<-PTdf %>% mice(m=1,maxit=5,seed=500, method='cart') %>% complete(1)
save(PTdfi, file=file.path(Private_Cache,"PTdfi.RData"))
```

Cluster Analysis of Full-time Faculty
-------------------------------------

Typology typically have a purpose. We will be focusing on functional typologies, based on faculty motivations, responsibilities and experiences. Fifty-five such features were used to cluster full-time faculty. To implement k-means clustering, all data must be numeric. This requires converting binary factors to zero and ones. Multinomial variables needed to be converted into a matrix of dummy variables.

``` r
load(file.path(Private_Cache,"FTdfi.RData")) # Singly imputed data

source(file.path(Munge, "HERI_vars.R"))
FTdfi <- FTdfi %>% select(WORKVARS, FTVARS, OTHERVARS, PROFDEVVARS, STRESSVARS, PRODUCTIVITYVARS)

idata<-data.frame(model.matrix(~ ., data=FTdfi, contrasts.arg = lapply(FTdfi[,sapply(FTdfi, is.factor)], contrasts, contrasts=FALSE)))
```

### Determining the number of Clusters

When implementing k-means clustering, one must specify the number of means to cluster around in the data. The most common approach to choosing the number of clusters is to plot how the within sum of squared residuals decreases as additional means are added and identify the "elbow." This is the point where the explained variation starts to begin to plateau off.

``` r
wssplot(idata, nc=7)
```

![](graphs/unnamed-chunk-4-1.png)

The elbow suggests that three clusters may be sufficient for explaining most of the variation in the full-time data. I therefore opt to go with three means in the k-means clustering analysis of full-time faculty.

### K-Means Clustering

``` r
n_clusters<-3
d<-idata %>% select(-X.Intercept.) %>% scale() %>% data.frame() # git rid of the intercept (no variation, convergence issues)
singular_d<-(colnames(d)[apply(d, 2, anyNA)])
d<-d %>% select(-one_of(singular_d))
kmeans.obj<- kmeans(d, n_clusters, nstart = 10, algorithm = c("Hartigan-Wong"))
FTdf$cluster<-kmeans.obj$cluster
```

Before conducting k-means clustering, all variables were normalized so that features with the greatest ranges did not have undue influence on the formation of clusters.

To conduct the k-means analysis, it is important to choose random starting points for the means. This helps prevent the algorithm (Hartingan-Wong 1979) from converging on suboptimal means. I used 10 different sets of starting points to identify the means that best summarize the information in the data.

### Full-time Faculty Crosstabulations

#### Demography Table

``` r
DemVars<-c("AGE","SEX","MARITAL2","RACEGROUP2","GENACT02","NATENGSP","NCHILD3","DEGEARN2","DEGWORK2")
table<-round(nfCrossTable(data=df[DemVars],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Age","Male","Married","White","Citizen","Native English","Avg. Children","BA or Less","Prof Degree","Ph.D.","Working on a Degree")
kable(table, caption = "Distribution of Adjunct Clusters by Demographic Characteristics")
```

|                     |  Cluster 1 (n=2686)|  Cluster 2 (n=716)|  Cluster 3 (n=1125)|
|---------------------|-------------------:|------------------:|-------------------:|
| Age                 |               48.28|              45.38|               51.37|
| Male                |                0.45|               0.39|                0.43|
| Married             |                0.77|               0.74|                0.80|
| White               |                0.84|               0.80|                0.85|
| Citizen             |                0.93|               0.91|                0.97|
| Native English      |                0.88|               0.86|                0.94|
| Avg. Children       |                1.47|               1.29|                1.72|
| BA or Less          |                0.06|               0.04|                0.05|
| Prof Degree         |                0.58|               0.51|                0.62|
| Ph.D.               |                0.37|               0.45|                0.33|
| Working on a Degree |                0.20|               0.17|                0.16|

-   Cluster 3 is a bit older and more children
-   Cluster 3 tends to have a professional background (and somewhat fewer PhDs).
-   Cluster 1 and 2 are quite similar in this domain

#### Institution Table

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Cluster 1 (n=2686)|  Cluster 2 (n=716)|  Cluster 3 (n=1125)|
|--------------------------------|-------------------:|------------------:|-------------------:|
| 2-year                         |                0.00|               0.00|                0.00|
| 4-year                         |                0.64|               0.62|                0.63|
| University                     |                0.36|               0.38|                0.37|
| Public                         |                0.40|               0.43|                0.30|
| Research I                     |                0.04|               0.04|                0.05|
| Research II                    |                0.21|               0.24|                0.21|
| R3/Doctoral                    |                0.09|               0.07|                0.10|
| Bachelors/Masters              |                0.65|               0.64|                0.63|
| Associates                     |                0.00|               0.00|                0.00|
| Other Inst.                    |                0.01|               0.01|                0.01|
| Highly Selective               |                0.10|               0.11|                0.09|
| Faculty very respectful        |                0.53|               0.49|                0.47|
| Administators very considerate |                0.21|               0.14|                0.27|
| Research valued                |                0.70|               0.57|                0.69|
| Teaching valued                |                0.91|               0.85|                0.89|

-   Cluster 3 more likely to work in private institutions
-   Cluster 2 tends to find administrators less respectful, and research and teaching less valued

#### Department Table

HERI has departamental level infromation, but it is challenging to see any patterns. So I collapsed the disciplines into the Biglan classification.

``` r
# DEPTA available, but tough to see any patterns with so many categories
source(file.path(Libraries, "Subclass.R"))
table<-round(prop.table(table(df$SUBCLASS, df$cluster),2),2)
colnames(table)<-Clusternames
#rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stats","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                 |  Cluster 1 (n=2686)|  Cluster 2 (n=716)|  Cluster 3 (n=1125)|
|-----------------|-------------------:|------------------:|-------------------:|
| Sciences        |                0.22|               0.22|                0.10|
| Soft/Applied    |                0.33|               0.26|                0.50|
| Humanities/Arts |                0.22|               0.31|                0.17|
| Health Sciences |                0.13|               0.06|                0.14|
| Social Sciences |                0.10|               0.14|                0.09|

-   Cluster 3 tends to be in the Soft/applied fields
-   Cluster 1 and 2 look pretty similar. Both groups are more likely than cluster 3 to work in the pure sciences.

#### Employment Table

``` r
WORKVARS<-c("PRINACT2","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","SALARY","COURSENUM","PROFDEVFAC")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration/Other","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses","Prof. Dev. Rating")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                      |  Cluster 1 (n=2686)|  Cluster 2 (n=716)|  Cluster 3 (n=1125)|
|----------------------|-------------------:|------------------:|-------------------:|
| Teaching             |                0.92|               0.87|                0.45|
| Research             |                0.04|               0.06|                0.02|
| Administration/Other |                0.04|               0.07|                0.53|
| Assistant Professor  |                0.35|               0.29|                0.26|
| Associate Professor  |                0.08|               0.06|                0.19|
| Instructor           |                0.27|               0.32|                0.23|
| Lecturer             |                0.26|               0.31|                0.20|
| Professor            |                0.05|               0.03|                0.11|
| Union member         |                0.14|               0.16|                0.11|
| Health benefits      |                0.95|               0.94|                0.96|
| Retirement           |                0.94|               0.90|                0.98|
| Avg. Salary          |            55276.56|           49720.19|            75026.27|
| Avg. Courses         |                3.14|               3.04|                1.83|
| Prof. Dev. Rating    |               -0.02|              -0.27|                0.22|

``` r
write.csv(table, file.path(Graphs,"FT_Employment_table.csv"))
```

-   Most salient differences found in employment
-   Cluster 3 tend to teach less. They are mostly administrators.
-   The other clusters are teachers (so there are two types of full-time teachers)
-   Cluster 1 is in a slightly more agreeable situation
-   Little professional development of cluster 2

### Full-time Faculty Typology

-   Cluster 1: Assimilating adjunct
-   Cluster 2: Repelled adjunct (only 16% of FT NTT faculty)
-   Cluster 3: Administrative adjuncts (although some teach)

The "administrative adjunct" is the clearest pattern to emerge from the full-time faculty analysis. About 25% of full-time faculty fall into this class. The other two full-time adjuncts are more difficult to distinguish. However, cluster one seems to be in a slightly better work environment. They resport more support, better salary, and more respect. Might be slightly linked to educational background, as cluster 2 is "more education" but confined to an off tenure-track position. Cluster 2 is most likely to be in the humanities.

Cluster Analysis of Part-time Faculty
-------------------------------------

For part-time faculty, we cluster based on eight-two features related to faculty functions, experiences and responsibilities.

``` r
load(file.path(Private_Cache,"PTdfi.RData")) # Singly imputed data

source(file.path(Munge, "HERI_vars.R"))
PTdfi <- PTdfi %>% select(WORKVARS, PTVARS, OTHERVARS, PROFDEVVARS, STRESSVARS, PRODUCTIVITYVARS)

idata<-data.frame(model.matrix(~ ., data=PTdfi, contrasts.arg = lapply(PTdfi[,sapply(PTdfi, is.factor)], contrasts, contrasts=FALSE)))
```

### Determining the number of Clusters

``` r
wssplot(idata, nc=7) 
```

![](graphs/unnamed-chunk-6-1.png)

The elbow suggests that five clusters sufficiently explain most of the variation in the part-time data. I therefore opt to go with five means in the k-means clustering analysis for part-time faculty.

### K-Means Clustering

``` r
n_clusters<-5
d<-idata %>% select(-X.Intercept.) %>% scale() %>% data.frame() # git rid of the intercept (no variation, convergence issues)
singular_d<-(colnames(d)[apply(d, 2, anyNA)])
d<-d %>% select(-one_of(singular_d))
kmeans.obj<- kmeans(d, n_clusters, nstart = 10, algorithm = c("Hartigan-Wong"))
PTdf$cluster<-kmeans.obj$cluster
```

### Part-time Faculty Crosstabulations

#### Demography Table

``` r
DemVars<-c("AGE","SEX","MARITAL2","RACEGROUP2","GENACT02","NATENGSP","NCHILD3","DEGEARN2","DEGWORK2")
table<-round(nfCrossTable(data=df[DemVars],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Age","Male","Married","White","Citizen","Native English","Avg. Children","BA or Less","Prof Degree","Ph.D.","Working on a Degree")
kable(table, caption = "Distribution of Adjunct Clusters by Demographic Characteristics")
```

|                     |  Cluster 1 (n=953)|  Cluster 2 (n=1027)|  Cluster 3 (n=391)|  Cluster 4 (n=1641)|  Cluster 5 (n=441)|
|---------------------|------------------:|-------------------:|------------------:|-------------------:|------------------:|
| Age                 |              50.75|               47.64|              49.66|               51.70|              60.09|
| Male                |               0.48|                0.40|               0.44|                0.51|               0.53|
| Married             |               0.80|                0.74|               0.75|                0.80|               0.81|
| White               |               0.86|                0.79|               0.83|                0.82|               0.89|
| Citizen             |               0.97|                0.93|               0.96|                0.97|               0.97|
| Native English      |               0.93|                0.88|               0.92|                0.92|               0.93|
| Avg. Children       |               1.75|                1.41|               1.57|                1.88|               1.88|
| BA or Less          |               0.12|                0.05|               0.09|                0.12|               0.05|
| Prof Degree         |               0.72|                0.63|               0.70|                0.76|               0.33|
| Ph.D.               |               0.16|                0.32|               0.21|                0.12|               0.61|
| Working on a Degree |               0.20|                0.25|               0.23|                0.21|               0.09|

-   Cluster 5 is distinctly older and have PhDs
-   The other clusters tend to have professional degrees
-   Cluster 2 really educated with lots of PhDs and professional degrees. They are also the youngest and with fewer children (probably because they are young)

#### Institution Table

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Cluster 1 (n=953)|  Cluster 2 (n=1027)|  Cluster 3 (n=391)|  Cluster 4 (n=1641)|  Cluster 5 (n=441)|
|--------------------------------|------------------:|-------------------:|------------------:|-------------------:|------------------:|
| 2-year                         |               0.04|                0.03|               0.04|                0.06|               0.02|
| 4-year                         |               0.69|                0.74|               0.74|                0.65|               0.71|
| University                     |               0.27|                0.23|               0.22|                0.29|               0.27|
| Public                         |               0.39|                0.57|               0.32|                0.35|               0.39|
| Research I                     |               0.03|                0.05|               0.04|                0.04|               0.07|
| Research II                    |               0.15|                0.13|               0.11|                0.16|               0.14|
| R3/Doctoral                    |               0.05|                0.05|               0.05|                0.07|               0.04|
| Bachelors/Masters              |               0.72|                0.74|               0.75|                0.67|               0.72|
| Associates                     |               0.04|                0.03|               0.04|                0.06|               0.02|
| Other Inst.                    |               0.00|                0.00|               0.01|                0.00|               0.00|
| Highly Selective               |               0.04|                0.08|               0.03|                0.03|               0.15|
| Faculty very respectful        |               0.57|                0.44|               0.48|                0.64|               0.56|
| Administators very considerate |               0.22|                0.10|               0.14|                0.28|               0.25|
| Research valued                |               0.56|                0.56|               0.49|                0.69|               0.77|
| Teaching valued                |               0.89|                0.84|               0.82|                0.95|               0.94|

-   Cluster 2 tend to work at public institutions.
-   Cluster 5 more likley to work in more selective institutions
-   Clusters 2 and 3 tend to work in more difficult environments (reporting less colleague respect, admin respect, value of teaching and value of research)

#### Department Table

``` r
source(file.path(Libraries, "Subclass.R"))
table<-round(prop.table(table(df$SUBCLASS, df$cluster),2),2) # DEPTA was aggregated by HERI
colnames(table)<-Clusternames
#rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stats","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                 |  Cluster 1 (n=953)|  Cluster 2 (n=1027)|  Cluster 3 (n=391)|  Cluster 4 (n=1641)|  Cluster 5 (n=441)|
|-----------------|------------------:|-------------------:|------------------:|-------------------:|------------------:|
| Sciences        |               0.16|                0.14|               0.12|                0.15|               0.21|
| Soft/Applied    |               0.42|                0.25|               0.39|                0.47|               0.28|
| Humanities/Arts |               0.21|                0.40|               0.30|                0.18|               0.24|
| Health Sciences |               0.07|                0.05|               0.04|                0.08|               0.10|
| Social Sciences |               0.14|                0.16|               0.15|                0.12|               0.18|

-   Cluster 2 most likely to work in humanities
-   Cluster 5 most likely to work in sciences
-   Clusters 1, 3 and 4 work in diverse departments, perhaps a bit more in the soft/applied fields.

#### Employment Table

``` r
WORKVARS<-c("PRINACT2","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","PTSALARY","COURSENUM","PROFDEVFAC")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration/Other","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses","Prof. Dev. Rating")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                      |  Cluster 1 (n=953)|  Cluster 2 (n=1027)|  Cluster 3 (n=391)|  Cluster 4 (n=1641)|  Cluster 5 (n=441)|
|----------------------|------------------:|-------------------:|------------------:|-------------------:|------------------:|
| Teaching             |               0.98|                0.97|               0.98|                0.97|               0.82|
| Research             |               0.00|                0.01|               0.00|                0.00|               0.06|
| Administration/Other |               0.02|                0.02|               0.02|                0.02|               0.12|
| Assistant Professor  |               0.06|                0.10|               0.05|                0.07|               0.12|
| Associate Professor  |               0.04|                0.02|               0.04|                0.04|               0.17|
| Instructor           |               0.60|                0.36|               0.63|                0.60|               0.13|
| Lecturer             |               0.24|                0.50|               0.23|                0.20|               0.15|
| Professor            |               0.05|                0.02|               0.05|                0.08|               0.43|
| Union member         |               0.16|                0.39|               0.14|                0.15|               0.21|
| Health benefits      |               0.34|                0.79|               0.44|                0.34|               0.82|
| Retirement           |               0.38|                0.77|               0.49|                0.39|               0.86|
| Avg. Salary          |           11005.33|            21356.14|           12272.68|            11256.67|           34657.24|
| Avg. Courses         |               1.95|                3.31|               2.46|                1.88|               1.82|
| Prof. Dev. Rating    |              -0.20|                0.08|              -0.17|               -0.10|               0.76|

``` r
write.csv(table, file.path(Graphs,"PT_Employment_table_1.csv"))
```

-   Cluster 5 are sligtly less likely to be teachers and more likely to report that they are primarily administrators. They also have better status as professors.
-   Cluster 2 is unionizing at higher rates. They are also acquireing health and retirement benefits at higher rates. They also have significantly higher salaries, in part because they teach the most courses. Cluster 2 also tend to get more PD than other clusters (outside of cluster 5)

#### Part-time Employment Table

``` r
PTVARS<-c("PRINACT2","PTCHOICE","PTWORKFT","PTCAREER","PTTEACH","PTSALARY","PTPAY") # kept PRINACT2 bc bug requires it
table<-round(nfCrossTable(data=df[PTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration/Other","Involuntary PT","Sought FT","Career Outside Academia","# Other Institutions","Total Salary","Payment Per Course")
kable(table, caption = "Distribution of Adjunct Clusters by Part-time Characteristics")
```

|                         |  Cluster 1 (n=953)|  Cluster 2 (n=1027)|  Cluster 3 (n=391)|  Cluster 4 (n=1641)|  Cluster 5 (n=441)|
|-------------------------|------------------:|-------------------:|------------------:|-------------------:|------------------:|
| Teaching                |               0.98|                0.97|               0.98|                0.97|               0.82|
| Research                |               0.00|                0.01|               0.00|                0.00|               0.06|
| Administration/Other    |               0.02|                0.02|               0.02|                0.02|               0.12|
| Involuntary PT          |               0.55|                0.83|               0.65|                0.51|               0.22|
| Sought FT               |               0.40|                0.74|               0.50|                0.33|               0.68|
| Career Outside Academia |               0.50|                0.11|               0.49|                0.52|               0.09|
| \# Other Institutions   |               0.54|                0.79|               0.71|                0.52|               0.24|
| Total Salary            |           11005.33|            21356.14|           12272.68|            11256.67|           34657.24|
| Payment Per Course      |            3056.51|             4020.35|            3041.47|             3190.15|            6770.13|

``` r
write.csv(table, file.path(Graphs,"PT_Employment_table_2.csv"))
```

-   Cluster 2 is most likely to report that they work involuntarily in this position and that they have pursued full-time employment in higher education
-   Cluster 2 also does not work outside of higher education
-   Other cluster categoreis are far less assimilated into postsecondary careers.

### Part-time Faculty Typology

-   Cluster 1:
-   Cluster 2: Aspiring Adjunct
-   Cluster 3:
-   Cluster 4: Career-switchers?
-   Cluster 5: Career Enders

Part-time faculty tend to be teachers, even career-enders with small roles in research and administration. Cluster four may be only "one coursers" with a very limited role, often in the professional fields, and without any serious professional development or benefits.

Conclusion
----------

The employment characteristics tend to offer the most coherent understanding of who these adjuncts are and what they are doing in academia.

Appendix
--------

As demonstrated earlier, non-tenure track faculty are heterogeneous in their composition. Some are young academics trying to begin their careers in academia. Some are retiring faculty who are interested in leaving their life's work in a gradual fashion. We explored the motives for working part-time in academia and indeed it was a useful way to characterize and classify non-tenure track faculty. We also explored two other classification schema in our research. The first was designed by Gappa and Leslie and featured in their 1993 book "The Invisible Faculty." Gappa and Leslie were some of the earlist scholars to identify and seek knowledge about the growing numbers of part-time faculty in higher education. Examining faculty at 18 colleges and universities, they focused on the features of career-enders; experts; freelancers; and aspiring academics. Career enders consisted of faculty who were in the process of retiring from the workforce. Many of these individuals were not career academics, but instead had worked in the private sector. Part-time faculty in this class worked for supplemental income or simply because they enjoyed teaching. Experts (specialists or professionals) were hired for their specialized knowledge or experience. Freelancers were mostly faculty who wanted to supplement the income earned from a career outside of academia. These faculty were also commonly homemakers, taking care of children and domestic chores on the side.

A final category identified by these authors were the aspiring academics. Aspiring academics are “relatively new Ph.D.’s seeking tenure-track appointments and some Ph.D. recipients who have been teaching on a part-time basis for years in the hope of attaining a full-time, tenure-track position. Under better circumstances, they would be part of the tenured faculty (1993, p.54.55).” This definition calls attention to an important dimension often excluded by many researchers. Rather than generalize across all adjuncts, Gappa and Leslie make the point of distinguishing between those who are trying to establish a full-time, long-term career in academia and those who simply dabble in it. Recognizing the voluntary/involuntary nature of contingent status, then, is integral for any typological schema. This is a point also stressed by other researchers (Tilly 1998, Maynard and Joseph 2008).

While the IPEDS and SDR datasets were very useful in understanding the growth of non-tenure track faculty over the decades, they each possess limitations. For the follow section, we draw on the HERI Faculty Survey. HERI is a cross-sectional instrument generalizing to all postsecondary instructional faculty. We draw specifically on their component related to non-tenure track faculty. The information on these faculty, particularly the part-time, non-tenure track faculty, is far more detailed than IPEDS or SDR. In the following figure (Figure 27), we take advantage of this detail by reproducing the classifcation schema formalized by Leslie and Gappa (1993).

``` r
load(file.path(Private_Cache,"HERI_Class.RData"))
source(file.path(Munge, "01_Merge_the_data.R"))
source(file.path(Munge, "02_Clean_the_data.R"))
#source(file.path(Munge, "03_Recode_4_Impute.R"))
source(file.path(Munge, "HERI_vars.R"))
source(file.path(Munge, "03_Recode_HERI.R"))
```

``` r
ggplot(data=subset(df, !is.na(GAPPANTT)), aes(x=GAPPANTT)) + geom_bar(fill="firebrick") + xlab("Adjunct Type") + ylab("Count") +
labs(title="Gappa and Leslie Classification of Adjunct Faculty (1993)", subtitle= "Schema Applied to HERI Faculty Survey 2010") +
labs(caption = "Evans & Furstenberg. HERI Faculty Survey 2010") + theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10))
```

![Caption](graphs/Gappa_Classification-1.png)

As shown above, half of the data we were supplied by HERI pertain to full-time, non-tenure track faculty. Aspiring academics are the largest subgroup of part-time faculty. In fact, there are more part-time aspiring academics in the sample than all other part-time adjuncts put together. Experts and freelancers each constitute about 10% of the non-tenure track population. Career-enders constitute a small minority of 4%.

Dr. Furstenberg developed a second classification system that splits adjuncts into four categories, rather than five. In his framework, adjunct classification takes on two dimensions: whether the faculty member worked full-time or part-time, and whether the adjunct held work in addition to his or her adjunct position. Professional adjuncts were part-time faculty who held full-time careers outside of education. Itinerants were faculty members teaching piecemeal at two or more institutions. Some have referred to these individuals as "freedom flyers" as they tend to spend a considerable amount of time commuting between jobs. Single institution adjuncts, however, only hold one part-time position and no other career (inside or outside of academia). They are therefore employed by a single institution. His last category is the full-time, non-tenure track group of faculty members. When applying Furstenberg's schema, it divides the sample of part-time faculty members into equitably sized groups, each containing between 13% and 19% of the sample.

``` r
ggplot(data=subset(df, !is.na(ADJUNCT1)), aes(x=ADJUNCT1)) + geom_bar(fill="steelblue4") + xlab("Adjunct Type") + ylab("Count") +
labs(title="Furstenberg Classification of Adjunct Faculty (2015)") +
labs(caption = "Evans & Furstenberg. HERI Faculty Survey 2010") + theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10))
```

![Furstenberg (2016) Classification](graphs/Furstenberg_Classification-1.png)

Now we turn our attention to how adjunct classification compares across demographics, employment conditions and job activities. We rely on the Gappa and Leslie categorization here due to its prevalence in the literature. HERI allowed us to distinguish all five categories of adjuncts, derived from questions related to full-time/part-time status, (in)voluntary status, outside career status and whether retirement is imminent (with three years).

### Demographic Characteristics of Gappa Classification

``` r
GAPPAnames<-c("Full-time","Aspiring Academic","Career-Ender","Expert","Freelancer")
DemVars<-c("AGE","SEX","MARITAL2","RACEGROUP2","GENACT02","NATENGSP","NCHILD3","DEGEARN2","DEGWORK2")
table<-round(as.data.frame(nfCrossTable(data=df[DemVars],CTvar=as.integer(df$GAPPANTT))),2)
colnames(table)<-GAPPAnames
rownames(table)<-c("Age","Male","Married","White","Citizen","Native English","Avg. Children","BA or Less","Prof Degree","Ph.D.","Working on a Degree")
kable(table, caption = "Distribution of Adjunct Clusters by Demographic Characteristics")
```

|                     |  Full-time|  Aspiring Academic|  Career-Ender|  Expert|  Freelancer|
|---------------------|----------:|------------------:|-------------:|-------:|-----------:|
| Age                 |      48.59|              48.47|         65.64|   52.64|       52.36|
| Male                |       0.44|               0.47|          0.52|    0.60|        0.34|
| Married             |       0.77|               0.75|          0.80|    0.84|        0.83|
| White               |       0.84|               0.79|          0.92|    0.87|        0.90|
| Citizen             |       0.94|               0.95|          1.00|    0.97|        0.96|
| Native English      |       0.89|               0.89|          0.95|    0.96|        0.93|
| Avg. Children       |       1.50|               1.60|          2.07|    1.87|        1.79|
| BA or Less          |       0.05|               0.08|          0.05|    0.16|        0.08|
| Prof Degree         |       0.58|               0.69|          0.53|    0.70|        0.65|
| Ph.D.               |       0.37|               0.23|          0.42|    0.14|        0.28|
| Working on a Degree |       0.19|               0.27|          0.07|    0.13|        0.13|

In terms of demographic characteristics, full-time non-tenure track faculty tend to be married with children. Three-quarters are married and two-thirds of them have at least one child. They come from various segments of the age distribution, with sizeable numbers (39%) mid-career (age 40-55) and are evenly split between men and women. Half tend to have earned a Ph.D. and half are at the Master's level. There are also a few (5%) who have only earned a bachelor's (or lower).

Aspiring academics tend to closely resemble full-time, non-tenure track faculty. The only difference is that aspiring academics have been unable to obtain a full-time position in academia. Aspiring academics also are slightly less likely to have earned a Ph.D., which is probably closely related to why they have been unable to find full-time work in a single institution.

Career-enders are predictably older, with 96% of them 55 or older. They are overwelmingly white, which is an artifact of the time period when many of them were hired. However, the gender composition is evenly split. They also are more likely to have children, which again is related to the fact that they are older and have had more time to begin families. Aside from these features, career-enders are otherwise very similar to full-time adjuncts and aspiring academics.

Experts in academia tend to be white men (60% are men). They also tend to be older and are less likely to have earned a Ph.D.. Many of these characteristics make sense for an expert. They are older, having spent many years outside of academia gaining practical experience valuable to students. They are hired for this expertise, not for their educational credentials, per se.

Finally, freelancers are very similar to experts on most demographic characteristics. The one important exception is related to gender. Freelancers are much more likely to be female. Perhaps women pursue freelancing academic careers in order to be more available to their families. However, our analysis finds that freelancers are no more likely to have children than experts. Nonetheless, traditional gender roles likely have a role in determining why women become freelancers and men become adjunct experts.

### Institutional Characteristics of Gappa Classification

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=as.integer(df$GAPPANTT)),2)
colnames(table)<-GAPPAnames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Full-time|  Aspiring Academic|  Career-Ender|  Expert|  Freelancer|
|--------------------------------|----------:|------------------:|-------------:|-------:|-----------:|
| 2-year                         |       0.00|               0.04|          0.05|    0.04|        0.05|
| 4-year                         |       0.64|               0.69|          0.70|    0.68|        0.70|
| University                     |       0.36|               0.26|          0.24|    0.28|        0.26|
| Public                         |       0.38|               0.44|          0.44|    0.33|        0.40|
| Research I                     |       0.05|               0.04|          0.06|    0.04|        0.04|
| Research II                    |       0.21|               0.14|          0.10|    0.19|        0.14|
| R3/Doctoral                    |       0.09|               0.06|          0.06|    0.04|        0.06|
| Bachelors/Masters              |       0.64|               0.71|          0.72|    0.69|        0.71|
| Associates                     |       0.00|               0.04|          0.05|    0.04|        0.04|
| Other Inst.                    |       0.01|               0.00|          0.00|    0.00|        0.00|
| Highly Selective               |       0.14|               0.05|          0.09|    0.04|        0.07|
| Faculty very respectful        |       0.51|               0.55|          0.61|    0.58|        0.53|
| Administators very considerate |       0.22|               0.20|          0.23|    0.22|        0.23|
| Research valued                |       0.68|               0.61|          0.64|    0.64|        0.65|
| Teaching valued                |       0.89|               0.89|          0.94|    0.90|        0.91|

There are also differences in the actual work activities of adjuncts. While all adjuncts in the HERI sample are instructors in some capacity, full-time adjuncts are more likely than the other classes of adjuncts to have principle job responsibilities in other areas like administration and research. Full-time adjuncts are hired in all academic fields (Biglan classification), however, they are less likely to be found in the hard/pure sciences. They do work regularly in the hard/applied fields, however, like medicine and nursing. Nearly all full-time adjuncts have professional development opportunities available to them and, furthermore, the majority partipate in professional development on a regular basis. This behavior ties in to what we know about full-time employees. Employers make a greater effort to develop the staff they see as full-time and fully committed. Full-time adjuncts tend to have considerable teaching obligations. They teach nearly three courses on average and also tend to have larger classes than other adjunct types.

Regarding aspiring adjuncts, their principle activity in the vast majority of cases is teaching. Aspiring academics are found in all Biglan academic fields (although rarely in hard/pure sciences). Professional development is less likely part of an aspiring academic's work experiences. Many (20%) do not have opportunities for professional development and only a third ever participate in workplace trainings. Their teaching load resembles that of full-time adjuncts, despite the fact that they only work part-time. This high teaching load, no doubt, is part of the reason aspiring academics have difficulty securing full-time work. They teach almost as much as their full-time, non-tenure track collegues, often hold outside careers, and must prepare applications and interview for work. This is a sizeable amount of responsibly for someone only working part-time in academia.

Career-enders also tend to be teachers, although some have principle responsibilities in administration. Professional development opportunities tend to be available for career-enders and a small majority of this group of adjuncts actually takes part in training opportunities. Among part-time adjunct types, career-enders are the most likely to participate in professional development, perhaps because they are accomstomed to these kinds of trainings from the earlier part of their careers. Career-enders teach fewer classes on average (1.8 classes).

Similar to aspiring academics, the principle job activity of experts is to teach. Their teaching, however, tends to mainly be in the hard/applied sciences. So clinical faculty and those working in medicine are often adjunct experts. Professional development is widely available for experts, but they are less likely to actually participate in these trainings. This may be related to the fact that they are "experts" and additional training may not be useful or valuable to them. Experts teach 1.5 classes on average, with slightly smaller class sizes. This may be related to the applied nature of some of the courses they teach. There may be more hands-on activities requiring smaller class sizes for these adjuncts.

Freelancers resemble aspiring academics with regared to work activities, without quite as demanding of circumstances. The majority of them have principle teaching activities and they teach in the same fields as aspriring adjuncts. Professional development is generally avaialble and many are participants in these trainings. At 2.2 courses, their workload is slightly lighter than that of aspiring academics.

### Academic Departments of Gappa Classification

``` r
table<-round(prop.table(table(df$DEPTA, df$GAPPANTT),2),2) # DEPTA was aggregated by HERI
colnames(table)<-GAPPAnames
rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stats","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                   |  Full-time|  Aspiring Academic|  Career-Ender|  Expert|  Freelancer|
|-------------------|----------:|------------------:|-------------:|-------:|-----------:|
| Agri/Forestry     |       0.01|               0.00|          0.00|    0.00|        0.00|
| Biology           |       0.05|               0.03|          0.03|    0.02|        0.05|
| Business          |       0.08|               0.13|          0.06|    0.21|        0.06|
| Education         |       0.10|               0.10|          0.18|    0.08|        0.17|
| Engineering       |       0.02|               0.02|          0.02|    0.03|        0.01|
| English           |       0.07|               0.09|          0.12|    0.03|        0.13|
| Fine Arts         |       0.06|               0.12|          0.06|    0.08|        0.07|
| Health-related    |       0.12|               0.05|          0.06|    0.12|        0.07|
| History/PoliSci   |       0.02|               0.04|          0.07|    0.01|        0.04|
| Humanities        |       0.09|               0.08|          0.06|    0.04|        0.07|
| Math/Stats        |       0.05|               0.04|          0.07|    0.03|        0.07|
| Non-technical     |       0.19|               0.15|          0.11|    0.17|        0.11|
| Technical         |       0.03|               0.03|          0.02|    0.05|        0.02|
| Physical Sciences |       0.04|               0.02|          0.03|    0.02|        0.04|
| Social Sciences   |       0.08|               0.12|          0.10|    0.11|        0.08|

### Employment Conditions of Gappa Classification

``` r
df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # important to include the combined salary variable so that salary insn't exclude from the clustering.
WORKVARS<-c("PRINACT2","FULLSTAT","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","SALARYALL","COURSENUM","PROFDEVFAC")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=as.integer(df$GAPPANTT)),2)
colnames(table)<-GAPPAnames
rownames(table)<-c("Teaching","Research","Administration/Other","Full-time","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses","Prof. Dev. Rating")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                      |  Full-time|  Aspiring Academic|  Career-Ender|    Expert|  Freelancer|
|----------------------|----------:|------------------:|-------------:|---------:|-----------:|
| Teaching             |       0.80|               0.97|          0.92|      0.98|        0.93|
| Research             |       0.04|               0.01|          0.02|      0.01|        0.01|
| Administration/Other |       0.17|               0.02|          0.06|      0.02|        0.06|
| Full-time            |       1.00|               0.00|          0.00|      0.00|        0.00|
| Assistant Professor  |       0.32|               0.08|          0.05|      0.07|        0.10|
| Associate Professor  |       0.11|               0.04|          0.06|      0.05|        0.07|
| Instructor           |       0.27|               0.51|          0.31|      0.59|        0.47|
| Lecturer             |       0.25|               0.32|          0.21|      0.23|        0.24|
| Professor            |       0.06|               0.06|          0.37|      0.06|        0.13|
| Union member         |       0.13|               0.23|          0.25|      0.10|        0.26|
| Health benefits      |       0.95|               0.54|          0.59|      0.31|        0.52|
| Retirement           |       0.94|               0.56|          0.65|      0.34|        0.59|
| Avg. Salary          |   59270.56|           15868.56|      23485.43|  10302.64|    18674.39|
| Avg. Courses         |       2.80|               2.59|          1.85|      1.50|        2.21|
| Prof. Dev. Rating    |       0.23|              -0.27|          0.10|     -0.38|       -0.11|

In terms of employment conditions, different types of adjunct face different types of conditions. As one would expect, full-time, non-tenure track faculty tend to have little or no work outside of their principle job. This makes sense, as they are already dedicating 35 hours or more to their principle job. As they are full-time workers, they also tend to have much stronger institutional support in terms of salary and perquisites. They earn good salaries (60,000/yr) and the vast majority have health insurance and retirement contributions. Perhaps because of this support, few of them have organized as members of labor unions.

Nearly half of aspriring academics have some or a lot of work outside of their principle academic position. Many of these aspiring academics, no doubt, are the itinerants or freeway fliers trying to piece together a career and work towards a full-time position. Half of them receive retirment benefits and half have employer contributions to their healthcare. Only a quarter have union membership. Their institutional salary is low ($16,000), which is probably why so many of them have second or even third jobs. Their family income is more robust ($70,000), suggesting that these individuals may have spouses taking on considerable responsibilities for family finances.

Career-enders tend to have similar job conditions as aspiring academics, only they are less likely to have jobs outside of their academic appointment. Their salary also tends to be higher, perhaps because they are older, have more job connections and more experience. They are also more likely to have health and retirement benefits. Maintaining health and retirement benefits is probably an important part of why career-enders like to stay active in academia before completely retiring. In all other ways, their work conditions seem closely related to what aspiring academics expereince.

Three-quarters of experts work substantial hours outside of their academic appointment. They are also less likely to have healthcare contributions, retirement benefits and union representation. Perhaps many of them already have health and retirement benefits from outside careers. Their institutional salary is also pretty low, but their overall income is by far the highest of all adjunct types. This all conforms with our expectations regarding experts. These individuals do not participate in academia for financial reasons. They probably do it simply because they like sharing their work experience with young people, being exposed to a stimulating, intellectual environment or they need a productive break from their full-time careers.

Finally, Freelancers are nearly identical to aspiring academics, except that they are much less likely to have substantial work outside of their principle academic appointment. Freelancers teach part-time proabably because they enjoy teaching, but do not necessarily want to do it full-time. A part-time job probably gives them great flexibility to meet family demands or other responsibilities.

### Exploratory

``` r
clusters <- hclust(dist(FTdfi))
```

    ## Warning in dist(FTdfi): NAs introduced by coercion

``` r
plot(clusters)
```

![](graphs/unnamed-chunk-10-1.png)

``` r
clusterCut <- cutree(clusters, 3)
```
