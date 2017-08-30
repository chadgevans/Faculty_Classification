Faculty Classification
================
Chad Evans

Built with R version 3.3.2. Last run on 2017-08-30.

Contents
--------

-   [Configure](#configure)
    -   Directories
    -   Libraries
-   [Munge](#munge)
    -   Subset
    -   Missing Data
    -   Variables
    -   Imputation
    -   Data
-   [Cluster Analysis](#cluster-analysis)
    -   Determining the Number of Clusters
    -   K-means Clustering
-   Tabulations
    -   [Demography Table](#demography-table)
    -   [Institution Table](#institution-table)
    -   [Department Table](#departmental-table)
    -   [Employment Table](#employment-table)

Configure
---------

Munge
-----

``` r
load(file.path(Private_Cache,"HERI_Class.RData"))
source(file.path(Munge, "01_Merge_the_data.R"))
source(file.path(Munge, "02_Clean_the_data.R"))
source(file.path(Munge, "HERI_vars.R"))
```

``` r
raw_n<-nrow(df)
df<-df %>% filter(!(TENURE=="Tenured")) # reduces from 8980 to 8450
filtered_n<-nrow(df)
```

The original 2010 HERI data had 8980 observations. After removing part-time faculty with Tenure (an anomoly outside the scope of this research), we have a total of 8450 faculty.

### Missing Data

``` r
miss_pct<-df %>%
  map_dbl(function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
ggplot(aes(x=reorder(var, -miss), y=miss)) +
geom_bar(stat='identity', fill='red') +
labs(x='', y='% missing', title='Percent missing data by feature') +
theme(axis.text.x=element_text(angle=90, hjust=1))
```

![](graphs/unnamed-chunk-1-1.png)

The missingness in these data is less concerning than it appears. There are 31 variables missing more than 50 percent of their observations; however, this is because they are mostly only questions that pertain to part-time faculty. Full-time faculty did not respond to this battery of questions and so these questions will not play any role in the analysis. There are an additional 10 variables with 5-10 percent missingness. This is not ideal, but the level of missingness is pretty small. The vast majority of our variables (112 of them) have less than 5 percent missingness. These will be very useful in helping identify clusters of faculty with these characteristics. Despite this small amount of missingness, I still anticipate imputing data. With so many features, surely listwise deletion would lead to an unacceptable deletion of most of our data.

``` r
df<-df %>% select(one_of(c(ADMINVARS,WORKVARS,SALARYVARS,INSTVARS,BACKVARS,ATTITUDEVARS,OTHERVARS,PROFDEVVARS, FACTORVARS, PRODUCTIVITYVARS,STRESSVARS,SATISVARS,PTVARS)))
HIGHMISS<-labels(which(miss_pct>50))
OMIT<-c(ADMINVARS,PTVARS,HIGHMISS) 

df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # important to include the combined salary variable so that salary insn't exclude from the clustering.

df<-df %>% select(-one_of(OMIT))
```

We will conduct k-means clustering using as many meaningful faculty features as possible. We remove the administrative variables like subject IDs and institution IDs that contribute no meaningful information. We also remove all variables corresponding only to part-time faculty and the handful of other characterstics with missingness greater than 50 percent. Our final data frame for k-means clustering consists of 8450 observations and 113 faculty features.

``` r
#Imputing takes 2-3 hours
tempData<-df %>% select(-one_of(OMIT)) %>% mice(m=1,maxit=50,seed=500) # originally used meth='pmm'
#save(tempData, file=file.path(Private_Cache,"tempData.RData"))
IData <- complete(tempData,1)
save(IData, file=file.path(Private_Cache,"IData.RData"))
```

As expected, listwise deletion across 117 features is impossible. In fact, there is not a single observation with complete data. We thus must consider a method to deal with the missingness. I opt for single imputiation. In some cases, like regression, it would probably be worth multiply imputing to get standard errors correct. However, this study is not using standard errors and is merely an experimental procedure to find coherence in the data. I thus opt to singly impute the data for reasons of simplicity.

I set the maximum iterations to 50, rather than the default of 5. This will give the chained equations more attempts to converge on a good imputed value for each cell. Predictive mean matching was used for numeric variables. Logistic regression was used to impute binary data. Polytomous regression imputation was used for unordered categorical variables.

Data
----

``` r
load(file.path(Private_Cache,"IDataOLD.RData")) # Singly imputed data
data<-IData
```

Cluster Analysis
----------------

``` r
data<-data.frame(model.matrix(~ ., data=data , contrasts.arg = lapply(data[,sapply(data, is.factor)], contrasts, contrasts=FALSE)))
```

To implement k-means clustering, all data must be numeric. This required converting binary factors to zero and ones. Multinomial variables needed to be converted into a matrix of dummy variables. Some claim that it is ineffective to convert categorical predictors into binaries in this fashion. But it is necessary if you want multinomial features to factor into the analysis. As this chapter is descriptive and exploratory, I implemented this traditional practice of creating a matrix of numeric variables.

### Determining the number of Clusters.

When implementing k-means clustering, one must specify the number of means to cluster around in the data. The most common approach to choosing the number of clusters is to plot how the within sum of squared residuals decreases as additional means are added and identify the "elbow." This is the point where explained variation begins its platau.

``` r
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Determining the Number of Clusters")
}
wssplot(data, nc=7) # put in # of clusters here
```

![](graphs/Determining_Number_Clusters-1.png)

The elbow suggests that four clusters sufficiently explain most of the variation. I therefore opt to go witih four means in the k-means clustering analysis.

### K-Means Clustering

``` r
d<-data[,-1] # git rid of the intercept (no variation, convergence issues)
d<-scale(d)
kmeans.obj<- d %>% kmeans(4, nstart = 10, algorithm = c("Hartigan-Wong"))
df$cluster<-kmeans.obj$cluster
```

Before conducting k-means clustering, all variables were normalized so that features with the greatest ranges did not have undue influence on the formation of clusters.

To conduct the k-means analysis, it is important to choose random starting points for the means. This helps prevent the algorithm (Hartingan-Wong 1979) from converging on suboptimal means. I used 10 different sets of starting points to identify the means that best summarize the information in the data.

Tabulations
-----------

``` r
source(file.path(Munge, "03_Recode_HERI.R"))
```

``` r
clusters<-table(df$cluster)
print(clusters)
```

    ## 
    ##    1    2    3    4 
    ## 2244  950 2557 3229

``` r
C1<-paste("Cluster 1 (n=",clusters[1],")",sep = "")
C2<-paste("Cluster 2 (n=",clusters[2],")",sep = "")
C3<-paste("Cluster 3 (n=",clusters[3],")",sep = "")
C4<-paste("Cluster 4 (n=",clusters[4],")",sep = "")
Clusternames<-c(C1,C2,C3,C4)
```

### Demography Table

``` r
DemVars<-c("AGE","SEX","MARITAL2","NCHILD3","RACEGROUP2","GENACT02","NATENGSP","DEGEARN2","DEGWORK2")
table<-round(nfCrossTable(data=df[DemVars],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Age","Male","Married","No Children","One Child","Multiple Children","White","Citizen","Native English","BA or Less","Prof Degree","Ph.D.","Working on a Degree")
kable(table, caption = "Distribution of Adjunct Clusters by Demographic Characteristics")
```

|                     |  Cluster 1 (n=2244)|  Cluster 2 (n=950)|  Cluster 3 (n=2557)|  Cluster 4 (n=3229)|
|---------------------|-------------------:|------------------:|-------------------:|-------------------:|
| Age                 |               47.37|              50.19|               52.14|               49.78|
| Male                |                0.41|               0.45|                0.51|                0.44|
| Married             |                0.75|               0.79|                0.80|                0.77|
| No Children         |                0.39|               0.29|                0.24|                0.31|
| One Child           |                0.17|               0.14|                0.14|                0.14|
| Multiple Children   |                0.44|               0.57|                0.63|                0.56|
| White               |                0.80|               0.84|                0.84|                0.85|
| Citizen             |                0.93|               0.94|                0.97|                0.95|
| Native English      |                0.88|               0.90|                0.93|                0.90|
| BA or Less          |                0.05|               0.06|                0.12|                0.06|
| Prof Degree         |                0.57|               0.56|                0.75|                0.58|
| Ph.D.               |                0.38|               0.38|                0.14|                0.36|
| Working on a Degree |                0.24|               0.13|                0.19|                0.20|

### Institution Table

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","BIGLAN","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Hard/Applied","Hard/Pure","Soft/Applied","Soft/Pure","Other Biglan","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Cluster 1 (n=2244)|  Cluster 2 (n=950)|  Cluster 3 (n=2557)|  Cluster 4 (n=3229)|
|--------------------------------|-------------------:|------------------:|-------------------:|-------------------:|
| 2-year                         |                0.01|               0.00|                0.05|                0.01|
| 4-year                         |                0.67|               0.57|                0.69|                0.67|
| University                     |                0.32|               0.42|                0.26|                0.32|
| Public                         |                0.48|               0.34|                0.36|                0.38|
| Research I                     |                0.04|               0.07|                0.03|                0.05|
| Research II                    |                0.19|               0.26|                0.15|                0.18|
| R3/Doctoral                    |                0.07|               0.09|                0.06|                0.08|
| Bachelors/Masters              |                0.67|               0.56|                0.71|                0.68|
| Associates                     |                0.01|               0.00|                0.05|                0.01|
| Other Inst.                    |                0.01|               0.01|                0.00|                0.01|
| Hard/Applied                   |                0.27|               0.31|                0.39|                0.35|
| Hard/Pure                      |                0.03|               0.04|                0.01|                0.05|
| Soft/Applied                   |                0.13|               0.24|                0.23|                0.18|
| Soft/Pure                      |                0.50|               0.22|                0.30|                0.35|
| Other Biglan                   |                0.06|               0.19|                0.07|                0.07|
| Highly Selective               |                0.09|               0.13|                0.03|                0.14|
| Faculty very respectful        |                0.27|               0.45|                0.65|                0.65|
| Administators very considerate |                0.04|               0.27|                0.26|                0.28|
| Research valued                |                0.40|               0.70|                0.66|                0.81|
| Teaching valued                |                0.74|               0.86|                0.95|                0.98|

### Departmental Table

``` r
table<-round(prop.table(table(df$DEPTA, df$cluster),2),2) # DEPTA was aggregated by HERI
colnames(table)<-Clusternames
rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stats","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                   |  Cluster 1 (n=2244)|  Cluster 2 (n=950)|  Cluster 3 (n=2557)|  Cluster 4 (n=3229)|
|-------------------|-------------------:|------------------:|-------------------:|-------------------:|
| Agri/Forestry     |                0.01|               0.01|                0.00|                0.01|
| Biology           |                0.05|               0.04|                0.02|                0.04|
| Business          |                0.06|               0.06|                0.17|                0.08|
| Education         |                0.06|               0.12|                0.14|                0.11|
| Engineering       |                0.02|               0.02|                0.02|                0.02|
| English           |                0.13|               0.04|                0.06|                0.07|
| Fine Arts         |                0.13|               0.03|                0.07|                0.07|
| Health-related    |                0.07|               0.14|                0.07|                0.12|
| History/PoliSci   |                0.05|               0.01|                0.02|                0.03|
| Humanities        |                0.11|               0.05|                0.06|                0.08|
| Math/Stats        |                0.03|               0.01|                0.05|                0.06|
| Non-technical     |                0.14|               0.33|                0.15|                0.15|
| Technical         |                0.03|               0.02|                0.04|                0.03|
| Physical Sciences |                0.03|               0.04|                0.01|                0.05|
| Social Sciences   |                0.09|               0.08|                0.10|                0.09|

### Employment Table

``` r
WORKVARS<-c("PRINACT2","FULLSTAT","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","SALARYALL","COURSENUM","PROFDEVFAC")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration","Other","Full-time","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses","Prof. Dev. Rating")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                     |  Cluster 1 (n=2244)|  Cluster 2 (n=950)|  Cluster 3 (n=2557)|  Cluster 4 (n=3229)|
|---------------------|-------------------:|------------------:|-------------------:|-------------------:|
| Teaching            |                0.96|               0.15|                0.97|                0.96|
| Research            |                0.02|               0.13|                0.00|                0.01|
| Administration      |                0.01|               0.55|                0.00|                0.00|
| Other               |                0.02|               0.16|                0.02|                0.03|
| Full-time           |                0.50|               0.90|                0.03|                0.77|
| Assistant Professor |                0.20|               0.27|                0.06|                0.29|
| Associate Professor |                0.05|               0.18|                0.04|                0.09|
| Instructor          |                0.36|               0.27|                0.61|                0.25|
| Lecturer            |                0.35|               0.14|                0.21|                0.27|
| Professor           |                0.04|               0.14|                0.07|                0.08|
| Union member        |                0.25|               0.09|                0.15|                0.16|
| Health benefits     |                0.87|               0.96|                0.27|                0.92|
| Retirement          |                0.86|               0.97|                0.31|                0.93|
| Avg. Salary         |            35363.96|           73436.25|            11485.56|            50186.92|
| Avg. Courses        |                3.20|               1.09|                1.86|                3.04|
| Prof. Dev. Rating   |               -0.06|               0.35|               -0.37|                0.23|
