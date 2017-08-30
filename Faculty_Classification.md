Faculty Classification
================
Chad Evans

Built with R version 3.3.2. Last run on 2017-08-30.

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
#The original data had `r raw_n` observations.  After removing part-time faculty with Tenure, we have a total of #`r filtered_n`.  This is a decrease of `r (raw_n-filtered_n)/raw_n`.
```

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

The missingness in these data is very low. About 20 variables have 3-10 percent missing. The vast majority have trivial amounts of missingness. When applying listwise deletion, 100 percent of observations are lot. This is because there are so many covariates and the missingness really adds up. It is still worth considering listwise deletion. We'll compare the results with a dataset that has been singly imputated, in order to capitalize on all available information. Single imputation is less concerning here because we are not interested in standard errors. We just are trying to induce a classification schema.

``` r
df<-df %>% select(one_of(c(ADMINVARS,WORKVARS,SALARYVARS,INSTVARS,BACKVARS,ATTITUDEVARS,OTHERVARS,PROFDEVVARS, FACTORVARS, PRODUCTIVITYVARS,STRESSVARS,SATISVARS,PTVARS)))

HIGHMISS<-labels(which(miss_pct>50))
OMIT<-c(ADMINVARS,PTVARS)
```

``` r
#Imputing takes 2-3 hours
tempData<-df %>% select(-one_of(OMIT)) %>% mice(m=1,maxit=50,meth='pmm',seed=500)
#save(tempData, file=file.path(Private_Cache,"tempData.RData"))
IData <- complete(tempData,1)
save(IData, file=file.path(Private_Cache,"IData.RData"))
```

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

Some claim it is ineffective to convert categorical predictors into binaries. In any case, that is what we do here. This chapter is purely exploratory, so I am not concerned.

### Determining the number of Clusters.

I used the "elbow method".

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

The elbow plot suggests that four clusters best describe these data. I'll go with four.

### K-Means Clustering

``` r
d<-data[,-1] # git rid of the intercept (no variation, convergence issues)
d<-scale(d)
kmeans.obj<- d %>% kmeans(4, nstart = 20)
df$cluster<-kmeans.obj$cluster
```

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
    ## 2558 2242 3213  967

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

|                     |  Cluster 1 (n=2558)|  Cluster 2 (n=2242)|  Cluster 3 (n=3213)|  Cluster 4 (n=967)|
|---------------------|-------------------:|-------------------:|-------------------:|------------------:|
| Age                 |               52.14|               47.37|               49.75|              50.27|
| Male                |                0.51|                0.41|                0.44|               0.45|
| Married             |                0.80|                0.75|                0.77|               0.79|
| No Children         |                0.24|                0.39|                0.31|               0.28|
| One Child           |                0.14|                0.17|                0.14|               0.14|
| Multiple Children   |                0.63|                0.44|                0.56|               0.57|
| White               |                0.84|                0.80|                0.85|               0.84|
| Citizen             |                0.97|                0.93|                0.95|               0.94|
| Native English      |                0.93|                0.88|                0.90|               0.90|
| BA or Less          |                0.12|                0.05|                0.06|               0.06|
| Prof Degree         |                0.75|                0.57|                0.58|               0.56|
| Ph.D.               |                0.14|                0.38|                0.36|               0.38|
| Working on a Degree |                0.19|                0.24|                0.20|               0.13|

### Institution Table

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","BIGLAN","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Hard/Applied","Hard/Pure","Soft/Applied","Soft/Pure","Other Biglan","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Cluster 1 (n=2558)|  Cluster 2 (n=2242)|  Cluster 3 (n=3213)|  Cluster 4 (n=967)|
|--------------------------------|-------------------:|-------------------:|-------------------:|------------------:|
| 2-year                         |                0.05|                0.01|                0.01|               0.00|
| 4-year                         |                0.69|                0.67|                0.67|               0.58|
| University                     |                0.26|                0.32|                0.32|               0.42|
| Public                         |                0.36|                0.48|                0.38|               0.33|
| Research I                     |                0.03|                0.04|                0.05|               0.07|
| Research II                    |                0.15|                0.19|                0.18|               0.25|
| R3/Doctoral                    |                0.06|                0.07|                0.08|               0.09|
| Bachelors/Masters              |                0.71|                0.67|                0.68|               0.57|
| Associates                     |                0.05|                0.01|                0.01|               0.00|
| Other Inst.                    |                0.00|                0.01|                0.01|               0.01|
| Hard/Applied                   |                0.39|                0.27|                0.35|               0.31|
| Hard/Pure                      |                0.01|                0.03|                0.05|               0.03|
| Soft/Applied                   |                0.23|                0.13|                0.18|               0.25|
| Soft/Pure                      |                0.30|                0.50|                0.35|               0.22|
| Other Biglan                   |                0.07|                0.06|                0.07|               0.19|
| Highly Selective               |                0.03|                0.09|                0.14|               0.13|
| Faculty very respectful        |                0.65|                0.27|                0.65|               0.44|
| Administators very considerate |                0.26|                0.04|                0.28|               0.27|
| Research valued                |                0.66|                0.40|                0.81|               0.70|
| Teaching valued                |                0.95|                0.74|                0.98|               0.86|

### Departmental Table

``` r
table<-round(prop.table(table(df$DEPTA, df$cluster),2),2) # DEPTA was aggregated by HERI
colnames(table)<-Clusternames
rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stats","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                   |  Cluster 1 (n=2558)|  Cluster 2 (n=2242)|  Cluster 3 (n=3213)|  Cluster 4 (n=967)|
|-------------------|-------------------:|-------------------:|-------------------:|------------------:|
| Agri/Forestry     |                0.00|                0.01|                0.01|               0.01|
| Biology           |                0.02|                0.05|                0.04|               0.04|
| Business          |                0.17|                0.06|                0.08|               0.07|
| Education         |                0.14|                0.06|                0.11|               0.12|
| Engineering       |                0.02|                0.02|                0.02|               0.02|
| English           |                0.06|                0.13|                0.07|               0.04|
| Fine Arts         |                0.07|                0.13|                0.07|               0.04|
| Health-related    |                0.07|                0.07|                0.12|               0.14|
| History/PoliSci   |                0.02|                0.05|                0.03|               0.01|
| Humanities        |                0.06|                0.11|                0.09|               0.05|
| Math/Stats        |                0.05|                0.03|                0.06|               0.01|
| Non-technical     |                0.15|                0.14|                0.15|               0.32|
| Technical         |                0.04|                0.03|                0.03|               0.02|
| Physical Sciences |                0.01|                0.03|                0.05|               0.03|
| Social Sciences   |                0.10|                0.09|                0.09|               0.08|

### Employment Table

``` r
WORKVARS<-c("PRINACT2","FULLSTAT","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","SALARYALL","COURSENUM","PROFDEVFAC")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration","Other","Full-time","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses","Prof. Dev. Rating")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                     |  Cluster 1 (n=2558)|  Cluster 2 (n=2242)|  Cluster 3 (n=3213)|  Cluster 4 (n=967)|
|---------------------|-------------------:|-------------------:|-------------------:|------------------:|
| Teaching            |                0.97|                0.96|                0.96|               0.17|
| Research            |                0.00|                0.02|                0.01|               0.13|
| Administration      |                0.00|                0.01|                0.00|               0.54|
| Other               |                0.02|                0.02|                0.03|               0.16|
| Full-time           |                0.03|                0.50|                0.77|               0.90|
| Assistant Professor |                0.06|                0.20|                0.29|               0.27|
| Associate Professor |                0.04|                0.05|                0.09|               0.19|
| Instructor          |                0.61|                0.36|                0.25|               0.27|
| Lecturer            |                0.21|                0.36|                0.27|               0.14|
| Professor           |                0.07|                0.04|                0.08|               0.14|
| Union member        |                0.15|                0.25|                0.16|               0.09|
| Health benefits     |                0.27|                0.87|                0.92|               0.96|
| Retirement          |                0.31|                0.86|                0.93|               0.97|
| Avg. Salary         |            11485.77|            35342.05|            50157.19|           73218.24|
| Avg. Courses        |                1.86|                3.20|                3.04|               1.13|
| Prof. Dev. Rating   |               -0.37|               -0.06|                0.23|               0.36|
