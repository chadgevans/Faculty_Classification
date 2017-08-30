Faculty Classification
================
Chad Evans

Built with R version 3.3.2. Last run on 2017-08-29.

-   [Configure](#configure)
    -   Directories
    -   Libraries
-   [Munge](#munge)
-   [Exploratory Analysis](#exploratory-analysis)
    -   Missing Data
    -   Imputation
-   [Cluster Analysis](#cluster-analysis)
    -   K-means Clustering Algorithm
-   [Tabulations](#tabulations)

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
df<-df %>% select(one_of(c(ADMINVARS,WORKVARS,SALARYVARS,INSTVARS,BACKVARS,ATTITUDEVARS,OTHERVARS,PROFDEVVARS, FACTORVARS, PRODUCTIVITYVARS,STRESSVARS,SATISVARS,PTVARS)))

OMIT<-c(SVYVARS,INSTVARS,DEMGVARS,PTVARS,OTHERS)
```

``` r
df<-df %>% filter(!(TENURE=="Tenured")) # reduces from 8980 to 8450
```

``` r
lwdata<-df %>% select(-one_of(OMIT)) %>% na.omit()
save(lwdata, file=file.path(Private_Cache,"lwdata.RData"))

#Imputing takes 2-3 hours
tempData<-df %>% select(-one_of(OMIT)) %>% mice(m=1,maxit=50,meth='pmm',seed=500)
save(tempData, file=file.path(Private_Cache,"tempData.RData"))
IData <- complete(tempData,1)
save(IData, file=file.path(Private_Cache,"IData.RData"))
```

Data
----

``` r
load(file.path(Private_Cache,"IData.RData")) # Singly imputed data
data<-IData
```

Exploratory Analysis
--------------------

### Missing Data

``` r
miss_pct<-df %>% select(-one_of(OMIT)) %>%
  map_dbl(function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
ggplot(aes(x=reorder(var, -miss), y=miss)) +
geom_bar(stat='identity', fill='red') +
labs(x='', y='% missing', title='Percent missing data by feature') +
theme(axis.text.x=element_text(angle=90, hjust=1))
```

The missingness in these data is very low. About 20 variables have 3-10 percent missing. The vast majority have trivial amounts of missingness. When applying listwise deletion, 100 percent of observations are lot. This is because there are so many covariates and the missingness really adds up. It is still worth considering listwise deletion. We'll compare the results with a dataset that has been singly imputated, in order to capitalize on all available information. Single imputation is less concerning here because we are not interested in standard errors. We just are trying to induce a classification schema.

Analysis
--------

Determine the number of Clusters.
---------------------------------

I used the "elbow method"

``` r
data<-data.frame(model.matrix(~ ., data=data , contrasts.arg = lapply(data[,sapply(data, is.factor)], contrasts, contrasts=FALSE)))
```

``` r
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

wssplot(data, nc=7) # put in # of clusters here
```

![](graphs/Determining_Number_Clusters-1.png)

Cluster Analysis
----------------

For both imputed data and listwise deletion, elbow plots suggests four or five clusters. I'll go with four.

``` r
# data<-scale(data) CONSIDER WHEATHER TO SCALE
kmeans.obj<- data %>% kmeans(4, nstart = 20)
df$cluster<-kmeans.obj$cluster
```

Tabulations
-----------

``` r
source(file.path(Munge, "03_Recode_HERI.R"))
```

``` r
table(df$cluster)
```

    ## 
    ##    1    2    3    4 
    ## 3855   10 1093 4022

``` r
Clusternames<-c("Cluster1 (n=3855)","Cluster2 (n=10)","Cluster3 (n=1093)","Cluster4 (n=4022)")
```

``` r
DemVars<-c("AGE","SEX","MARITAL2","NCHILD3","RACEGROUP2","GENACT02","NATENGSP","DEGEARN2","DEGWORK2")
table<-round(nfCrossTable(data=df[DemVars],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Age","Male","Married","No Children","One Child","Multiple Children","White","Citizen","Native English","BA or Less","Prof Degree","Ph.D.","Working on a Degree")
kable(table, caption = "Distribution of Adjunct Clusters by Demographic Characteristics")
```

|                     |  Cluster1 (n=3855)|  Cluster2 (n=10)|  Cluster3 (n=1093)|  Cluster4 (n=4022)|
|---------------------|------------------:|----------------:|------------------:|------------------:|
| Age                 |              50.56|             50.4|              53.43|              48.28|
| Male                |               0.48|              0.3|               0.54|               0.41|
| Married             |               0.78|              0.9|               0.82|               0.76|
| No Children         |               0.28|              0.3|               0.25|               0.35|
| One Child           |               0.15|              0.2|               0.13|               0.14|
| Multiple Children   |               0.57|              0.5|               0.62|               0.51|
| White               |               0.83|              0.8|               0.86|               0.83|
| Citizen             |               0.96|              1.0|               0.95|               0.94|
| Native English      |               0.91|              0.9|               0.91|               0.89|
| BA or Less          |               0.10|              0.1|               0.06|               0.05|
| Prof Degree         |               0.70|              0.7|               0.49|               0.59|
| Ph.D.               |               0.20|              0.2|               0.45|               0.36|
| Working on a Degree |               0.22|              0.3|               0.12|               0.20|

``` r
INSTVARS<-c("INSTTYPE","INSTCONT","CARNEGIE","BIGLAN","SELECTIVITY2","INSTDESCR03","INSTDESCR08","INSTOPN10","INSTOPN11")
table<-round(nfCrossTable(data=df[INSTVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("2-year","4-year","University","Public","Research I","Research II","R3/Doctoral","Bachelors/Masters","Associates","Other Inst.","Hard/Applied","Hard/Pure","Soft/Applied","Soft/Pure","Other Biglan","Highly Selective","Faculty very respectful","Administators very considerate","Research valued","Teaching valued")
kable(table, caption = "Distribution of Adjunct Clusters by Institutional Characteristics")
```

|                                |  Cluster1 (n=3855)|  Cluster2 (n=10)|  Cluster3 (n=1093)|  Cluster4 (n=4022)|
|--------------------------------|------------------:|----------------:|------------------:|------------------:|
| 2-year                         |               0.05|             0.00|               0.00|               0.00|
| 4-year                         |               0.69|             0.80|               0.47|               0.69|
| University                     |               0.26|             0.20|               0.53|               0.31|
| Public                         |               0.41|             0.30|               0.27|               0.42|
| Research I                     |               0.04|             0.00|               0.10|               0.03|
| Research II                    |               0.14|             0.20|               0.30|               0.18|
| R3/Doctoral                    |               0.06|             0.00|               0.11|               0.08|
| Bachelors/Masters              |               0.71|             0.80|               0.46|               0.70|
| Associates                     |               0.05|             0.00|               0.00|               0.00|
| Other Inst.                    |               0.00|             0.00|               0.03|               0.01|
| Hard/Applied                   |               0.33|             0.30|               0.48|               0.31|
| Hard/Pure                      |               0.02|             0.00|               0.04|               0.04|
| Soft/Applied                   |               0.20|             0.30|               0.19|               0.18|
| Soft/Pure                      |               0.39|             0.20|               0.18|               0.38|
| Other Biglan                   |               0.07|             0.20|               0.12|               0.08|
| Highly Selective               |               0.04|             0.00|               0.19|               0.12|
| Faculty very respectful        |               0.57|             0.40|               0.49|               0.51|
| Administators very considerate |               0.21|             0.20|               0.27|               0.20|
| Research valued                |               0.61|             0.67|               0.70|               0.67|
| Teaching valued                |               0.90|             1.00|               0.89|               0.90|

``` r
table<-round(prop.table(table(df$DEPTA, df$cluster),2),2) # DEPTA was aggregated by HERI
colnames(table)<-Clusternames
rownames(table)<-c("Agri/Forestry","Biology","Business","Education","Engineering","English","Fine Arts","Health-related","History/PoliSci","Humanities","Math/Stas","Non-technical","Technical","Physical Sciences","Social Sciences")
kable(table, caption = "Distribution of Adjunct Clusters by Departmental Characteristics")
```

|                   |  Cluster1 (n=3855)|  Cluster2 (n=10)|  Cluster3 (n=1093)|  Cluster4 (n=4022)|
|-------------------|------------------:|----------------:|------------------:|------------------:|
| Agri/Forestry     |               0.00|              0.0|               0.00|               0.01|
| Biology           |               0.03|              0.0|               0.04|               0.05|
| Business          |               0.13|              0.0|               0.12|               0.06|
| Education         |               0.12|              0.2|               0.09|               0.10|
| Engineering       |               0.02|              0.0|               0.05|               0.01|
| English           |               0.09|              0.1|               0.03|               0.08|
| Fine Arts         |               0.10|              0.2|               0.03|               0.08|
| Health-related    |               0.06|              0.2|               0.21|               0.09|
| History/PoliSci   |               0.03|              0.0|               0.01|               0.03|
| Humanities        |               0.07|              0.0|               0.03|               0.10|
| Math/Stas         |               0.05|              0.0|               0.02|               0.05|
| Non-technical     |               0.14|              0.3|               0.22|               0.17|
| Technical         |               0.03|              0.0|               0.03|               0.03|
| Physical Sciences |               0.02|              0.0|               0.04|               0.04|
| Social Sciences   |               0.11|              0.0|               0.07|               0.08|

``` r
WORKVARS<-c("PRINACT2","FULLSTAT","ACADRANK","GENACT01","HEALTHBENEFITS", "RETIREBENEFITS","SALARYALL","COURSENUM")
table<-round(nfCrossTable(data=df[WORKVARS],CTvar=df$cluster),2)
colnames(table)<-Clusternames
rownames(table)<-c("Teaching","Research","Administration","Other","Full-time","Assistant Professor","Associate Professor","Instructor","Lecturer","Professor","Union member","Health benefits","Retirement","Avg. Salary","Avg. Courses")
kable(table, caption = "Distribution of Adjunct Clusters by Work Characteristics")
```

|                     |  Cluster1 (n=3855)|  Cluster2 (n=10)|  Cluster3 (n=1093)|  Cluster4 (n=4022)|
|---------------------|------------------:|----------------:|------------------:|------------------:|
| Teaching            |               0.96|             0.70|               0.65|               0.86|
| Research            |               0.01|             0.00|               0.06|               0.03|
| Administration      |               0.01|             0.10|               0.25|               0.06|
| Other               |               0.02|             0.20|               0.04|               0.05|
| Full-time           |               0.03|             1.00|               0.96|               0.83|
| Assistant Professor |               0.07|             0.44|               0.29|               0.30|
| Associate Professor |               0.04|             0.22|               0.24|               0.07|
| Instructor          |               0.55|             0.22|               0.10|               0.30|
| Lecturer            |               0.27|             0.00|               0.19|               0.28|
| Professor           |               0.07|             0.11|               0.18|               0.05|
| Union member        |               0.19|             0.10|               0.13|               0.16|
| Health benefits     |               0.45|             0.90|               0.95|               0.93|
| Retirement          |               0.49|             0.90|               0.97|               0.92|
| Avg. Salary         |           11181.20|        631944.40|           89445.56|           47913.02|
| Avg. Courses        |               2.19|             2.60|               2.15|               2.97|
