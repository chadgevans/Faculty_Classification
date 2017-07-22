# Salary and Income
df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # returns the max of the two
df$AGE<-2010-df$BIRTHYR