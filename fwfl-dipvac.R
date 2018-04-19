# load packages
library(lme4)
library(party)

df <- read.csv("data.csv")

summary(df)
colnames(df)

df <- droplevels(df[df$dep.var!="ah",]) # remove ahs

prop.table(xtabs(~dep.var+role,df),2)

# overall df
levels(df$gender) <- c("F","F","M","M")

ctree <- ctree(dep.var ~ group + role + gender,df)
plot(ctree)

# farmer df
df.f <- droplevels(df[df$role=="farmer",]) # drop ivers
prop.table(xtabs(~dep.var+gender,df.f),2) # xtabs by gender

df.f$yob <- as.numeric(as.character(df.f$yob)) # coerce df.f yob to numeric

ctree.f <- ctree(dep.var ~ yob + group + gender,df.f)
plot(ctree.f)

# iver df
df.iv <- droplevels(df[df$role=="interviewer",]) # drop farmers
prop.table(xtabs(~dep.var+gender,df.iv),2)
ctree.iv <- ctree(dep.var ~ gender,df.iv)
plot(ctree.iv)

xtabs(~dep.var+indiv,df.iv)
