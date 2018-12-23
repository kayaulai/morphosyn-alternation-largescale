library(dplyr)
library(brms)
library(readr)

setwd("G:\\§Úªº¶³ºÝµwºÐ\\corpus priming\\eng")

#Utility functions
splitConjString = function(conjString){
  return(strsplit(conjString,";;")[[1]])
}

#Read CSV
clauseTable = read_csv("dec22-table-withintersentence.csv")
clauseTable$PredFreq = parse_double(clauseTable$PredFreq)
clauseTable$PredSylCo = parse_double(clauseTable$PredSylCo)
clauseTable$Voice = parse_factor(clauseTable$Voice, levels = c("Act","Pass"))

#Select only sentences that are potentially passivisable
clauseTablePassivisable = clauseTable %>% filter(Voice == "Pass" || ObjHead != "/" || OblHead != "/")
clauseTablePassivisable %>% filter(Voice != "Act" & OblCase != "by")
clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentSylCo = case_when(
  Voice == "Act" & SubjSylCo != "/" & !is.na(as.numeric(SubjSylCo)) ~ as.numeric(SubjSylCo),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblSylCo))  ~ as.numeric(OblSylCo),
  TRUE ~ 0
))
clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeSylCo = case_when(
  Voice == "Act" & OblSylCo != "/" & !is.na(as.numeric(OblSylCo)) ~ as.numeric(OblSylCo),
  Voice == "Act" & ObjSylCo != "/" & !is.na(as.numeric(ObjSylCo)) ~ as.numeric(ObjSylCo),
  Voice == "Pass" & !is.na(as.numeric(SubjSylCo))  ~ as.numeric(SubjSylCo),
  TRUE ~ 0
))

clauseTablePassivisable = clauseTablePassivisable[1:3000,]


beta = c(0,0)
phi = 0
f = function(x, z){return(-2 + t(beta) %*% x + phi * z)}
x1 = rnorm(100)
x2 = rnorm(100)

t=1
voices = rbinom(1,1,
                exp(f(c(x1[t],x2[t]),.5))/
                  (1+exp(f(c(x1[t],x2[t]),.5))))
for(t in 2:100){
  voices = append(voices, rbinom(1,1,exp(f(c(x1[t],x2[t]),voices[t-1]))/
                                   (1+exp(f(c(x1[t],x2[t]),voices[t-1])))))
}
data = data.frame(voices,x1,x2)
glm(voices ~ x1 + x2 + 0)

fake_model = brm(voices ~ x1 + x2, data = data,  family = bernoulli(link = "logit"), chains = 1)


pred_only_model_3000 = brm(Voice ~ (1 | PredLemma) + AgentSylCo + PredTense + PredMorph + PredFreq + ThemeSylCo + PredSylCo, data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), chains = 1)

pred_coefs = coef(pred_only_model)
