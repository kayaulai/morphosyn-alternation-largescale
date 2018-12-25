library(dplyr)
library(brms)
library(readr)

setwd("G:\\§Úªº¶³ºÝµwºÐ\\corpus priming\\eng")

#Utility functions
splitConjString = function(conjString){
  return(strsplit(conjString,";;")[[1]])
}

gmean = function(x){
  return(prod(x)^(1/length(x)))
}

isNotNull = function(string){
  string = as.character(string)
  return(!any(splitConjString(string) == "/") & !any(splitConjString(string) == "")
         & string != "" & !is.na(string)
         & !any(splitConjString(string) == "NMA!"))
}
#Read CSV
clauseTable = read_csv("dec22-table-withintersentence.csv")
clauseTable$PredFreq = parse_double(clauseTable$PredFreq)
clauseTable$PredSylCo = parse_double(clauseTable$PredSylCo)
clauseTable$Voice = parse_factor(clauseTable$Voice, levels = c("Act","Pass"))
clauseTable = clauseTable[1:3419,]


correctionsFilePassiveAgents = read_csv("dec22-table-withintersentence-modified-3000clauses.csv")
correctionsFilePassiveAgents = correctionsFilePassiveAgents %>% filter(!is.na(AgentDesc))
correctionsFilePassiveAgents = correctionsFilePassiveAgents %>% select(c("ClauseID","Doc","AgentDesc","AgentDef","AgentNum","AgentPers",	"AgentAnim"))

clauseTable = clauseTable %>% left_join(correctionsFilePassiveAgents, by = c("ClauseID","Doc"), suffix = c("",".y"))

#Select only sentences that are potentially passivisable
clauseTablePassivisable = clauseTable %>% filter(PredType == "V" & (Voice == "Pass" | ObjHead != "/" | OblHead != "/"))
clauseTablePassivisable %>% filter(Voice != "Act" & OblCase != "by")
clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentSylCo = case_when(
  Voice == "Act" & SubjSylCo != "/" & !is.na(as.numeric(SubjSylCo)) ~ as.numeric(SubjSylCo),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblSylCo))  ~ as.numeric(OblSylCo),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$AgentSylCo == 9999] = mean(clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$AgentSylCo != 9999])
clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeSylCo = case_when(
  Voice == "Act" & OblSylCo != "/" & !is.na(as.numeric(OblSylCo)) ~ as.numeric(OblSylCo),
  Voice == "Act" & ObjSylCo != "/" & !is.na(as.numeric(ObjSylCo)) ~ as.numeric(ObjSylCo),
  Voice == "Pass" & !is.na(as.numeric(SubjSylCo))  ~ as.numeric(SubjSylCo),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$ThemeSylCo == 9999] = mean(clauseTablePassivisable$ThemeSylCo[clauseTablePassivisable$AgentSylCo != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeFreqAM = case_when(
  Voice == "Act" & OblFreq != "/" & !is.na(as.numeric(OblFreq)) ~ mean(as.numeric(splitConjString(OblFreq))),
  Voice == "Act" & ObjFreq != "/" & !is.na(as.numeric(ObjFreq)) ~ mean(as.numeric(splitConjString(ObjFreq))),
  Voice == "Pass" & !is.na(as.numeric(SubjFreq))  ~ mean(as.numeric(splitConjString(SubjFreq))),
  TRUE ~ 9999
))
clauseTablePassivisable$ThemeFreqAM[clauseTablePassivisable$ThemeFreqAM == 9999] = mean(clauseTablePassivisable$ThemeFreqAM[clauseTablePassivisable$ThemeFreqAM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentFreqAM = case_when(
  Voice == "Act" & SubjFreq != "/" & !is.na(as.numeric(SubjFreq)) ~ as.numeric(SubjFreq),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblFreq))  ~ as.numeric(OblFreq),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentFreqAM[clauseTablePassivisable$AgentFreqAM == 9999] = mean(clauseTablePassivisable$AgentFreqAM[clauseTablePassivisable$ThemeFreqAM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeFreqGM = case_when(
  Voice == "Act" & OblFreq != "/" & !is.na(as.numeric(OblFreq)) ~ gmean(as.numeric(splitConjString(OblFreq))),
  Voice == "Act" & ObjFreq != "/" & !is.na(as.numeric(ObjFreq)) ~ gmean(as.numeric(splitConjString(ObjFreq))),
  Voice == "Pass" & !is.na(as.numeric(SubjFreq))  ~ gmean(as.numeric(splitConjString(SubjFreq))),
  TRUE ~ 9999
))
clauseTablePassivisable$ThemeFreqGM[clauseTablePassivisable$ThemeFreqGM == 9999] = gmean(clauseTablePassivisable$ThemeFreqGM[clauseTablePassivisable$ThemeFreqGM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentFreqGM = case_when(
  Voice == "Act" & SubjFreq != "/" & !is.na(as.numeric(SubjFreq)) ~ as.numeric(SubjFreq),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblFreq))  ~ as.numeric(OblFreq),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentFreqGM[clauseTablePassivisable$AgentFreqGM == 9999] = gmean(clauseTablePassivisable$AgentFreqGM[clauseTablePassivisable$ThemeFreqGM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentPersEgo = case_when(
  Voice == "Act" & SubjPers != "/" & !is.na(as.numeric(SubjPers)) ~ SubjPers == "1",
  Voice == "Pass" & OblPers != "/" & !is.na(as.numeric(OblPers)) ~ OblPers == "1",
  AgentPers != "/" & !is.na(as.numeric(AgentPers)) ~ AgentPers == "1",
  TRUE ~ FALSE),
  AgentPersSAP = case_when(
    Voice == "Act" & SubjPers != "/" & !is.na(as.numeric(SubjPers)) ~ SubjPers %in% c("1","2"),
    Voice == "Pass" & OblPers != "/" & !is.na(as.numeric(OblPers)) ~ OblPers %in% c("1","2"),
    AgentPers != "/" & !is.na(as.numeric(AgentPers)) ~ AgentPers %in% c("1","2"),
    TRUE ~ FALSE),
  ThemePersEgo = case_when(
    Voice == "Pass" & SubjPers != "/" & !is.na(as.numeric(SubjPers)) ~ (SubjPers) == "1",
    Voice == "Act" & ObjPers != "/" & !is.na(as.numeric(ObjPers)) ~ (ObjPers) == "1",
    Voice == "Act" & OblPers != "/" & !is.na(as.numeric(OblPers)) ~ (OblPers) == "1",
    #ThemePers != "/" & !is.na(as.numeric(ThemePers)) ~ ThemePers == "1",
    TRUE ~ FALSE),
    ThemePersSAP = case_when(
      Voice == "Pass" & SubjPers != "/" & !is.na(as.numeric(SubjPers)) ~ SubjPers %in% c("1","2"),
      Voice == "Act" & ObjPers != "/" & !is.na(as.numeric(OblPers)) ~ ObjPers %in% c("1","2"),
      Voice == "Act" & OblPers != "/" & !is.na(as.numeric(OblPers)) ~ OblPers %in% c("1","2"),
      #ThemePers != "/" & !is.na(as.numeric(ThemePers)) ~ ThemePers %in% c("1","2"),
      TRUE ~ FALSE)
  )



animacyLevels = c("human", "machine", "vehicle", "org", "animal", "time", "loc", "conc", "nonconc")
animacyFeatures = c("Concrete","Setting","AgentCollective","Moving","Displacable","Volitional")
animacyFeatureMatrix = matrix(c(1,0,0,1,1,1,#human
                              1,0,0,1,0,0,#machine
                              1,0,0,1,1,0,#vehicle
                              1,0,1,1,1,1,#org
                              1,0,1,1,1,1,#animal
                              0,1,0,0,0,0,#time
                              1,1,0,0,0,0,#loc
                              1,0,0,0,0,0,#conc
                              0,0,0,0,0,0)#nonconc
  ,nrow=9,byrow=T,dimnames = list(animacyLevels,animacyFeatures)
)

agentAnimColnames = paste("Agent",animacyFeatures,sep="")
agentAnimMatrix = matrix(0,nrow=nrow(clauseTablePassivisable),ncol=length(agentAnimColnames))
colnames(agentAnimMatrix) = agentAnimColnames
for(i in 1:nrow(agentAnimMatrix)){
  AgentAnim = character(1)
  if(clauseTablePassivisable[i,"Voice"] == "Act"){
    if(isNotNull(clauseTablePassivisable[i,"SubjAnim"])){
      AgentAnim = clauseTablePassivisable[i,"SubjAnim"]
    } else if(isNotNull(clauseTablePassivisable[i,"AgentAnim"])){
      AgentAnim = clauseTablePassivisable[i,"AgentAnim"]
    }
  } else {
    if(isNotNull(clauseTablePassivisable[i,"OblAnim"])){
      AgentAnim = clauseTablePassivisable[i,"OblAnim"]
    } else if(isNotNull(clauseTablePassivisable[i,"ObjAnim"])){
      AgentAnim = clauseTablePassivisable[i,"ObjAnim"]
    }
  }
  if(isNotNull(AgentAnim)){
    AgentAnims = splitConjString(as.character(AgentAnim))
    if(length(AgentAnims) == 1){
      print(paste("1",AgentAnims))
      agentAnimMatrix[i,] = animacyFeatureMatrix[AgentAnims,]
    } else {
      print(paste("M",AgentAnims))
      agentAnimMatrix[i,] = colMeans(animacyFeatureMatrix[AgentAnims,])
    }
  }
}

clauseTablePassivisable = cbind(clauseTablePassivisable, agentAnimMatrix)

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


pred_only_model_3000 = brm(Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP +  PredTense + PredFreq + PredSylCo + PrevVoice1 + PrevVoice2 + PrevVoice3 + PrevVoice4 + PrevVoice5, data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), chains = 1)

pred_coefs = coef(pred_only_model)
