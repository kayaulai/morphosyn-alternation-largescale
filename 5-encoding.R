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

findAllInteractions = function(factors, factors2 = NA){
  if(all(is.na(factors2))){
    factors = factors2
    strings = sapply(factors, function(factor) return(sapply(factors2, function(factor2) return(paste(factor, " * ", factor2)))))
    strings = as.vector(strings)[-seq(1,length(factors)^2,length(factors)+1)]
  } else{
    strings = sapply(factors, function(factor) return(sapply(factors2, function(factor2) return(paste(factor, " * ", factor2)))))
    strings = as.vector(strings)
  }
  return(paste(strings,collapse=" + "))
}

isNotNull = function(string){
  string = as.character(string)
  return(!any(splitConjString(string) == "/") & !any(splitConjString(string) == "")
         & string != "" & !is.na(string)
         & !any(splitConjString(string) == "NMA!"))
}

#Read CSV
clauseTable = read_csv("dec26-table-withintersentence.csv")
clauseTable$PredFreq = parse_double(clauseTable$PredFreq)
clauseTable$PredSylCo = parse_double(clauseTable$PredSylCo)
clauseTable$Voice = parse_factor(clauseTable$Voice, levels = c("Act","Pass"))
clauseTable = clauseTable[1:3419,]


correctionsFilePassiveAgents = read_csv("dec22-table-withintersentence-modified-3000clauses.csv")
correctionsFilePassiveAgents = correctionsFilePassiveAgents %>% filter(!is.na(AgentDesc))
correctionsFilePassiveAgents = correctionsFilePassiveAgents %>% select(c("ClauseID","Doc","AgentDesc","AgentDef","AgentNum","AgentPers",	"AgentAnim"))

clauseTable = clauseTable %>% left_join(correctionsFilePassiveAgents, by = c("ClauseID","Doc"), suffix = c("",".1"))

correctionsFileActiveAgents = read_csv("dec25table-corrtable-implicitsubjs-first3000.csv")
correctionsFileActiveAgents = correctionsFileActiveAgents %>% filter(!is.na(AgentDesc))
correctionsFileActiveAgents = correctionsFileActiveAgents %>% select(c("ClauseID","Doc","AgentDesc","AgentDef","AgentNum","AgentPers",	"AgentAnim"))


sentCorefTable = read_csv("dec27-sentence-coref-table.csv")
docs = unique(sentCorefTable$Doc)

clauseTable = clauseTable %>% left_join(correctionsFileActiveAgents, by = c("ClauseID","Doc"), suffix = c("",".2"))
clauseTable = clauseTable %>% mutate(AgentDesc = coalesce(AgentDesc,AgentDesc.2),
                                     AgentDef = coalesce(AgentDef,AgentDef.2),
                                     AgentNum = coalesce(AgentNum,AgentNum.2),
                                     AgentPers = coalesce(AgentPers,AgentPers.2),
                                     AgentAnim = coalesce(AgentAnim,AgentAnim.2)) %>%
                              select(-c(AgentDesc.2,AgentDef.2,AgentNum.2,AgentPers.2,AgentAnim.2))


correctionsFilePassivisability = read_csv("dec26table-first3000-v2-passivisability.csv")
correctionsFilePassivisability = correctionsFilePassivisability %>% filter(!is.na(NonAlternableCorr))
clauseTable = clauseTable %>% left_join(correctionsFilePassivisability, by = c("ClauseID","Doc"), suffix = c("",".2")) %>%
  mutate(NonAlternable = case_when(NonAlternable != "/" ~ NonAlternable, TRUE ~ as.character(NA))) %>% 
  mutate(NonAlternable = coalesce(NonAlternable,as.character(NonAlternableCorr))) %>%
  select(-c(NonAlternableCorr))


#Select only sentences that are potentially passivisable
clauseTablePassivisable = clauseTable %>% filter(PredType == "V" & (Voice == "Pass" | ObjHead != "/" | OblHead != "/") & is.na(NonAlternable))
clauseTablePassivisable %>% filter(Voice != "Act" & OblCase != "by")
clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentSylCo = case_when(
  Voice == "Act" & isNotNull(SubjSylCo) ~ as.numeric(SubjSylCo),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblSylCo))  ~ as.numeric(OblSylCo),
  TRUE ~ 9999
))

#Syllable count
clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$AgentSylCo == 9999] = median(clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$AgentSylCo != 9999])
clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeSylCo = case_when(
  Voice == "Act" & isNotNull(OblSylCo) ~ as.numeric(OblSylCo),
  Voice == "Act" & isNotNull(ObjSylCo) ~ as.numeric(ObjSylCo),
  Voice == "Pass" & !is.na(as.numeric(SubjSylCo))  ~ as.numeric(SubjSylCo),
  TRUE ~ 9999
))

clauseTablePassivisable$AgentSylCo[clauseTablePassivisable$ThemeSylCo == 9999] = median(clauseTablePassivisable$ThemeSylCo[clauseTablePassivisable$AgentSylCo != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeFreqAM = case_when(
  Voice == "Act" & isNotNull(OblFreq) ~ mean(as.numeric(splitConjString(OblFreq))),
  Voice == "Act" & isNotNull(ObjFreq) ~ mean(as.numeric(splitConjString(ObjFreq))),
  Voice == "Pass" & !is.na(as.numeric(SubjFreq))  ~ mean(as.numeric(splitConjString(SubjFreq))),
  TRUE ~ 9999
))

#Frequency
clauseTablePassivisable$ThemeFreqAM[clauseTablePassivisable$ThemeFreqAM == 9999] = median(clauseTablePassivisable$ThemeFreqAM[clauseTablePassivisable$ThemeFreqAM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentFreqAM = case_when(
  Voice == "Act" & isNotNull(SubjFreq) ~ as.numeric(SubjFreq),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblFreq))  ~ as.numeric(OblFreq),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentFreqAM[clauseTablePassivisable$AgentFreqAM == 9999] = median(clauseTablePassivisable$AgentFreqAM[clauseTablePassivisable$ThemeFreqAM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(ThemeFreqGM = case_when(
  Voice == "Act" & isNotNull(OblFreq) ~ gmean(as.numeric(splitConjString(OblFreq))),
  Voice == "Act" & isNotNull(ObjFreq) ~ gmean(as.numeric(splitConjString(ObjFreq))),
  Voice == "Pass" & !is.na(as.numeric(SubjFreq))  ~ gmean(as.numeric(splitConjString(SubjFreq))),
  TRUE ~ 9999
))
clauseTablePassivisable$ThemeFreqGM[clauseTablePassivisable$ThemeFreqGM == 9999] = median(clauseTablePassivisable$ThemeFreqGM[clauseTablePassivisable$ThemeFreqGM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentFreqGM = case_when(
  Voice == "Act" & isNotNull(SubjFreq) ~ as.numeric(SubjFreq),
  Voice == "Pass" & OblCase == "by" & !is.na(as.numeric(OblFreq))  ~ as.numeric(OblFreq),
  TRUE ~ 9999
))
clauseTablePassivisable$AgentFreqGM[clauseTablePassivisable$AgentFreqGM == 9999] = median(clauseTablePassivisable$AgentFreqGM[clauseTablePassivisable$ThemeFreqGM != 9999])

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentPersEgo = case_when(
  Voice == "Act" & isNotNull(SubjPers) ~ SubjPers == "1",
  Voice == "Pass" & isNotNull(OblPers) ~ OblPers == "1",
  isNotNull(AgentPers) ~ AgentPers == "1",
  TRUE ~ FALSE),
  AgentPersSAP = case_when(
    Voice == "Act" & isNotNull(SubjPers) ~ SubjPers %in% c("1","2"),
    Voice == "Pass" & isNotNull(OblPers) ~ OblPers %in% c("1","2"),
    isNotNull(AgentPers) ~ AgentPers %in% c("1","2"),
    TRUE ~ FALSE),
  ThemePersEgo = case_when(
    Voice == "Pass" & isNotNull(SubjPers) ~ (SubjPers) == "1",
    Voice == "Act" & isNotNull(ObjPers) ~ (ObjPers) == "1",
    Voice == "Act" & isNotNull(OblPers) ~ (OblPers) == "1",
    #isNotNull(ThemePers) ~ ThemePers == "1",
    TRUE ~ FALSE),
    ThemePersSAP = case_when(
      Voice == "Pass" & isNotNull(SubjPers) ~ SubjPers %in% c("1","2"),
      Voice == "Act" & isNotNull(ObjPers) ~ ObjPers %in% c("1","2"),
      Voice == "Act" & isNotNull(OblPers) ~ OblPers %in% c("1","2"),
      #isNotNull(ThemePers) ~ ThemePers %in% c("1","2"),
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
    } else if(isNotNull(clauseTablePassivisable[i,"AgentAnim"])){
      AgentAnim = clauseTablePassivisable[i,"AgentAnim"]
    }
  }
  
  if(isNotNull(AgentAnim)){
    AgentAnims = splitConjString(as.character(AgentAnim))
    if(length(AgentAnims) == 1){
      #print(paste("1",AgentAnims))
      agentAnimMatrix[i,] = animacyFeatureMatrix[AgentAnims,]
    } else {
      #print(paste("M",AgentAnims))
      agentAnimMatrix[i,] = colMeans(animacyFeatureMatrix[AgentAnims,])
    }
  }
}


clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentDefTrue = case_when(
  Voice == "Act" & isNotNull(SubjDef) ~ SubjDef == "def",
  Voice == "Pass" & isNotNull(OblDef) ~ OblDef == "def",
  AgentDef != "/" & isNotNull(AgentDef) ~ AgentDef == "def",
  TRUE ~ FALSE),
  ThemeDefTrue = case_when(
    Voice == "Pass" & isNotNull(SubjDef) ~ (SubjDef) == "def",
    Voice == "Act" & isNotNull(ObjDef) ~ (ObjDef) == "def",
    Voice == "Act" & isNotNull(OblDef) ~ (OblDef) == "def",
    #isNotNull(ThemeDef) ~ ThemeDef == "def",
    TRUE ~ FALSE)
)


clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentPlurTrue = case_when(
  Voice == "Act" & isNotNull(SubjNum) ~ SubjNum == "Plur",
  Voice == "Pass" & isNotNull(OblNum) ~ OblNum == "Plur",
  isNotNull(AgentNum) ~ AgentNum == "Plur",
  TRUE ~ FALSE),
  ThemePlurTrue = case_when(
    Voice == "Pass" & isNotNull(SubjNum) ~ (SubjNum) == "Plur",
    Voice == "Act" & isNotNull(ObjNum) ~ (ObjNum) == "Plur",
    Voice == "Act" & isNotNull(OblNum) ~ (OblNum) == "Plur",
    #isNotNull(ThemePlur) ~ ThemePlur == "Plur",
    TRUE ~ FALSE)
)

clauseTablePassivisable = cbind(clauseTablePassivisable, agentAnimMatrix)

#Prev mentioned?
for(doc in docs){
  currClauseTable =  clauseTablePassivisable %>% filter(Doc == doc)
  currSentTable = sentCorefTable %>% filter(Doc == doc)
  clauseTablePassivisable = clauseTablePassivisable %>% mutate(prevMentioned = case_when(
    (currSentTable %>% filter(SentID < parent.frame()[[SentID]]))$ ~ ,
    TRUE ~ "/"
  ))
}

write_csv(clauseTablePassivisable, "encoded-dec28.csv")

lambda_ridge = exp(-10)
ridge_vars = stanvar(x = lambda_ridge, name = "lambda_ridge") + stanvar(scode = "target += - lambda_ridge * dot_self(b);", block = "model")


pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredAspect + PredTense + PredFreq + PredFin + PredSylCo + ",findAllInteractions(c("AgentPersEgo","AgentPersSAP"),c("ThemePersEgo","ThemePersSAP"))), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)


stanplot(pred_only_model_3000)
waic(pred_only_model_3000)

