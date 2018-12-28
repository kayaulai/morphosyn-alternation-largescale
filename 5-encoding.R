library(dplyr)
library(brms)
library(readr)

setwd("G:\\§Úªº¶³ºÝµwºÐ\\corpus priming\\eng")

#Utility functions
splitConjString = function(conjString){
  return(strsplit(conjString,";;")[[1]])
}

splitChainString = function(chainString){
  splitElements = strsplit(chainString,",")[[1]]
  splitElements = as.integer(splitElements[splitElements != "/"])
  return(splitElements)
}

gmean = function(x){
  return(prod(x)^(1/length(x)))
}

`%+%` = function(a, b) paste(a, b, sep="")

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
    if(is.na(string)){
      isNotNull = F
    } else if(length(string) == 0){
      isNotNull = F
    } else if (string == ""){
      isNotNull = F
    } else {
      isNotNull = !any(splitConjString(string) == "/") & !any(splitConjString(string) == "") & !any(splitConjString(string) == "NMA!")
    }
    return(isNotNull)
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
correctionsFilePassivisability = correctionsFilePassivisability %>% filter(!is.na(NonAlternableCorr)) %>% select(ClauseID, Doc, NonAlternableCorr)
clauseTable = clauseTable %>% left_join(correctionsFilePassivisability, by = c("ClauseID","Doc"), suffix = c("",".2")) %>%
  mutate(NonAlternable = case_when(NonAlternable != "/" ~ NonAlternable, TRUE ~ as.character(NA))) %>% 
  mutate(NonAlternable = coalesce(NonAlternable,as.character(NonAlternableCorr))) %>%
  select(-c(NonAlternableCorr))

write_csv(clauseTable, "dec28-correctedtable.csv")

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

#Person
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


#Animacy
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

#TODO: Encode these
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
givennessTable = data.frame()

determineIfPrevAppearedSamePos = function(currSentID, currGRChains, allGRChains){
  prevChains = sapply(as.character((allGRChains[allGRChains$currFullClauseTable.SentID < currSentID,2])),splitChainString)
  currGRChainsAsVec = splitChainString(as.character(currGRChains))
  prevApps = sapply(prevChains, function(prevSentChains) return(length(intersect(prevSentChains, currGRChainsAsVec)) > 0))
  return(any(prevApps))
}

determineIfPrevAppeared = function(currSentID, currGRChains){
  prevApps = sapply(chains[1:currSentID], function(prevSentChains) return(length(intersect(prevSentChains, splitChainString(currGRChains))) > 0))
  return(any(prevApps))
}

for(doc in docs){
  print(paste("Processing",doc))
  
  currClauseTable =  clauseTablePassivisable %>% filter(Doc == doc) %>% select(c(Doc, SentID, ClauseID, SubjChain, ObjChain, OblChain, Obl2Chain, Obl3Chain))
  currFullClauseTable =  clauseTablePassivisable %>% filter(Doc == doc) %>% select(c(Doc, SentID, ClauseID, SubjChain, ObjChain, OblChain, Obl2Chain, Obl3Chain))
  
  currSentTable = sentCorefTable %>% filter(Doc == doc) %>% select(-c(OverallSentID))
  chains = sapply(currSentTable$Chains, splitChainString) #Note: element 1 of chains = SentID 0
  chainCol = chains[currClauseTable$SentID] #A 'column' containing chain data, if tibbles allowed vector-valued cells
  currClauseTable = currClauseTable %>% mutate(CombinedOblChain = paste(OblChain,Obl2Chain,Obl3Chain,sep=","))
  currFullClauseTable = currFullClauseTable %>% mutate(CombinedOblChain = paste(OblChain,Obl2Chain,Obl3Chain,sep=","))
  
  
  
  #TODO: Previously mentioned in the same sentence? 
  
  currClauseTable = currClauseTable %>% mutate(SubjPrevAppeared = mapply(determineIfPrevAppeared, SentID, SubjChain),
                                               ObjPrevAppeared = mapply(determineIfPrevAppeared, SentID, ObjChain),
                                               OblPrevAppeared = mapply(determineIfPrevAppeared, SentID, CombinedOblChain))
  
  
  currClauseTable = currClauseTable %>% mutate(
 SubjPrevAppearedSamePos = mapply(determineIfPrevAppearedSamePos, SentID, SubjChain, MoreArgs = list(data.frame(currFullClauseTable$SentID, currFullClauseTable$SubjChain))),
 ObjPrevAppearedSamePos = mapply(determineIfPrevAppearedSamePos, SentID, ObjChain, MoreArgs = list(data.frame(currFullClauseTable$SentID, currFullClauseTable$ObjChain))),
 OblPrevAppearedSamePos = mapply(determineIfPrevAppearedSamePos, SentID, OblChain, MoreArgs = list(data.frame(currFullClauseTable$SentID, currFullClauseTable$CombinedOblChain))))
  
  givennessTable = givennessTable %>% rbind(currClauseTable)
}

clauseTablePassivisable = clauseTablePassivisable %>% left_join(givennessTable %>% select(-c(SentID, SubjChain, ObjChain, OblChain, Obl2Chain, Obl3Chain, CombinedOblChain)), by = c("ClauseID", "Doc"))

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentPrevAppeared = case_when(
  Voice == "Act" & isNotNull(SubjPrevAppeared) ~ as.integer(SubjPrevAppeared),
  Voice == "Pass" & isNotNull(OblPrevAppeared) ~ as.integer(OblPrevAppeared),
  TRUE ~ 0L),
  ThemePrevAppeared = case_when(
    Voice == "Pass" & isNotNull(SubjPrevAppeared) ~ as.integer(SubjPrevAppeared),
    Voice == "Act" & isNotNull(ObjPrevAppeared) ~ as.integer(ObjPrevAppeared),
    Voice == "Act" & isNotNull(OblPrevAppeared) ~ as.integer(OblPrevAppeared),
    TRUE ~ 0L)
)

clauseTablePassivisable = clauseTablePassivisable %>% mutate(AgentPrevAppearedSamePos = case_when(
  Voice == "Act" & isNotNull(SubjPrevAppearedSamePos) ~ as.integer(SubjPrevAppearedSamePos),
  Voice == "Pass" & isNotNull(OblPrevAppearedSamePos) ~ as.integer(OblPrevAppearedSamePos),
  TRUE ~ 0L),
  ThemePrevAppearedSamePos = case_when(
    Voice == "Pass" & isNotNull(SubjPrevAppearedSamePos) ~ as.integer(SubjPrevAppearedSamePos),
    Voice == "Act" & isNotNull(ObjPrevAppearedSamePos) ~ as.integer(ObjPrevAppearedSamePos),
    Voice == "Act" & isNotNull(OblPrevAppearedSamePos) ~ as.integer(OblPrevAppearedSamePos),
    TRUE ~ 0L)
)

synTypeLevels = c("full", "proper", "wh", "det", "dem", "poss","pron", "num", "there")
synTypeFeatures = c("Pronominal","Proper","Wh","Dem","Poss","Numeral","Det")
synTypeFeatureMatrix = matrix(c(0,0,0,0,0,0,0,#full
                                0,1,0,0,0,0,0,#proper
                                1,0,1,0,0,0,1,#wh
                                1,0,0,0,0,0,1,#det
                                1,0,0,1,0,0,1,#dem
                                1,0,0,0,1,0,0,#poss
                                1,0,0,0,0,0,0,#pron
                                1,0,0,0,0,1,1,#num
                                1,0,0,1,0,1,1)#'there' category (to be deprecated)
                              ,nrow=9,byrow=T,dimnames = list(synTypeLevels,synTypeFeatures))

agentSynTypeColnames = paste("Agent",synTypeFeatures,sep="")
agentSynTypeMatrix = matrix(0,nrow=nrow(clauseTablePassivisable),ncol=length(agentSynTypeColnames))
colnames(agentSynTypeMatrix) = agentSynTypeColnames
for(i in 1:nrow(agentSynTypeMatrix)){
  AgentSynType = character(1)
  if(clauseTablePassivisable[i,"Voice"] == "Act"){
    if(isNotNull(clauseTablePassivisable[i,"SubjSynType"])){
      AgentSynType = clauseTablePassivisable[i,"SubjSynType"]
    }
  } else {
    if(isNotNull(clauseTablePassivisable[i,"OblSynType"])){
      AgentSynType = clauseTablePassivisable[i,"OblSynType"]
    } else if(isNotNull(clauseTablePassivisable[i,"ObjSynType"])){
      AgentSynType = clauseTablePassivisable[i,"ObjSynType"]
    }
  }
  
  if(isNotNull(AgentSynType)){
    print(AgentSynType)
    AgentSynTypes = splitConjString(as.character(AgentSynType))
    if(length(AgentSynTypes) == 1){
      #print(paste("1",AgentSynTypes))
      agentSynTypeMatrix[i,] = synTypeFeatureMatrix[AgentSynTypes,]
    } else {
      #print(paste("M",AgentSynTypes))
      agentSynTypeMatrix[i,] = colMeans(synTypeFeatureMatrix[AgentSynTypes,])
    }
  }
  
}

clauseTablePassivisable = clauseTablePassivisable %>% cbind(agentSynTypeMatrix)


write_csv(clauseTablePassivisable, "encoded-dec28.csv")

#lambda_ridge = exp(-10)
lambda_ridge = 0
ridge_vars = stanvar(x = lambda_ridge, name = "lambda_ridge") + stanvar(scode = "target += - lambda_ridge * dot_self(b);", block = "model")

pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredAspect + PredTense + PredFreq + PredFin + PredSylCo + AgentPrevAppeared + ThemePrevAppeared + AgentPrevAppearedSamePos + ThemePrevAppearedSamePos + AgentPronominal + AgentProper + AgentWh + AgentDem  + AgentNumeral + AgentDet"), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)




stanplot(pred_only_model_3000, c("AgentSylCo", "AgentFreqAM", "AgentFreqGM", "AgentPersEgo", "AgentPersSAP", " AgentConcrete", "AgentSetting", "AgentAgentCollective", "AgentDisplacable", "AgentVolitional", "AgentMoving", "AgentDefTrue", "AgentPlurTrue", "AgentPronominal", "AgentProper", "AgentWh", "AgentDem",  "AgentNumeral", "AgentDet", "ThemeSylCo", "ThemeFreqAM", "ThemeFreqGM", "ThemePersEgo", "ThemePersSAP", "ThemeDefTrue", "ThemePlurTrue", "PredAspect", "PredTense", "PredFreq", "PredFin", "PredSylCo", "AgentPrevAppeared", "ThemePrevAppeared", "AgentPrevAppearedSamePos", "ThemePrevAppearedSamePos"))
waic(pred_only_model_3000)

