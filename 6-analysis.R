

waicModel = list()
for(lambda_lasso in exp(seq(-10,10,2.5))){
  for(lambda_ridge in exp(seq(-10,10,2.5))){
        ridge_vars = stanvar(x = lambda_ridge, name = "lambda_ridge") + stanvar(scode = "target += - lambda_ridge * dot_self(b);", block = "model") + stanvar(x = lambda_lasso, name = "lambda_lasso") + stanvar(scode = "for(k in 1:K) target += - lambda_lasso * fabs(b);", block = "model")
        pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM  + AgentPersEgo + AgentPersSAP +  AgentHuman + AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue  + AgentPronominal + AgentProper + AgentWh + AgentDem  + AgentNumeral + AgentDet + ThemeSylCo + ThemeFreqAM  + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + ThemeHuman + ThemeConcrete + ThemeSetting + ThemeAgentCollective + ThemeDisplacable + ThemeVolitional + ThemeMoving + ThemeDefTrue + ThemePlurTrue  + ThemePronominal + ThemeProper +  AgentWh + AgentDem  + AgentNumeral + AgentDet +  PredAspect + PredTense + PredFreq + PredFin + PredSylCo + AgentPrevAppeared + ThemePrevAppeared + AgentPrevAppearedSamePos + ThemePrevAppearedSamePos"), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)
        modelName = paste("model","_",lambda_lasso,"/",lambda_ridge,sep="")
        waicModel[[modelName]] = waic(pred_only_model_3000)
  }
}



lambda_ridge = exp(-10)
ridge_vars = stanvar(x = lambda_ridge, name = "lambda_ridge") + stanvar(scode = "target += - lambda_ridge * dot_self(b);", block = "model")


pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredAspect + PredTense + PredFreq + PredFin + PredSylCo + ",findAllInteractions(c("AgentPersEgo","AgentPersSAP"),c("ThemePersEgo","ThemePersSAP"))), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)


stanplot(pred_only_model_3000)
waic(pred_only_model_3000)
loo(pred_only_model_3000)
kfold(pred_only_model_3000, K = 10)
pred_coefs = coef(pred_only_model_3000)

pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredTense + PredFreq + PredFin + PredSylCo + PrevVoice1 + PrevVoice2 + PrevVoice3 + PrevVoice4 + PrevVoice5",findAllInteractions(c("AgentPersEgo","AgentPersSAP"),c("ThemePersEgo","ThemePersSAP"))), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)


pred_only_model_3000_nointeract = brm("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredTense + PredFreq + PredSylCo + PrevVoice1 + PrevVoice2 + PrevVoice3 + PrevVoice4 + PrevVoice5", data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), chains = 1)


pred_only_model_3000 = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue + AgentPlurTrue + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue + ThemePlurTrue + PredTense + PredFreq + PredSylCo + PrevVoice1 + PrevVoice2 + PrevVoice3 + PrevVoice4 + PrevVoice5 + ",findAllInteractions(c("AgentPersEgo","AgentPersSAP"),c("ThemePersEgo","ThemePersSAP")),"+",findAllInteractions(c("AgentConcrete","AgentSetting","AgentAgentCollective","AgentDisplacable","AgentVolitional","AgentMoving"),c("ThemeConcrete","ThemeSetting","ThemeAgentCollective","ThemeDisplacable","ThemeVolitional","ThemeMoving"))), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)


pred_only_model_3000_modified = brm(paste("Voice ~ (1 | PredLemma) + (1 | Doc) + AgentSylCo + AgentFreqAM + AgentFreqGM + AgentPersEgo + AgentPersSAP +  AgentConcrete + AgentSetting + AgentAgentCollective + AgentDisplacable + AgentVolitional + AgentMoving + AgentDefTrue  + ThemeSylCo + ThemeFreqAM + ThemeFreqGM + ThemePersEgo + ThemePersSAP + ThemeDefTrue  + PredTense + PredFreq  + PrevVoice1 + PrevVoice2 + PrevVoice3 + PrevVoice4 + PrevVoice5 + ",findAllInteractions(c("AgentPersEgo","AgentPersSAP"),c("ThemePersEgo","ThemePersSAP"))), data = clauseTablePassivisable, family = bernoulli(link = "logit"), init_r = 20, prior = set_prior("lasso(1)"), cores = getOption("mc.cores", 4L), stanvars = ridge_vars, chains = 1)
