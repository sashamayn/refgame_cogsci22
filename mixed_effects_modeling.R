###
# Glmer models for the pilot and the main experiment 
# (Table 2 in the paper)
###
library('ggplot2')
library('lme4')

df <- read.csv('main_task_data.csv')
indiv_diffs <- read.csv('averages_with_indiv_measures.csv')

df <- subset(df, item_fewer_cats != 'filler unambiguous' & answer_which != 'distractor')
df$item_fewer_cats <- factor(df$item_fewer_cats, levels=c("target simple","target complex", "filler ambiguous"))

df$aq <- indiv_diffs$asq_rescaled[match(df$participant_id,indiv_diffs$participant_id)]
df$crt <- indiv_diffs$crt_rescaled[match(df$participant_id,indiv_diffs$participant_id)]
df$iq <- indiv_diffs$iq_rescaled[match(df$participant_id,indiv_diffs$participant_id)]
df$ospan <- indiv_diffs$ospan_rescaled[match(df$participant_id,indiv_diffs$participant_id)]

meantrial <- round(mean(df$trialid))
df$trialid_centered <- df$trialid - meantrial
df$msgtype <- ifelse(df$message %in% c('rh','bh'), 'accessory','species')
df$targetpos <- as.factor(df$targetpos)
colnames(df)[14] <- 'condition'

condition_helmert_matrix <- matrix(c(2/3,-1/3,-1/3,0,1/2,-1/2),nrow=3)
targetpos_dummy_matrix <- matrix(c(0,1,0,0,0,1),nrow=3)

### PILOT MODEL WITHOUT INDIV DIFFS ### 
pilot <- subset(df, study == 'pilot')
m_pilot <- glmer(correct ~ condition + trialid_centered + trialid_centered * condition +
                  msgtype + targetpos + (1 | participant_id),data=pilot, 
                  contrasts = list(condition = condition_helmert_matrix,
                  targetpos = targetpos_dummy_matrix), family=binomial, glmerControl(optimizer="bobyqa"))
   

summary(m_pilot)

### MAIN EXPERIMENT MODEL WITHOUT INDIV DIFFS ###
main <- subset(df, study == 'main')
m_main <- glmer(correct ~ condition + trialid_centered + trialid_centered * condition +
                  msgtype + targetpos + (1 + trialid | participant_id),data=main, 
                  contrasts = list(condition = condition_helmert_matrix,
                                  targetpos = targetpos_dummy_matrix), family=binomial, glmerControl(optimizer="bobyqa"))

summary(m_main)

### PILOT MODEL WITH INDIV DIFFS ###
pilot2 <- subset(pilot, condition != 'filler ambiguous')
pilot2$condition <- factor(pilot2$condition,levels=c('target simple','target complex'))

m_pilot_w_ids <- glmer(correct ~ condition +
                   iq + crt + aq + ospan +
                   targetpos + (1+trialid_centered | participant_id),data=pilot2, 
                 contrasts = list(targetpos = targetpos_dummy_matrix), family=binomial, glmerControl(optimizer="bobyqa"))

summary(m_pilot_w_ids)

### MAIN EXPERIMENT MODEL WITH INDIV DIFFS ###

main2 <- subset(main, condition != 'filler ambiguous')
main2$condition <- factor(main2$condition,levels=c('target simple','target complex'))

m_main_w_ids <- glmer(correct ~ condition +
                         iq + crt + aq + ospan +
                         targetpos + (1+trialid_centered | participant_id),data=main2, 
                       contrasts = list(targetpos = targetpos_dummy_matrix), family=binomial, glmerControl(optimizer="bobyqa"))

summary(m_main_w_ids)
