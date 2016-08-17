# Open Libaries
library(ggplot2)
library(dplyr)
library(ROCR)
library(stringr)
library(glmnet)
library(randomForest)
library(gbm)

if (!require("devtools")) install.packages("devtools")
devtools::install_github("trestletech/shinyAce")
devtools::install_github("swarm-lab/editR")
library(editR)

# Open Card Information Files
cardinfo_collect <- read.csv("cardinfo_collect.csv", header = TRUE, na.string="")
cardinfo <- cardinfo_collect %>% select(name,type,rarity,playerClass,cost,attack,health,race,text,spellDamage,overload)
cardinfo <- cardinfo

levels(cardinfo$text) <- append(levels(cardinfo$text), "")
for(idx in which(is.na(cardinfo$text)))
  cardinfo[idx,"text"] <- ""

# Card Information Pre-processing
cardinfo <- cardinfo %>% filter( type!="HERO" ) %>%
  mutate( hasTaunt        = as.numeric( str_detect( text,"Taunt") ) ) %>%
  mutate( hasCharge       = as.numeric( str_detect( text,"Charge") ) ) %>%
  mutate( hasDeathrattle  = as.numeric( str_detect( text,"Deathrattle") ) ) %>%
  mutate( hasBattlecry    = as.numeric( str_detect( text,"Battlecry") ) ) %>%
  mutate( hasDivineShield = as.numeric( str_detect( text,"Divine Shield") ) ) %>%
  mutate( hasStealth      = as.numeric( str_detect( text,"Stealth") ) ) %>%
  mutate( hasWindfury     = as.numeric( str_detect( text,"Windfury") ) )%>%
  mutate( isSecret        = as.numeric( ( type=="SPELL" & str_detect(text,"Secret") ) ) )%>%
  mutate( hasCombo        = as.numeric( str_detect( text,"Combo") ) ) %>%
  mutate( hasDestroy      = as.numeric( str_detect( text,"Destroy") ) ) %>%
  mutate( hasInspire      = as.numeric( str_detect( text,"Inspire") ) ) %>%
  mutate( hasFreeze       = as.numeric( str_detect( text,"Freeze") ) ) %>%
  mutate( hasDraw         = as.numeric( str_detect( text,"Draw") ) ) %>%
  mutate( hasDiscard      = as.numeric( str_detect( text,"Discard") ) ) %>%
  mutate( hasChoose       = as.numeric( str_detect( text,"Choose One") ) ) %>%
  mutate( relSummon       = as.numeric( str_detect( text,"Summon ") ) ) %>%
  mutate( affectToAll     = as.numeric( str_detect( text,"to all ") ) ) %>%
  mutate( restoreAmount   = ifelse( str_detect( text,"Restore" ), as.integer(str_extract(text, "[0-9]+")) , 0 ) ) %>%
  mutate( dealAmount      = ifelse( str_detect( text,"Deal" ),    as.integer(str_extract(text, "[0-9]+")) , 0 ) ) %>%
  transform( name = as.character(name) ) %>% select(-text) %>% arrange( as.character(name) )

# NA Processing
levels(cardinfo$playerClass) <- append(levels(cardinfo$playerClass), "COMMON")
for(idx in which(is.na(cardinfo$playerClass)))
  cardinfo[idx,"playerClass"] <- "COMMON"

levels(cardinfo$race) <- append(levels(cardinfo$race), "NORMAL")
for(idx in which(is.na(cardinfo$race)))
  cardinfo[idx,"race"] <- "NORMAL"

cardinfo[is.na(cardinfo)] <- 0

# Open Arena Information Files
arenainfo <- read.csv( "arena_info.csv",    header= TRUE )
druid     <- read.csv( "arena_druid.csv",   header= TRUE )
hunter    <- read.csv( "arena_hunter.csv",  header= TRUE )
mage      <- read.csv( "arena_mage.csv",    header= TRUE )
paladin   <- read.csv( "arena_paladin.csv", header= TRUE )
priest    <- read.csv( "arena_priest.csv",  header= TRUE )
rogue     <- read.csv( "arena_rogue.csv",   header= TRUE )
shaman    <- read.csv( "arena_shaman.csv",  header= TRUE )
warlock   <- read.csv( "arena_warlock.csv", header= TRUE )
warrior   <- read.csv( "arena_warrior.csv", header= TRUE )

overall <- data.frame( name=character(), cnt=integer(), freqeuncy=numeric(), 
                       allfrequency=numeric(), diff=numeric(), className=character() )

druid   <- druid %>% mutate(className = "Druid") %>% arrange(desc(frequency))
hunter  <- hunter %>% mutate(className = "Hunter") %>% arrange(desc(frequency))
mage    <- mage %>% mutate(className = "Mage") %>% arrange(desc(frequency))
paladin <- paladin %>% mutate(className = "Paladin") %>% arrange(desc(frequency))
priest  <- priest %>% mutate(className = "Priest") %>% arrange(desc(frequency))
rogue   <- rogue %>% mutate(className = "Rogue") %>% arrange(desc(frequency))
shaman  <- shaman %>% mutate(className = "Shaman") %>% arrange(desc(frequency))
warlock <- warlock %>% mutate(className = "Warlock") %>% arrange(desc(frequency))
warrior <- warrior %>% mutate(className = "Warrior") %>% arrange(desc(frequency))

classlist = list(druid,hunter,mage,paladin,priest,rogue,shaman,warlock,warrior)

for( cl in classlist ){
  #cl <- cl %>% mutate(className = deparse(substitute(cl)) )
  overall <- rbind(overall, cl)
}

overall <- overall %>% group_by(name) %>% summarise(sumcount = sum(cnt) ) %>%
  arrange(as.character(name) )


# Plot Overall Top 15
overall_top10 <- data.frame( name=character(), cnt=integer(), freqeuncy=numeric(), 
                             allfrequency=numeric(), diff=numeric(), className=character() )

classlist_top10 <- list(druid %>% head(15),hunter %>% head(15),mage %>% head(15),paladin %>% head(15),
                        priest %>% head(15),rogue %>% head(15),shaman %>% head(15), warlock %>% head(15),
                        warrior %>% head(15))

for( cl in classlist_top10 ){
  overall_top10 <- rbind(overall_top10, cl)
}

overall_top10 %>% group_by(className) %>% 
  ggplot(aes(x=reorder(name,frequency),y=frequency, fill=className)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip() + ggtitle("Most Top 15 Pick Frequency Group by Class")


# Plot Top 50 Pick on the Arena
result_top50 <- left_join(cardinfo, overall)
result_top50 %>% arrange(desc(sumcount)) %>% head(50) %>% 
  ggplot(aes(x=reorder(name,sumcount),sumcount)) + geom_bar(stat="identity") + coord_flip() +
  ggtitle("Most Top 50 Pick on the Arena")

# Make Model for Mage Class
result_mage <- left_join(cardinfo, mage[1:4])
result_mage[is.na(result_mage)] <- 0
result_mage <- result_mage %>% filter( playerClass=='MAGE' | playerClass=='COMMON' )

# Rarity Normalize
leg  <- as.integer(sum( result_mage %>% filter(rarity=='LEGENDARY') %>% select(cnt) ))
epic <- as.integer(sum( result_mage %>% filter(rarity=='EPIC') %>% select(cnt) ))
rare <- as.integer(sum( result_mage %>% filter(rarity=='RARE') %>% select(cnt) ))
com  <- as.integer(sum( result_mage %>% filter(rarity=='COMMON' | rarity=='FREE') %>% select(cnt) ))

sumr <- as.integer( sum(c(leg,epic,rare,com)) )
legr <- leg/sumr
epicr <- epic/sumr
rarer <- rare/sumr
comr <- com/sumr

result_mage <- result_mage %>% 
  mutate( normfreq = frequency / ifelse( rarity=='LEGENDARY', legr,
                                  ifelse( rarity=='EPIC', epicr, 
                                    ifelse( rarity=='RARE', rarer, comr) ) ) )

# Set target attribute (freqover)
freqthres <- as.numeric(result_mage %>% summarize(mean(normfreq)))
result_mage <- result_mage %>% 
  mutate( freqover = ifelse(normfreq >= freqthres, "Y", "N") )
result_mage$freqover <- as.factor(result_mage$freqover)

# Sampling Set
set.seed(1601)
n <- nrow(result_mage)

idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx = sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)

length(training_idx)
length(validate_idx)
length(test_idx)

train_set    <- result_mage[training_idx,]
validate_set <- result_mage[validate_idx,]
test_set     <- result_mage[test_idx,]

levels(train_set$name) <- levels(result_mage$name)
levels(validate_set$name) <- levels(result_mage$name)
levels(test_set$name) <- levels(result_mage$name)

train_set %>%
  ggplot(aes(health, fill=freqover)) + geom_density(alpha=.5)

train_set %>%
  ggplot(aes(cost, fill=freqover)) + geom_density(alpha=.5)

# Generalized Linear Model
glm_full = glm(freqover ~ +rarity +type +race +health +attack +cost +dealAmount +restoreAmount  
               +spellDamage +overload +hasTaunt +hasCharge +hasBattlecry +hasDivineShield
               +hasDeathrattle +hasStealth +hasWindfury +isSecret +hasCombo +hasDestroy +hasInspire
               +hasFreeze +hasDraw +hasDiscard +hasChoose +relSummon +affectToAll, data=train_set, family=binomial)
summary(train_set)

y_obs = ifelse(validate_set$normfreq >= freqthres, "Y", "N")
y_obs <- as.factor(y_obs)
yhat_lm = predict(glm_full, newdata=validate_set, type='response')

pred_lm = prediction(yhat_lm, y_obs)
perf_lm = performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]

# Glmnet
xx = model.matrix(freqover ~ +rarity +type +race +health +attack +cost +dealAmount +restoreAmount  
                  +spellDamage +overload +hasTaunt +hasCharge +hasBattlecry +hasDivineShield
                  +hasDeathrattle +hasStealth +hasWindfury +isSecret +hasCombo +hasDestroy +hasInspire
                  +hasFreeze +hasDraw +hasDiscard +hasChoose +relSummon +affectToAll -1, result_mage)
x = xx[training_idx, ]
y = ifelse(train_set$freqover  == "Y", 1, 0)
dim(x)

( hs_glmnet_fit = glmnet(x, y) )
plot(hs_glmnet_fit)

hs_cvfit = cv.glmnet(x, y, family = "binomial")
plot(hs_cvfit)
log(hs_cvfit$lambda.min)
log(hs_cvfit$lambda.1se)

coef(hs_cvfit, s=hs_cvfit$lambda.min)
coef(hs_cvfit, s="lambda.1se")
length(which(coef(hs_cvfit, s="lambda.min")>0))

predict(hs_cvfit, s="lambda.min", newx = x[1:5,], type='response')

y_obs = ifelse(validate_set$freqover == "Y", 1, 0)
yhat_glmnet = predict(hs_cvfit, s = "lambda.min", newx=xx[validate_idx,], type='response')
# yhat_glmnet = predict(ad_cvfit, s = "lambda.min", newx=xx[validate_idx,], type='response')
yhat_glmnet = yhat_glmnet[,1] # change to a vectro from [n*1] matrix

pred_glmnet = prediction(yhat_glmnet, y_obs)
perf_glmnet = performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
plot(perf_glmnet, col='blue')
abline(0,1)
performance(pred_glmnet, "auc")@y.values[[1]]

# Random Forest
set.seed(1607)
ad_rf = randomForest(freqover ~ +rarity +type +race +health +attack +cost +dealAmount +restoreAmount  
                     +spellDamage +overload +hasTaunt +hasCharge +hasBattlecry +hasDivineShield
                     +hasDeathrattle +hasStealth +hasWindfury +isSecret +hasCombo +hasDestroy +hasInspire
                     +hasFreeze +hasDraw +hasDiscard +hasChoose +relSummon +affectToAll, train_set)
ad_rf
 plot(ad_rf)

varImpPlot(ad_rf, main="Variable Importance")
test_set[1:5,"name"]
predict(ad_rf, newdata = test_set[1:5,],type="prob")

yhat_rf = predict(ad_rf, newdata = validate_set, type='prob')[,'Y']
pred_rf = prediction(yhat_rf, y_obs)
perf_rf = performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_rf, col='red')
abline(0,1)
performance(pred_rf, "auc")@y.values[[1]]

# GBM Boosting
set.seed(1607)
result_gbm = train_set %>% mutate(freqover=ifelse(freqover == "Y", 1, 0))
  
re_gbm = gbm(freqover ~ +rarity +type +race +health +attack +cost +dealAmount +restoreAmount  
             +spellDamage +overload +hasTaunt +hasCharge +hasBattlecry +hasDivineShield
             +hasDeathrattle +hasStealth +hasWindfury +isSecret +hasCombo +hasDestroy +hasInspire
             +hasFreeze +hasDraw +hasDiscard +hasChoose +relSummon +affectToAll, data=result_gbm,
             distribution = "bernoulli",
             n.trees=50000, cv.folds=3, verbose = TRUE)
(best_iter = gbm.perf(re_gbm, method="cv"))

re_gbm2 = gbm.more(re_gbm, n.new.trees = 10000)
(best_iter = gbm.perf(re_gbm2, method="cv"))

yhat_gbm = predict(re_gbm, n.trees=best_iter, newdata=validate_set, type='response')
pred_gbm = prediction(yhat_gbm, y_obs)
perf_gbm = performance(pred_gbm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
plot(perf_gbm, add=TRUE, col='cyan')
abline(0,1)

legend('bottomright', inset=.1,
         legend = c("GLM", "glmnet", "RF", "GBM"),
         col=c('black', 'blue', 'red', 'cyan'), lty=1, lwd=2)
performance(pred_gbm, "auc")@y.values[[1]]

# Predict by GBM Model
predict(re_gbm, n.trees=best_iter, newdata=result_gbm %>% filter(name=='Fireball'), type='response')
