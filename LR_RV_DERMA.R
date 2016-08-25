##### this model is build for the RV - DERMA franchise cosmetics

model_input <- read.table("D:/wizhan/01_data/INPUT_MODEL_20150630.csv", header=TRUE, sep = ',', quote = "\"")
sapply(model_input, class)
model_input[is.na(model_input)] <- 0
cols <- colnames(model_input)

## sample data
smp_size <- floor(0.70 * nrow(model_input))
set.seed(123)
train_ind <- sample(seq_len(nrow(model_input)), size = smp_size)
train <- model_input[train_ind, ]
test <- model_input[-train_ind, ]

### set lower and uppper boundary of the dataset
memory.limit(size=49148)
for (i in 2:143){
pct_min = min(quantile(train[,i], probs = 0.01),0)
pct_max = max(quantile(train[,i], probs = 0.99),0)

train[,i] = ifelse(train[,i] < pct_min, pct_min, train[,i])
train[,i] = ifelse(train[,i] > pct_max, pct_max, train[,i])
}

### univariate analysis
library(data.table)
cor_list <- cor(train[,2:143], train[,144])
cor_list <- as.data.frame(cor_list)
D_cor = setDT(cor_list, keep.rownames = TRUE)[]
colnames(D_cor) <- c('COLUMN','COR_COF')
write.table(as.matrix(D_cor), "D:/wizhan/01_data/D_cor.csv", sep="|", row.names=FALSE, col.names=TRUE)

list_var = c("P12_AMT_RV_DERMA","P12_SUM_NUM_TRIP","P12_SUM_AMT_TXN","P12_SUM_NUM_SKU","std_AMT_RV_DERMA","P12_AMT_MOISTURIZER","P12_AMT_SPCL_CARE_EYE","P12_AMT_TONER","P12_AMT_RV_OTHER","P12_AMT_MD","std_AMT_SPCL_CARE_EYE","std_RANGE_SKU","std_AMT_MOISTURIZER","std_AMT_TONER","std_AMT_RV_OTHER","std_NUM_SKU","std_AMT_TXN","P12_AMT_CLEANSER","P3_AMT_RV_DERMA","P12_AMT_DS","std_NUM_TRIP","P12_AMT_UV","std_AMT_UV","std_AMT_CLEANSER","P3_AMT_TXN","std_AMT_MD","P3_RANGE_SKU","P12_AMT_MU_FACE","P12_AMT_ESSENCE","P3_NUM_SKU","std_AMT_MU_FACE","std_AMT_ESSENCE","P3_AMT_MD","P3_NUM_TRIP","P12_AVG_RANGE_SKU","std_AMT_DS","P12_AMT_MU_EYE","MEM_TENURE","std_AMT_MU_EYE","std_AMT_MENS","P3_AMT_MOISTURIZER","std_AMT_RV_LASER","P12_AMT_MU_LIP","P12_AMT_RV_LASER","P3_AMT_RV_OTHER","P12_AMT_YC","std_AMT_HF","std_AMT_YC","P3_AMT_SPCL_CARE_EYE","std_AMT_MU_LIP","P3_AMT_TONER","P12_AMT_HF","P3_AMT_DS","P12_AMT_MENS","std_AMT_OTHER","std_AMT_WP_CLINICAL","P1_AMT_MD","P3_AMT_CLEANSER","P3_AMT_UV","P12_AMT_WP_CLINICAL","P12_AMT_OTHER","P1_AMT_TXN","P3_AMT_ESSENCE","P1_AMT_RV_DERMA","std_AMT_WP_NORMAL","P3_AMT_MU_FACE","P1_RANGE_SKU","P1_NUM_SKU","std_AMT_AP_BASIC","P12_AMT_WP_NORMAL","P3_AMT_RV_LASER","P3_AMT_MU_EYE","P3_AMT_YC","P12_AMT_AP_BASIC","P3_AMT_MU_LIP","P1_NUM_TRIP","P1_AMT_MOISTURIZER","P3_AMT_HF","P1_AMT_TONER","std_AMT_HAIR_COLOR","P1_AMT_SPCL_CARE_EYE","P1_AMT_RV_OTHER","P1_AMT_CLEANSER","P1_AMT_RV_LASER","std_AMT_AP_LUMI","P1_AMT_DS","P1_AMT_ESSENCE","P3_AMT_OTHER","P1_AMT_MU_FACE","P3_AMT_MENS","P3_AMT_WP_CLINICAL","std_AMT_WP_LASER","P12_AMT_WP_LASER","P1_AMT_UV","P12_AMT_AP_LUMI","P1_AMT_MU_EYE","P3_AMT_WP_NORMAL","P1_AMT_MENS","P1_AMT_MU_LIP","P1_AMT_HF","P1_AMT_YC","P12_AMT_MU_NAIL","P12_AMT_AP_OTHER","P1_AMT_WP_NORMAL","std_AMT_AP_OTHER","P1_AMT_WP_CLINICAL","P3_AMT_AP_BASIC","P12_AMT_HAIR_COLOR","P3_AMT_AP_LUMI","P1_AMT_AP_BASIC","P1_AMT_AP_LUMI","P12_AMT_WP_MIRACLE","P3_AMT_HAIR_COLOR","std_AMT_TMALL","P3_AMT_HAIR_CARE","P1_AMT_TMALL","P12_AMT_HAIR_CARE","P3_AMT_TMALL","P12_AMT_TMALL","AGE")

cor_var = cor(model_input[,list_var])
cor_var <- as.data.frame(cor_var)
D_cor_bi = setDT(cor_var, keep.rownames = TRUE)[]
write.table(cor_var, "D:/wizhan/01_data/D_cor_bi.csv", sep="|", row.names=FALSE, col.names=TRUE)

model_fit = glm(TAG_RV_DERMA ~ P12_AMT_RV_DERMA+P12_AMT_RV_OTHER+P12_AMT_CLEANSER+P3_AMT_RV_DERMA+P12_AMT_UV+P12_AMT_MU_FACE+P12_AMT_ESSENCE+P3_AMT_MD+P12_AVG_RANGE_SKU+P12_AMT_MU_EYE+MEM_TENURE+std_AMT_MU_EYE+std_AMT_MENS+std_AMT_RV_LASER+P12_AMT_MU_LIP+P12_AMT_RV_LASER+P3_AMT_RV_OTHER+P12_AMT_YC+std_AMT_HF+std_AMT_YC+P3_AMT_SPCL_CARE_EYE+std_AMT_MU_LIP+P12_AMT_HF+P12_AMT_MENS+std_AMT_OTHER+std_AMT_WP_CLINICAL+P1_AMT_MD+P3_AMT_CLEANSER+P3_AMT_UV+P12_AMT_WP_CLINICAL+P12_AMT_OTHER+P3_AMT_ESSENCE+P1_AMT_RV_DERMA+std_AMT_WP_NORMAL+P3_AMT_MU_FACE+std_AMT_AP_BASIC+P12_AMT_WP_NORMAL+P3_AMT_RV_LASER+P3_AMT_MU_EYE+P3_AMT_YC+P12_AMT_AP_BASIC+P3_AMT_MU_LIP+P1_NUM_TRIP+P1_AMT_MOISTURIZER+P3_AMT_HF+P1_AMT_TONER+std_AMT_HAIR_COLOR+P1_AMT_SPCL_CARE_EYE+P1_AMT_RV_OTHER+P1_AMT_CLEANSER+P1_AMT_RV_LASER+std_AMT_AP_LUMI+P1_AMT_DS+P1_AMT_ESSENCE+P3_AMT_OTHER+P1_AMT_MU_FACE+P3_AMT_MENS+P3_AMT_WP_CLINICAL+std_AMT_WP_LASER+P12_AMT_WP_LASER+P1_AMT_UV+P12_AMT_AP_LUMI+P1_AMT_MU_EYE+P3_AMT_WP_NORMAL+P1_AMT_MENS+P1_AMT_MU_LIP+P1_AMT_HF+P1_AMT_YC+P12_AMT_MU_NAIL+P12_AMT_AP_OTHER+P1_AMT_WP_NORMAL+std_AMT_AP_OTHER+P1_AMT_WP_CLINICAL+P3_AMT_AP_BASIC+P12_AMT_HAIR_COLOR+P3_AMT_AP_LUMI+P1_AMT_AP_BASIC+P1_AMT_AP_LUMI+P12_AMT_WP_MIRACLE+P3_AMT_HAIR_COLOR+P3_AMT_HAIR_CARE+P3_AMT_TMALL+AGE, data = train, family = binomial)

library(car)
list_vif = vif(model_fit)
d_vif <- as.data.frame(list_vif)
d_vif = setDT(d_vif, keep.rownames = TRUE)[]
write.table(d_vif, "D:/wizhan/01_data/D_vif.csv", sep="|", row.names=FALSE, col.names=TRUE)
#--- remove the hi

model_fit_vif = glm(TAG_RV_DERMA ~ P12_SUM_NUM_TRIP+P1_AMT_MD+std_AMT_WP_NORMAL+P12_AMT_HF+std_AMT_WP_CLINICAL+P12_AMT_YC+std_AMT_MENS+std_AMT_MU_EYE+std_AMT_RV_LASER+P1_NUM_TRIP+std_AMT_MU_LIP+P12_AMT_ESSENCE+std_AMT_YC+std_AMT_HF+P1_AMT_RV_DERMA+P1_AMT_CLEANSER+P3_AMT_CLEANSER+P3_AMT_ESSENCE+P12_AMT_CLEANSER+P1_AMT_MOISTURIZER+P3_AMT_RV_DERMA+P12_AMT_RV_OTHER+P1_AMT_SPCL_CARE_EYE+P1_AMT_TONER+P3_AMT_WP_CLINICAL+P3_AMT_SPCL_CARE_EYE+P3_AMT_RV_OTHER+P12_AMT_MU_FACE+P1_AMT_ESSENCE+P3_AMT_MD+P1_AMT_RV_OTHER+P3_AMT_UV+P3_AMT_MU_FACE+P3_AMT_HF+P12_AMT_UV+P12_AMT_RV_DERMA+P1_AMT_HF+P3_AMT_AP_LUMI+P1_AMT_RV_LASER+P3_AMT_MENS+P3_AMT_WP_NORMAL+P3_AMT_RV_LASER+P3_AMT_AP_BASIC+P3_AMT_MU_EYE+P1_AMT_WP_CLINICAL+P3_AMT_MU_LIP+P1_AMT_WP_NORMAL+P12_AVG_RANGE_SKU+P1_AMT_AP_BASIC+P3_AMT_YC+P1_AMT_MU_FACE+P1_AMT_AP_LUMI+P1_AMT_MENS+P1_AMT_MU_EYE+P1_AMT_UV+P1_AMT_YC+P1_AMT_MU_LIP+P3_AMT_OTHER+P3_AMT_HAIR_COLOR+P3_AMT_TMALL+MEM_TENURE+AGE+P12_AMT_MU_NAIL+P3_AMT_HAIR_CARE+P12_AMT_WP_MIRACLE+P12_AMT_MENS+std_AMT_AP_BASIC+P12_AMT_MU_LIP+P12_AMT_RV_LASER+std_AMT_OTHER, data = train, family = binomial)


### test train result
out_train = cbind(fitted.values(model_fit_vif), train[,144])
class(out_train)
df_out_train = data.frame(out_train)
colnames(df_out_train) <- c('prd', 'act')
df_out_train = transform(df_out_train,  prd.rank = ave(prd,  FUN = function(x) {rank(-x, ties.method = "first")}))
df_out_train$BIN = cut(df_out_train$prd.rank, breaks=100, labels=FALSE)
df_out_train_grp = ddply(df_out_train, "BIN", summarise, MEAN_TGT = mean(prd), CNT_TGT = sum(act), CNT_CST = length(BIN))
write.table(df_out_train_grp, "D:/wizhan/01_data/out_train.csv", sep="|", row.names=FALSE, col.names=TRUE)

# test model on the test data
out_test = predict(model_fit_vif, newdata=test, type="response")
out_test = cbind(out_test, test[,144])
df_out_test = data.frame(out_test)
colnames(df_out_test) <- c('prd', 'act')
df_out_test = transform(df_out_test,  prd.rank = ave(prd,  FUN = function(x) {rank(-x, ties.method = "first")}))
df_out_test$BIN = cut(df_out_test$prd.rank, breaks=100, labels=FALSE)
df_out_test_grp = ddply(df_out_test, "BIN", summarise, MEAN_TGT = mean(prd), CNT_TGT = sum(act), CNT_CST = length(BIN))
write.table(df_out_test_grp, "D:/wizhan/01_data/out_test.csv", sep="|", row.names=FALSE, col.names=TRUE)

# generalization
model_input_gen <- read.table("D:/wizhan/01_data/INPUT_MODEL_20150905.csv", header=TRUE, sep = ',', quote = "\"")
sapply(model_input_gen, class)
model_input_gen[is.na(model_input_gen)] <- 0
for (i in 2:143){
pct_min = min(quantile(model_input_gen[,i], probs = 0.01),0)
pct_max = max(quantile(model_input_gen[,i], probs = 0.99),0)

model_input_gen[,i] = ifelse(model_input_gen[,i] < pct_min, pct_min, model_input_gen[,i])
model_input_gen[,i] = ifelse(model_input_gen[,i] > pct_max, pct_max, model_input_gen[,i])
}
out_gen = predict(model_fit_vif, newdata=model_input_gen, type="response")
out_gen = cbind(out_gen, model_input_gen[,144])
df_out_gen = data.frame(out_gen)
colnames(df_out_gen) <- c('prd', 'act')
df_out_gen = transform(df_out_gen,  prd.rank = ave(prd,  FUN = function(x) {rank(-x, ties.method = "first")}))
df_out_gen$BIN = cut(df_out_gen$prd.rank, breaks=100, labels=FALSE)
df_out_gen_grp = ddply(df_out_gen, "BIN", summarise, MEAN_TGT = mean(prd), CNT_TGT = sum(act), CNT_CST = length(BIN))
write.table(df_out_gen_grp, "D:/wizhan/01_data/out_gen.csv", sep="|", row.names=FALSE, col.names=TRUE)

# generalization batch 2
model_input_gen_2 <- read.table("D:/wizhan/01_data/INPUT_MODEL_20151107.csv", header=TRUE, sep = ',', quote = "\"")
sapply(model_input_gen_2, class)
model_input_gen_2[is.na(model_input_gen_2)] <- 0
for (i in 2:143){
pct_min = min(quantile(model_input_gen_2[,i], probs = 0.01),0)
pct_max = max(quantile(model_input_gen_2[,i], probs = 0.99),0)

model_input_gen_2[,i] = ifelse(model_input_gen_2[,i] < pct_min, pct_min, model_input_gen_2[,i])
model_input_gen_2[,i] = ifelse(model_input_gen_2[,i] > pct_max, pct_max, model_input_gen_2[,i])
}
out_gen_2 = predict(model_fit_vif, newdata=model_input_gen_2, type="response")
out_gen_2 = cbind(out_gen_2, model_input_gen_2[,144])
df_out_gen_2 = data.frame(out_gen_2)
colnames(df_out_gen_2) <- c('prd', 'act')
df_out_gen_2 = transform(df_out_gen_2,  prd.rank = ave(prd,  FUN = function(x) {rank(-x, ties.method = "first")}))
df_out_gen_2$BIN = cut(df_out_gen_2$prd.rank, breaks=100, labels=FALSE)
df_out_gen_grp_2 = ddply(df_out_gen_2, "BIN", summarise, MEAN_TGT = mean(prd), CNT_TGT = sum(act), CNT_CST = length(BIN))
write.table(df_out_gen_grp_2, "D:/wizhan/01_data/out_gen_2.csv", sep="|", row.names=FALSE, col.names=TRUE)
