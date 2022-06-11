#library
library(tidyverse)
library(kableExtra)           #view data- kable
library(PerformanceAnalytics) #view chart- chart.Correlation
library(reshape2)             #view chart- melt
library(psych)                #view chart- KMO / Bartlett's
library(plotly)               #view chart- ggplotly
library(factoextra)           #view chart- fviz_eig
library(ggrepel)              #view chart- geom_text_repel


#load data
load("data/atlasambiental.RData")

#view data
atlasambiental %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
# cod_ibge  distritos           renda     quota     escolaridade ... denspop
#
# 1         Água Rasa           1961      34.6      7.60    32        125.61   
# 12        Alto de Pinheiros   4180      76.0      8.40    33        57.56    
# 23        Anhanguera          1093      4.5       5.80    23        8.57      
# 34        Aricanduva          1311      21.0      6.80    27        138.54   

#view correlations between variables
chart.Correlation(atlasambiental[, 3:11], histogram = TRUE, pch = "+")

#create the matrix of correlations
rho_atlas <- cor(atlasambiental[,3:11])

#create heatmap from correlations
rho_atlas %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "correlations") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#Bartlett's sphericity test
cortest.bartlett(R = rho_atlas)
# chisq       p.value           df
#
# 780.9853    8.646135e-141     36

#prcomp() algorithm, from the psych package, REQUIRES standardized base
#note: delete var cod_ibge, append var districts to id, standardize var numerics
atlasambiental_std <- atlasambiental %>% 
  select(-cod_ibge) %>% 
  column_to_rownames("distritos") %>% 
  scale() %>% 
  data.frame()
#                      renda        quota         escolaridade ... denspop
#
# Água Rasa            0.107935732  0.179771452   0.52036465         0.51907982 
# Alto de Pinheiros    2.393739629  1.974023367   1.31330115        -0.85270752 
# Anhanguera          -0.786195761 -1.127506380  -1.26374281        -1.84027349   
# Aricanduva          -0.561632782 -0.410500034  -0.27257184         0.77972935

#generate PCA
afpc_atlas <- prcomp(atlasambiental_std)
summary(afpc_atlas)
#Importance of components:
#                       PC1    PC2    PC3     PC4     PC5     ...   PC9        
#Standard deviation     2.2262 1.0790 0.9982 0.85092 0.72753        0.19196
#Proportion of Variance 0.5507 0.1294 0.1107 0.08045 0.05881        0.00409
#Cumulative Proportion  0.5507 0.6800 0.7907 0.87120 0.93001        1.00000

#summarize important points, format in management table
data.frame(eigenvalue = afpc_atlas$sdev ^ 2,
           var_compartilhada = summary(afpc_atlas)$importance[2,],
           var_cumulativa = summary(afpc_atlas)$importance[3,]) -> relatorio
relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#     eigenvalue    var_compartilhada   var_cumulativa
#PC1  4.95603069    0.55067             0.55067
#PC2  1.16433814    0.12937             0.68004
#PC3  0.99635412    0.11071             0.79075
#PC4  0.72406663    0.08045             0.87120
#PC5  0.52930223    0.05881             0.93001
#PC6  0.39832415    0.04426             0.97427
#PC7  0.12966871    0.01441             0.98868
#PC8  0.06506649    0.00723             0.99591
#PC9  0.03684884    0.00409             1.00000

#view weights of each variable on PCA 
ggplotly(
  data.frame(afpc_atlas$rotation) %>%
    mutate(var = names(atlasambiental[3:11])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "subtitle:") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

#shared var graph - ignore the warnings
ggplotly(
  fviz_eig(X = afpc_atlas,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#extract factor loadings, format in management table
k <- sum((afpc_atlas$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_atlas$rotation[, 1:k] %*% diag(afpc_atlas$sdev[1:k])
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#              [,1]        [,2]
# renda        -0.8321269  0.36897098
# quota        -0.9006058  0.22645316
# escolaridade -0.9665432 -0.02459367
# idade        -0.9601351 -0.06544713
# mortalidade   0.6556993 -0.17663396
# txcresc       0.6967815  0.33744591
# causasext     0.6665775 -0.04390999
# favel         0.4571854  0.44283981
# denspop      -0.1662813 -0.79304783

#visualize commonality
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#                 comunalidades
#
# renda	          0.8285748
# quota	          0.8623719
# escolaridade	  0.9348107
# idade	          0.9261428
# mortalidade	    0.4611411
# txcresc	        0.5993741
# causasext	      0.4462537
# favel	          0.4051255
# denspop	        0.6565743

#join factor loadings and commonalities
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#               F1	         F2	        Comunalidades
#renda	        -0.8321269	 0.3689710	0.8285748
#quota	        -0.9006058	 0.2264532	0.8623719
#escolaridade	  -0.9665432	-0.0245937	0.9348107
#idade	        -0.9601351	-0.0654471	0.9261428
#mortalidade	   0.6556993	-0.1766340	0.4611411
#txcresc	       0.6967815	 0.3374459	0.5993741
#causasext	     0.6665775	-0.0439100	0.4462537
#favel	         0.4571854	 0.4428398	0.4051255
#denspop	      -0.1662813	-0.7930478	0.6565743

#factor loading graph
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = "F1",
       y = "F2") +
  theme_bw()

#factor scores, format in management table
scores_fatoriais <- t(afpc_atlas$rotation)/afpc_atlas$sdev 
colnames(scores_fatoriais) <- colnames(atlasambiental_std)
scores_fatoriais
#     renda      quota       escolaridade ..   denspop
#
#PC1 -0.16790189 -0.1817192 -0.195023659      -0.033551305
#PC2  0.31689332  0.1944909 -0.021122444      -0.681114708
#PC3 -0.17327380 -0.1552978 -0.009374416      -0.410900501
#PC4  0.07449723  0.1644845  0.099403708      -0.448321004

#view factor scores by PC
scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#                 PC1	         PC2
#renda	          -0.1679019	 0.3168933
#quota	          -0.1817192	 0.1944909
#escolaridade	    -0.1950237	-0.0211224
#idade	          -0.1937307	-0.0562097
#mortalidade	     0.1323033	-0.1517033
#txcresc	         0.1405926	 0.2898178
#causasext	       0.1344983	-0.0377124
#favel	           0.0922483	 0.3803361
#denspop	        -0.0335513	-0.6811147

#build ranking ------------------------------------

#Assuming only F1 (PC1) as an indicator, the factor scores are calculated
score_D1 <- scores_fatoriais[1,]
score_D1
#       renda          quota        escolaridade ... denspop
#
#PC1   -0.16790189    -0.18171917  -0.19502366      -0.03355130

#establish the ranking of indicators
F1 <- t(apply(atlasambiental_std, 1, function(x) x * score_D1))
F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)