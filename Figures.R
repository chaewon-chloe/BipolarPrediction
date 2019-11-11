library(ggplot2)


#### Table5
d = data.frame(
  Subset = rep(c('(C3,P3)','(C3,P3,F3)','(C3,P3,F3,Cz,Pz)'),2,each=3),
  Stimulus = factor(c('Go','NoGo','Go & NoGo'), levels=c('Go','NoGo','Go & NoGo')),
  Epoch = as.factor(rep(c(100,200),each=9)),
  Accuracy = c(69.6, 67.9, 91.1,
               69.6, 75.0, 96.4,
               85.7, 91.1, 94.6,
               
               89.3-69.6, 73.2-67.9, 94.6-91.1,
               78.6-69.6, 92.9-75.0, 96.4-96.4,
               94.6-85.7, 96.4-91.1, 96.4-94.6)
  ) 


ggplot(data=d, aes(x=Subset, y=Accuracy, fill=Epoch)) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) + 
  facet_grid(~Stimulus) + 
  coord_cartesian(ylim=c(65,100)) +
  scale_fill_manual(values = c('100' = "#00BFC4", '200' = "#F8766D")) +
  ylab("Accuracy(%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=18))


#### Table4
library(dplyr)
d4 = data.frame(
  Subset = rep(c('D1','D2','D3','D4'), each=10),
  Method = c('Lin-SVM', 'Poly-SVM', 'Rad-SVM', 'LR', 'KNN', 'RF',
             'DNN(epo=100)','DNN(epo=200)', 'LDA', 'QDA'),
  Accuracy = c(65.9, 68.0, 69.8, 60.7, 62.5, 50.0, 67.9, 69.6, 55.4, 57.1,
               65.9, 81.3, 75.4, 60.7, 60.7, 53.6, 80.4, 83.9, 53.6, 69.6,
               65.9, 72.6, 75.6, 58.9, 62.5, 42.9, 89.3, 94.6, 48.2, 66.1,
               66.8, 70.0, 75.8, 55.4, 62.5, 46.4, 91.1, 96.4, 53.6, 53.6)
) %>% 
  mutate(Algorithm = ifelse(Method %in% c('Rad-SVM','Poly-SVM','Lin-SVM'),
                            'SVM',
                            ifelse(Method %in% c('DNN(epo=200)','DNN(epo=100)'),'DNN','Others'))) %>%
  arrange(Subset, desc(Accuracy)) %>%
  mutate(r = rep(1:10,4))
d4$Algorithm = factor(d4$Algorithm, levels = c('DNN','SVM','Others'))

ggplot(data=d4, aes(x=r, y=Accuracy, fill=Algorithm)) + 
  geom_bar(stat='identity') +
  facet_grid(Subset~.) + 
  scale_fill_manual(values = c('DNN' = "#00BFC4", 'SVM' = "#F8766D", 'Others'="#999999")) +
  scale_x_continuous(breaks = d4$r, labels=d4$Method) +
  coord_cartesian(ylim=c(45,100)) + 
  ylab("Accuracy(%)") + 
  theme(text = element_text(size=18),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
