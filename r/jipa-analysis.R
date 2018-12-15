library(tidyverse)
library(mgcv)
library(itsadug)
library(summarytools)
library(reshape2)
library(rstanarm)
library(yarrr)
library(lmerTest)
library(sjPlot)
library(tidybayes)
library(hrbrthemes)
library(cowplot)
options(mc.cores=parallel::detectCores()) # Run on multiple cores

set.seed (3875)

setwd("~/Documents/pohnpeian-phonetics/data")

data <- read.csv("locus-eq/locus-new.csv")

data <- data %>% separate(label, c("vowel","consonant"), sep="-")



data <- data %>% separate(filename, c("delete1","delete2","speaker", "word", "Repeat") , remove=F, sep="_") %>% separate(word, c("word", "frame"), sep="-", remove=F) %>% select(-delete1, -delete2)

data <- data %>% spread(percent.through.interval, F2) %>% select(-`100`,-filename,-Repeat)
data$f2ss = data$`50`
data$f2i = data$`0`
data <- data %>% select(-`0`,-`50`)
data$consonant <- ifelse(data$consonant == "t", "t̻",ifelse(data$consonant=="d","t",data$consonant))
locus.data <- data
# old data
#pw.data <- read.csv("locus-eq/p_pw_locus.csv")
#t.data <- read.csv("locus-eq/t_d_locus.csv")

#locus.data <- merge(locus.data,t.data,by.x=c("speaker", "word", "vowel","consonant","f2i","f2ss"), by.y=c("speaker","word", "vowel","consonant","f2i","f2ss"), all=T)


## plot vowels for locus


#pirateplot(f2ss~vowel+consonant,data=locus.data,inf="hdi",theme=3,hdi.iter=50000,avg.line.fun = median,ylab="F2 Steady State (Hz)",gl.col = "white",xlab="Consonant",pal="gray")#,bean.f.col=c("seagreen4","skyblue"))
#pirateplot(f2i~vowel+consonant,data=locus.data,inf="hdi",theme=3,hdi.iter=50000,avg.line.fun = median,ylab="F2 Initial (Hz)",gl.col = "white",xlab="Consonant",pal="gray")#,bean.f.col=c("seagreen4","skyblue"))


#pirateplot(f2ss~consonant,data=locus.data,inf="hdi",theme=3,hdi.iter=50000,avg.line.fun = median,ylab="F2 Steady State (Hz)",gl.col = "white",xlab="Consonant",pal="gray")#,bean.f.col=c("seagreen4","skyblue"))
#pirateplot(f2i~consonant,data=locus.data,inf="hdi",theme=3,hdi.iter=50000,avg.line.fun = median,ylab="F2 Initial (Hz)",gl.col = "white",xlab="Consonant",pal="gray")#,bean.f.col=c("seagreen4","skyblue"))





# descriptive stats
pw_locus_pw <- locus.data %>% filter(consonant == "pw")
pw_locus_p <- locus.data %>% filter(consonant == "p")
t_locus_t <- locus.data %>% filter(consonant == "t̻")
t_locus_d <- locus.data %>% filter(consonant == "t")


#ggplot(data=locus.data, aes(x=f2ss,y=f2i, color=consonant)) + geom_point() + geom_smooth(method="lm", fullrange=T) + theme_ipsum_rc() + scale_colour_ipsum()

#pw.data <- read.csv("locus-eq/p_pw_locus.csv")
#t.data <- read.csv("locus-eq/t_d_locus.csv")
#locus.data.old <- merge( pw.data, t.data,by.x=c("speaker", "word", "vowel","consonant","f2i","f2ss"), by.y=c("speaker","word", "vowel","consonant","f2i","f2ss"), all=T)
#ggplot(data=locus.data.old, aes(x=f2ss,y=f2i, color=consonant)) + geom_point() + geom_smooth(method="lm", fullrange=T) 

locus.data.counts.vowel <- locus.data %>% group_by(speaker, consonant) %>% count(vowel)
locus.data.counts.word <- locus.data  %>% count(word)
locus.data.counts.consonant <- locus.data  %>% count(consonant)
locus.data.counts.consonant.speaker <- locus.data %>% group_by(speaker)  %>% count(consonant)
locus.data.counts.speaker <- locus.data  %>% count(speaker)


locus.lmer <- lmer(f2i ~ f2ss + consonant + (1|speaker)+(1|word), data=locus.data)
summary(locus.lmer)
plot_model(locus.lmer, type="re") # eff, est, int, std, slope, resid, diag
plot_model(locus.lmer, type="diag") # eff, est, int, std, slope, resid, diag


 
## Bayesian regression for locus


locus.blmer = stan_lmer(f2i ~ f2ss + consonant + (1|speaker)+(1|word), data=locus.data, 
                        prior_intercept = normal(0, 2),
                        prior = normal(0, 2),
                        prior_covariance = decov(regularization = 2), 
                        chains = 6,
                        iter = 3000, adapt_delta = 0.9999999 )

locus.blm <- stan_lm(f2i ~ f2ss + consonant, data=locus.data,  prior = R2(0.75),
                    # prior_intercept = normal(0, 2),
                     #prior = normal(0, 2),
                    # prior_covariance = decov(regularization = 2), 
                     chains = 6,
                     iter = 3000, adapt_delta = 0.9999999 )


locus.data %>%
  group_by(consonant) %>%
  modelr::data_grid(f2ss = modelr::seq_range(f2ss, n = 51)) %>%
  add_fitted_draws(locus.blm) %>%
  ggplot(aes(x = f2ss, y = f2i, color = ordered(consonant))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  stat_lineribbon(aes(y = .value), fullrange=T) +
  geom_point(data = locus.data) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") 







pw_locus.blmer = stan_lmer(f2i ~ f2ss + (1|speaker), data=pw_locus_pw, 
                           prior_intercept = normal(0, 2),
                           prior = normal(0, 2),
                           prior_covariance = decov(regularization = 2), 
                           chains = 6,
                           iter = 3000, adapt_delta = 0.9999999 )
print(summary(pw_locus.blmer), digits=3)

plot(pw_locus.blmer)
plot_model(pw_locus.blmer)

draws3 <- as.data.frame(pw_locus.blmer)
colnames(draws3)[1:2] <- c("a", "b")
pw.plot <- ggplot(pw_locus_pw, aes(x = f2ss, y = f2i)) + geom_abline(data = draws3, aes(intercept = a, slope = b), 
                                                                     color = "orchid4", size = 0.2, alpha = 0.03) +
  geom_point(size = 1) + xlim(1000,3000)  + theme_bw() + xlab("F2 steady state (Hz)")+
  ylab("F2 Initial (Hz)") + ggtitle("/pʷ/ Locus Equations")+ ylim(500,3000)

  

ggsave("pw_locus.png", dpi=300, width=6,height=6,units="in",device="png")

### p

p_locus.blmer = stan_lmer(f2i ~ f2ss + (1|speaker), data=pw_locus_p, 
                          prior_intercept = normal(0, 2),
                          prior = normal(0, 2),
                          prior_covariance = decov(regularization = 2), 
                          chains = 6,
                          iter = 3000, adapt_delta = 0.999999 )

plot(p_locus.blmer)

draws4 <- as.data.frame(p_locus.blmer)
colnames(draws4)[1:2] <- c("a", "b")
p.plot<- ggplot(pw_locus_p, aes(x = f2ss, y = f2i)) + geom_abline(data = draws4, aes(intercept = a, slope = b), 
                                                                  color = "darkred", size = 0.2, alpha = 0.03) +
  geom_point(size = 1) + xlim(1000,3000) + ylim(500,3000) + theme_bw() + xlab("F2 steady state (Hz)")+
  ylab("F2 Initial (Hz)") + ggtitle("/p/ Locus Equations")

  
ggsave("p_locus.png", dpi=300, width=6,height=6,units="in",device="png")


# t

t_locus.blmer = stan_lmer(f2i ~ f2ss + (1|speaker), data=t_locus_t, 
                          prior_intercept = normal(0, 2),
                          prior = normal(0, 2),
                          prior_covariance = decov(regularization = 2), 
                          chains = 6,
                          iter = 3000,adapt_delta=0.99999)

blmer_t_locus_plot<-plot(t_locus.blmer)
blmer_t_locus_plot
#plotly_POST(blmer_t_locus_plot, filename = "blmer_t_locus_plot", sharing="public")
print(summary(t_locus.blmer), digits=2)


draws <- as.data.frame(t_locus.blmer)
colnames(draws)[1:2] <- c("a", "b")

write.csv(draws, "draws.csv")
# laminal

t.plot <- ggplot(t_locus_t, aes(x = f2ss, y = f2i)) + geom_abline(data = draws, aes(intercept = a, slope = b), color = "skyblue", size = 0.2, alpha = 0.03)+
  geom_point(size = 1) + xlim(1000,3000) + ylim(500,3000) + theme_bw() + xlab("F2 steady state (Hz)")+
  ylab("F2 Initial (Hz)") + ggtitle("/t̻/ Locus Equations")


t.plot 
  

#grid.arrange(d_plot, t_plot,p_plot,pw_plot,  ncol=2)


# save plot
t_plot
ggsave("t_locus.png", dpi=300, width=6,height=6,units="in",device="png")


### d

d_locus.blmer = stan_lmer(f2i ~ f2ss + (1|speaker), data=t_locus_d, 
                          prior_intercept = normal(0, 2),
                          prior = normal(0, 2),
                          prior_covariance = decov(regularization = 2), 
                          chains = 6,
                          iter = 3000, adapt_delta = 0.999999)

blmer_d_locus_plot<-plot(d_locus.blmer)
#plotly_POST(blmer_d_locus_plot, filename = "blmer_d_locus_plot", sharing="public")


sink("sink-summary-blmer-d_locus.txt")
print(summary(d_locus.blmer),digits=4)
sink()


draws2 <- as.data.frame(d_locus.blmer)
colnames(draws2)[1:2] <- c("a", "b")
d.plot<- ggplot(t_locus_d, aes(x = f2ss, y = f2i)) + geom_abline(data = draws2, aes(intercept = a, slope = b), 
                                                                 color = "seagreen4", size = 0.2, alpha = 0.03) +
  geom_point(size = 1) + xlim(1000,3000) + ylim(500,3000) + theme_bw() + xlab("F2 steady state (Hz)")+
  ylab("F2 Initial (Hz)") + ggtitle("/t/ Locus Equations")


d.plot 

ggsave("d_locus.png", dpi=300, width=6,height=6,units="in",device="png")


base3 <- ggplot(t_locus, aes(x = f2ss, y = f2i)) + 
  geom_point(size = 1)
base3 + 
  geom_abline(data = draws2, aes(intercept = a, slope = b), 
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(data = draws, aes(intercept = a, slope = b), 
              color = "green", size = 0.2, alpha = 0.25)

plot_grid(p.plot, pw.plot, d.plot, t.plot, labels=c("(a)","(b)","(c)","(d)"),ncol=2)
ggsave("locus_all.png", dpi=300, width=12,height=12,units="in",device="png")



dt.plot <- ggplot(t_locus_d, aes(x = f2ss, y = f2i)) + geom_abline(data = draws2, aes(intercept = a, slope = b), 
                                                                            color = "seagreen4", size = 0.2, alpha = 0.03) +
 xlim(1000,3000) + ylim(500,3000) + theme_bw() + xlab("F2 steady state (Hz)")+ geom_abline(data = draws, aes(intercept = a, slope = b), 
                                                                                                                   color = "skyblue", size = 0.2, alpha = 0.03) +
  ylab("F2 Initial (Hz)") + ggtitle("/t/ (green) and /t̻/ (blue) Locus Equations") 
dt.plot


pwp.plot <- ggplot(pw_locus_p, aes(x = f2ss, y = f2i)) + geom_abline(data = draws4, aes(intercept = a, slope = b), 
                                                                      color = "darkred", size = 0.2, alpha = 0.03) +
  xlim(1000,3000) + ylim(500,3000) + theme_bw() + xlab("F2 steady state (Hz)")+ geom_abline(data = draws3, aes(intercept = a, slope = b), 
                                                                                            color = "orchid4", size = 0.2, alpha = 0.03) +
  ylab("F2 Initial (Hz)") + ggtitle("/p/ (red) and /pʷ/ (purple) Locus Equations") 

pwp.plot
plot_grid(pwp.plot, dt.plot, labels=c("(a)","(b)"),ncol=2)
ggsave("locus_combined.png", dpi=300, width=10,height=6,units="in",device="png")

####

locus_plots<-ggplot(NULL, aes(y=f2i, x=f2ss, color=consonant, shape=consonant, linetype=consonant)) + 
  geom_point(data=locus.data) +
  theme_bw() + 
 # geom_smooth(data=t_locus,method=lm, fullrange=TRUE, se=FALSE,alpha = .15,size=0.5)  +
  geom_smooth(data=locus.data,method=lm,fullrange=T,se=T,alpha=.15,size=0.5)+
  ylab("F2 initial (Hz)") + 
  xlab("F2 steady state (Hz)") +
  scale_shape_manual(values=c(0,1,5,6))+
  scale_colour_manual(values=c("#000000","#900009","#666666","#145314"))+
  scale_linetype_manual(values=c("solid", "longdash", "dotted", "twodash")) +
  #name="consonant", 
  #breaks=c("t", "t̻"), 
  #labels = c("apical", "laminal")) +
  #scale_linetype_discrete()+#name="consonant", 
  # breaks=c("t", "t̻"), 
  #labels = c("apical", "laminal")) +
  #guides(shape=F)+
  theme(legend.position=c(.8,0.3))
locus_plots
