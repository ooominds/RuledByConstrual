
load('cxdat.rda')
# cxdat = read.csv('cxdat.csv')

##########################################
### Logistic Linear Mixed Effect Model ###
##########################################

options(show.signif.stars=FALSE)

require(lme4)
require(MuMIn) # for estimated R^2

summary(lme1 <- glmer(MatchChosenMode ~
    1 + (1|ParticipantNo) + (1|QuestionID),
    data=cxdat,
    family='binomial'), cor=FALSE)
#      AIC      BIC   logLik deviance df.resid 
#   9027.2   9048.9  -4510.6   9021.2    10228 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -6.8796  0.1292  0.2884  0.5053  2.1229 
# 
# Random effects:
#  Groups        Name        Variance Std.Dev.
#  ParticipantNo (Intercept) 0.3844   0.620   
#  ID2           (Intercept) 1.7093   1.307   
# Number of obs: 10231, groups:  ParticipantNo, 181; ID2, 171
# 
# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.8395     0.1164    15.8   <2e-16 ***

# r.squaredLR(lme1)
# # [1] 0.130905
# # attr(,"adj.r.squared")
# # [1] 0.2044927
# 
# prop.table(table(cxdat$MatchChosenMode))
# #         0         1 
# # 0.2077998 0.7922002 
# 
# plogis(1.8395)
# # [1] 0.862889618

##########################################
###         Caterpillar plots          ###
##########################################

require(ggplot2) 
require(grid)
source('ooom_ggDotplot.R') # modified version of ggplot for caterpillar plots

# random effects estimates
refs = ranef(lme1, condVar=TRUE)

# # # If you want to plot old-style, with dotplot()
# # refp <- dotplot(refs, scales=list(x=list(relation='free')))[['ParticipantNo']]
# # refq <- dotplot(refs, scales=list(x=list(relation='free')))[['QuestionID']]
# 
# # # Using our home-made Ooominds funtion
# # refp <- ggDotplot(refs, ylim=c(-3,3.5), main='Participants')[['ParticipantNo']]
# # refq <- ggDotplot(refs, ylim=c(-3,3.5), main='Items')[['QuestionID']]
# 
# pushViewport(viewport(
#     layout=grid.layout(1, 2, heights=unit(c(7), 'null'),
#        widths=unit(c(6,6), 'null'))))
# print(refp, vp=viewport(layout.pos.row=1, layout.pos.col=1))
# print(refq, vp=viewport(layout.pos.row=1, layout.pos.col=2))

# Extracting random effects estimates
refs.df = as.data.frame(refs)
refs.df = refs.df[,c(1,3,4,5)] 
refs.df$lo = refs.df$condval - 1.96*refs.df$condsd # lower bound of 95% confidence interval
refs.df$hi = refs.df$condval + 1.96*refs.df$condsd # upper bound of 95% confidence interval

# Split data for participants and questions
partc = refs.df[refs.df$grpvar == 'ParticipantNo',,drop=TRUE]
quest = refs.df[refs.df$grpvar == 'QuestionID',,drop=TRUE]

# Getting three groups of participants
partc = partc[order(partc[,3]),]
partc$ParticipantAgreement = ifelse(partc$condval < 0 & partc$hi < 0, 'low',
              ifelse(partc$condval > 0 & partc$lo > 0, 'high', 'average'))

# Getting three groups of questions
quest = quest[order(quest[,3]),]
quest$QuestionAgreement = ifelse(quest$condval < 0 & quest$hi < 0, 'low',
              ifelse(quest$condval > 0 & quest$lo > 0, 'high', 'average'))

# Adding random effects estimates to the main dataset 
dat2a = merge(cxdat, partc[,c(2,7)], by.x='ParticipantNo', by.y='grp')
dat2b = merge(dat2a, quest[,c(2,7)], by.x='QuestionID', by.y='grp')
dat2 = dat2b

# Ordering groups from low to high
dat2$ParticipantAgreement = factor(dat2$ParticipantAgreement, levels=c('low', 'average', 'high'))
dat2$QuestionAgreement = factor(dat2$QuestionAgreement, levels=c('low', 'average', 'high'))

prop.table(table(dat2$ParticipantAgreement))
#        low    average       high 
# 0.14983873 0.80607956 0.04408171 

prop.table(table(dat2$QuestionAgreement))
#       low   average      high 
# 0.3906754 0.4031864 0.2061382 

######################################
###        Log-Linear models       ###
######################################

# load('dat2.rda')

# Model candidate with good fit
tabdat <- xtabs(~
    MatchChosenMode +
    Mode +
    QuestionAgreement,
    data=dat2)

ll1 = loglin(tabdat, list(c(1,2), c(1,3), c(2,3)), param=TRUE, fit=TRUE)
1 - pchisq(ll1$lrt, ll1$df) # 0.148041

# Comparison with the model of full independence
# ll0 = loglin(tabdat, list(c(1), c(2,3)), param=TRUE, fit=TRUE)
# ll0$lrt - ll1$lrt
# [1] 1499.676

# Adding frequency predicted by the best model
tabdat2 = cbind(data.frame(tabdat),
    'loglinFreq'=data.frame(ll1$fit)[,4])

#######################################
###      Plotting histograms        ###
#######################################

mismatch = droplevels(tabdat2[tabdat2$MatchChosenMode == 0,])
match = droplevels(tabdat2[tabdat2$MatchChosenMode == 1,])

p1 <- ggplot(data=match, aes(x=QuestionAgreement, y=Freq, fill=factor(Mode))) +
    coord_cartesian(ylim = c(0, 2000)) +
    geom_bar(position=position_dodge(.9), width=0.5, stat='identity') +
    # Adding frequencies predicted by the model 
    geom_errorbarh(data=match, aes(xmin=as.numeric(QuestionAgreement)-.2,
        xmax=as.numeric(QuestionAgreement)+.2, y=loglinFreq), # +/-.2 for width
        position=position_dodge(0.9), size=1.5, height=0) +
    scale_x_discrete(name='Question agreement') +
    scale_y_continuous('Frequency') +
    scale_fill_brewer(palette='Greys', name='Dominant\narticle:') +
    ggtitle('Matches') +
    theme(axis.text=element_text(size=11),
        axis.title.x=element_text(size=13, vjust=-0.75),
        axis.title.y=element_text(size=13, vjust=0.75),
        legend.text=element_blank(),
        legend.position='none',
        legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))
p2 <- ggplot(data=mismatch, aes(x=QuestionAgreement, y=Freq, fill=factor(Mode))) +
    coord_cartesian(ylim = c(0, 2000)) +
    geom_bar(position=position_dodge(.9), width=0.5, stat='identity') +
    # Adding frequencies predicted by the model
    geom_errorbarh(data=mismatch, aes(xmin=as.numeric(QuestionAgreement)-.2,
        xmax=as.numeric(QuestionAgreement)+.2, y=loglinFreq), # +/-.2 for width
        position=position_dodge(0.9), size=1.5, height=0) +
    scale_x_discrete(name='Question agreement') +
    scale_y_continuous('Frequency') +
    scale_fill_brewer(palette='Greys', name='Dominant\narticle:') +
    ggtitle('Mismatches') +
    theme(axis.text=element_text(size=11),
        axis.title.x=element_text(size=13, vjust=-0.75),
        axis.title.y=element_text(size=13, vjust=0.75),
        legend.text=element_text(size=11),
        legend.title=element_text(size=13),
        plot.title=element_text(hjust=0.5))

pushViewport(viewport(
    layout=grid.layout(1, 2, heights=unit(c(1), 'null'),
        widths=unit(c(5.4,6.6), 'null'))))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
