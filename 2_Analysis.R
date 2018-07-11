

################################################################################
## R-Script - 2_Analysis.R                                                    ##
## author: Javier Lopatin                                                     ##
## mail: javierlopatin@gmail.com                                              ##  
##                                                                            ##
## Manuscript: Example scripts of the modeling approach for the Acacia flight ##
##                                                                            ##
## description: MaxEnt classification storing of bootstrap distributions      ## 
##                                                                            ##
################################################################################


setwd("C:/Users/Lopatin/Dropbox/PhD/UAV_invasive_spp/")

library("dismo")

#load("Analysis.RData")

#################################################
### Functions

# Function to obtain model accuracies
getACC <- function(eval){ 
  # accuracies 
  auc <- eval@auc
  kappa <- max(eval@kappa)
  TPR <- eval@TPR[which.max( eval@kappa )]
  TNR <- eval@TNR[which.max( eval@kappa )]
  mat <- eval@confusion[which.max( eval@kappa ),]
  num <- (mat[1]*mat[4])-(mat[3]*mat[2])
  den <- (mat[1]+mat[2])*(mat[3]+mat[4])
  tss <- num/den
  
  # prepare output
  out <- list(auc, kappa, TPR, TNR, tss)
  names(out) <- c("AUC", "Kappa", "TPR", "TNR", "TSS")
  out
  
}

# Functions to iterate through a list of eveluations for the overall models
iterList_all <- function(listname, model="all"){
  auc <- numeric()
  kappa <- numeric()
  TPR <- numeric()
  TNR <- numeric()
  tss <- numeric()
  for (i in 1:length(listname)){
    if (model == "all"){
      load(listname[[i]])
      out <- getACC(eval)
    }
    if (model == "sunny"){
      load(listname[[i]])
      out <- getACC(eval_sunny)
    }
    if (model == "shadows"){
      load(listname[[i]])
      out <- getACC(eval_shadow)
    }
    auc[i] <- out$AUC
    kappa[i] <- out$Kappa 
    TPR[i] <- out$TPR
    TNR[i] <- out$TNR
    tss[i] <- out$TSS
  }
  output <- list(auc, kappa, TPR, TNR, tss)
  names(output) <- c("AUC", "Kappa", "TPR", "TNR", "TSS")
  output
}

# Functions to iterate through a list of eveluations for the sunny models
iterList_sunny <- function(listname, model="all"){
  auc <- numeric()
  kappa <- numeric()
  TPR <- numeric()
  TNR <- numeric()
  tss <- numeric()
  for (i in 1:length(listname)){
    if (model == "all"){
      load(listname[[i]])
      out <- getACC(eval2)
    }
    if (model == "sunny"){
      load(listname[[i]])
      out <- getACC(eval_sunny2)
    }
    if (model == "shadows"){
      load(listname[[i]])
      out <- getACC(eval_shadow2)
    }
    auc[i] <- out$AUC
    kappa[i] <- out$Kappa 
    TPR[i] <- out$TPR
    TNR[i] <- out$TNR
    tss[i] <- out$TSS
  }
  output <- list(auc, kappa, TPR, TNR, tss)
  names(output) <- c("AUC", "Kappa", "TPR", "TNR", "TSS")
  output
}

# function to iterate th through all models (overall, sunny and shadow)
GetAllAcc <- function(pattern, model="all"){ 
  
  # list of files matching the pattern
  if (model == "all"){
    listt <- list.files("H:/results2/eval/all/", pattern=pattern, full.names = T)  
  }
  if (model == "sunny"){
    listt <- list.files("H:/results2/eval/sunny/", pattern=pattern, full.names = T)  
  }
 
  listNames <- c("rgb_", "texture", "struct_", "hyper_", "strcttext_", "structrgb_",
                 "structhyper_", "textrgb_", "texthyper_",
                 "structextrgb_", "structexthyper_")
  # iterate
  auc <- list()
  kappa <- list()
  TPR <- list()
  TNR <- list()
  tss <- list()
  if (model == "all"){
    for (i in 1:length(listNames)){ 
      # all
      all <- listt[ grep(paste0("^", paste0("H:/results2/eval/all/", listNames[[i]]) ), listt) ]
      all <- sort(all)
      # overall
      all1 <- all[ -grep(paste(c("sunny", "shadows"), collapse = "|"), basename(all)) ]
      all1_acc <- iterList_all(all1, "all")
      # sunny 
      all2 <- all[ grep("sunny",  basename(all)) ]
      all2_acc <- iterList_all(all2, model = "sunny")
      # shadows 
      all3 <- all[ grep("shadows",  basename(all)) ]
      all3_acc <- iterList_all(all3, model = "shadows")
      # accuracies
      auc[[i]]       <- list(all1_acc$AUC, all2_acc$AUC, all3_acc$AUC)
      kappa[[i]]     <- list(all1_acc$Kappa, all2_acc$Kappa, all3_acc$Kappa)
      TPR[[i]]       <- list(all1_acc$TPR, all2_acc$TPR, all3_acc$TPR)
      TNR[[i]]       <- list(all1_acc$TNR, all2_acc$TNR, all3_acc$TNR)
      tss[[i]]       <- list(all1_acc$TSS, all2_acc$TSS, all3_acc$TSS)
    }
  }
  
  if (model == "sunny"){
    for (i in 1:length(listNames)){ 
      # all
      all <- listt[ grep(paste0("^", paste0("H:/results2/eval/sunny/", listNames[[i]]) ), listt) ]
      all <- sort(all)
      # overall
      all1 <- all[ -grep(paste(c("sunny", "shadows"), collapse = "|"), basename(all)) ]
      all1_acc <- iterList_sunny(all1, "all")
      # sunny 
      all2 <- all[ grep("sunny",  basename(all)) ]
      all2_acc <- iterList_sunny(all2, model = "sunny")
      # shadows 
      all3 <- all[ grep("shadows",  basename(all)) ]
      all3_acc <- iterList_sunny(all3, model = "shadows")
      # accuracies
      auc[[i]]       <- list(all1_acc$AUC, all2_acc$AUC, all3_acc$AUC)
      kappa[[i]]     <- list(all1_acc$Kappa, all2_acc$Kappa, all3_acc$Kappa)
      TPR[[i]]       <- list(all1_acc$TPR, all2_acc$TPR, all3_acc$TPR)
      TNR[[i]]       <- list(all1_acc$TNR, all2_acc$TNR, all3_acc$TNR)
      tss[[i]]       <- list(all1_acc$TSS, all2_acc$TSS, all3_acc$TSS)
    }
  }
  
  
  output <- list(auc, kappa, TPR, TNR, tss)
  names(output) <- c("AUC", "Kappa", "TPR", "TNR", "TSS")
  output
}

#############################################
### Analysis
# Overall
eval_acacia <- GetAllAcc("acacia", model="all")
eval_ulex <- GetAllAcc("ulex", model="all")
eval_pinus <- GetAllAcc("pinus", model="all")

# Sunny
eval_acacia2 <- GetAllAcc("acacia", model="sunny")
eval_ulex2 <- GetAllAcc("ulex", model="sunny")
eval_pinus2 <- GetAllAcc("pinus", model="sunny")


### Comparison between overall, sunny and shadow 

# function to obtain the median and CV of the distribution of values
getValues <- function(eval1, eval2, x){ 
  doo <- function(eval, x){
    AUC = matrix(nrow = 11, ncol = 2); colnames(AUC) <- c("median", "CV")
    kappa = matrix(nrow = 11, ncol = 2); colnames(kappa) <- c("median", "CV")
    TPR = matrix(nrow = 11, ncol = 2); colnames(TPR) <- c("median", "CV")
    TNR = matrix(nrow = 11, ncol = 2); colnames(TNR) <- c("median", "CV")
    
    for (i in 1:11){ 
       AUC[i,1] = median(eval[[1]][[i]][[x]])
       AUC[i,2] = cv(eval[[1]][[i]][[x]])
       kappa[i,1] = median(eval[[2]][[i]][[x]])
       kappa[i,2] = cv(eval[[2]][[i]][[x]])
       TPR[i,1] = median(eval[[3]][[i]][[x]])
       TPR[i,2] = cv(eval[[3]][[i]][[x]])
       TNR[i,1] = median(eval[[4]][[i]][[x]])
       TNR[i,2] = cv(eval[[4]][[i]][[x]])
    }
    
    out <- list(AUC, kappa, TPR, TNR)
  
    med = matrix(nrow=11, ncol=4)
    cv = matrix(nrow=11, ncol=4)
    for (i in 1:length(out)){
      med[,i] = out[[i]][,1]
      cv[,i] = out[[i]][,2]
    }
    
  list(med, cv)

  }
  doo1 <- doo(eval1, x)
  doo2 <- doo(eval2, x)
  med <- matrix(nrow=11, ncol=8)
  cv <- matrix(nrow=11, ncol=8)
  for (i in 1:4){
    med[,seq(1,8,2)[i]] <- doo1[[1]][,i]
    med[,seq(1,8,2)[i]+1] <- doo2[[1]][,i]
    cv[,seq(1,8,2)[i]] <- doo1[[2]][,i]
    cv[,seq(1,8,2)[i]+1] <- doo2[[2]][,i]
  }
  rownames(med) <- c("rgb", "texture", "struct", "hyper", "strcttext", "structrgb",
                        "structhyper", "textrgb", "texthyper",
                        "structextrgb", "structexthyper")
  rownames(cv) <- c("rgb", "texture", "struct", "hyper", "strcttext", "structrgb",
                        "structhyper", "textrgb", "texthyper",
                        "structextrgb", "structexthyper")
  colnames(med) <- c("AUC1", "AUC2", "Kappa1", "Kappa2", "TPR1", "TPR2","TNR1","TNR2")
  colnames(cv) <- c("AUC1", "AUC2", "Kappa1", "Kappa2", "TPR1", "TPR2","TNR1","TNR2")
  
  out <- list(med, cv); names(out) <- c("med", "cv")
  out
  }

# Accuracies over the whole canopies
acaciaValues = getValues(eval_acacia, eval_acacia2, 1)
ulexValues = getValues(eval_ulex, eval_ulex2, 1)
pinusValues = getValues(eval_pinus, eval_pinus2, 1)

acaciaValues$med
ulexValues$med
pinusValues$med

acaciaValues$cv
ulexValues$cv
pinusValues$cv

# Accuracies over the sunny canopies
acaciaValues_sunny = getValues(eval_acacia, eval_acacia2, 2)
ulexValues_sunny = getValues(eval_ulex, eval_ulex2, 2)
pinusValues_sunny = getValues(eval_pinus, eval_pinus2, 2)

acaciaValues_sunny$med
ulexValues_sunny$med
pinusValues_sunny$med

acaciaValues_sunny$cv
ulexValues_sunny$cv
pinusValues_sunny$cv

# Accuracies over the shadowed canopies
acaciaValues_shadow = getValues(eval_acacia, eval_acacia2, 3)
ulexValues_shadow= getValues(eval_ulex, eval_ulex2, 3)
pinusValues_shadow = getValues(eval_pinus, eval_pinus2, 3)

acaciaValues_shadow$med
ulexValues_shadow$med
pinusValues_shadow$med

acaciaValues_shadow$cv
ulexValues_shadow$cv
pinusValues_shadow$cv


################
library(corrplot)

##############################
### Sunny canipies validation 

AUC <- read.table("clipboard", header=T)
kappa <- read.table("clipboard", header=T)
TPR <- read.table("clipboard", header=T)
TNR <- read.table("clipboard", header=T)
# CV
AUC2 <- read.table("clipboard", header=T)
kappa2 <- read.table("clipboard", header=T)
TPR2 <- read.table("clipboard", header=T)
TNR2 <- read.table("clipboard", header=T)

### plot 
x11()
svg(file = "Figures/corrplot_median1.svg", width=12, height=6)
par(mfrow=c(1,4))
corrplot( as.matrix(AUC), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0.6, .9), cl.pos = "b")
corrplot( as.matrix(kappa), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(.35, .75), cl.pos = "b")
corrplot( as.matrix(TPR), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0.5, .9), cl.pos = "b")
corrplot( as.matrix(TNR), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0.5, .9), cl.pos = "b")
dev.off()

x11()
svg(file = "Figures/corrplot_cv.svg", width=12, height=6)
par(mfrow=c(1,4))
corrplot( as.matrix(AUC2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(1, 3.3), cl.pos = "b")
corrplot( as.matrix(kappa2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(4, 17), cl.pos = "b")
corrplot( as.matrix(TPR2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(4.5, 28), cl.pos = "b")
corrplot( as.matrix(TNR2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(2, 17.5), cl.pos = "b")
dev.off()

##############################
### Shadow canipies validation 

AUC <- read.table("clipboard", header=T)
kappa <- read.table("clipboard", header=T)
TPR <- read.table("clipboard", header=T)
TNR <- read.table("clipboard", header=T)
# CV
AUC2 <- read.table("clipboard", header=T)
kappa2 <- read.table("clipboard", header=T)
TPR2 <- read.table("clipboard", header=T)
TNR2 <- read.table("clipboard", header=T)

### plot 
x11()
svg(file = "Figures/corrplot_median_shadow.svg", width=12, height=6)
par(mfrow=c(1,4))
corrplot( as.matrix(AUC), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0.2, .9), cl.pos = "b")
corrplot( as.matrix(kappa), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0, .3), cl.pos = "b")
corrplot( as.matrix(TPR), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0.1, 1), cl.pos = "b")
corrplot( as.matrix(TNR), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(0, 1), cl.pos = "b")
dev.off()

x11()
svg(file = "Figures/corrplot_shadow_cv.svg", width=12, height=6)
par(mfrow=c(1,4))
corrplot( as.matrix(AUC2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(2, 13), cl.pos = "b")
corrplot( as.matrix(kappa2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(15, 205), cl.pos = "b")
corrplot( as.matrix(TPR2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(18, 115), cl.pos = "b")
corrplot( as.matrix(TNR2), method = "circle", is.corr=F, col=color(100), tl.col="black", 
          cl.lim = c(4, 225), cl.pos = "b")
dev.off()
##############################


## plot functions;  n: 1 = overall, 2 = sunny, 3 = shadows
plot_bean <- function(eval1, eval2, n, xlab="Kappa", ylim=c(0,1), ...){ 

  library(beanplot)
  # get accuracies
  getAcc_matrix <- function(eval, n){ 
    data = matrix(nrow = 100, ncol = length(eval))
    for (i in 1:length(eval)){
      data[,i] = eval[[i]][[n]] 
    }
    colnames(data) <-  c("rgb", "texture", "struct", "hyper", "strcttext", "structrgb",
                         "structhyper", "textrgb", "texthyper",
                         "structextrgb", "structexthyper")
    stack(as.data.frame(data)) 
  }
  
  data = data.frame(getAcc_matrix(eval1, n=n), getAcc_matrix(eval2, n=n)[[1]])
  names(data) = c("all", "label", "sunny")
  # Plot the accuracies distribution
  par( mai = c(1, 1.3, 0.5, 0.5) )
  beanplot(all ~ label, data=data, las=1, horizontal=T, side="first", ll=NA, beanlines="median", border = NA,
           col="darkolivegreen", what = c(FALSE, TRUE, TRUE, TRUE), xlab=xlab, ylim=ylim, log="")
  beanplot(sunny ~ label, data=data, las=1, horizontal=T, side="second", ll=NA, beanlines="median", border = NA,
           col="darkolivegreen1", what = c(FALSE, TRUE, TRUE, TRUE), add=T)
  grid()
  # plot median values 
  lines( x=aggregate(data$all, list(data$label), median)$x, y=seq(1,11,1), lty=1, lwd=2, las=2 )
  lines( x=aggregate(data$sunny, list(data$label), median)$x, y=seq(1,11,1), lty=2, lwd=2, las=2 )
  legend("bottomleft", legend = c("Overall", "Sunny"), fill=c("darkolivegreen", "darkolivegreen1"), 
         lty=c(1,2), lwd=2, bty="n")
}

## save plots
# AUC
svg(file = "Figures/AUC_acacia_.svg", width=7, height=6)
plot_bean(eval_acacia$AUC, eval_acacia2$AUC, n=1, xlab="AUC", ylim=c(0.6, 0.9))
dev.off()
svg(file = "Figures/AUC_ulex_all.svg", width=7, height=6)
plot_bean(eval_ulex$AUC, eval_ulex2$AUC, n=1, xlab="AUC", ylim=c(0.6, 0.9))
dev.off()
svg(file = "Figures/AUC_pinus_all.svg", width=7, height=6)
plot_bean(eval_pinus$AUC, eval_pinus2$AUC, n=1, xlab="AUC", ylim=c(0, 0.9))
dev.off()

# Kappa
svg(file = "Figures/Kappa_acacia_all.svg", width=7, height=6)
plot_bean(eval_acacia$Kappa, eval_acacia2$Kappa, n=1, xlab="Kappa", ylim=c(0, 0.6))
dev.off()
svg(file = "Figures/Kappa_ulex_all.svg", width=7, height=6)
plot_bean(eval_ulex$Kappa, eval_ulex2$Kappa, n=1, xlab="Kappa", ylim=c(0, 0.6))
dev.off()
svg(file = "Figures/Kappa_pinus_all.svg", width=7, height=6)
plot_bean(eval_pinus$Kappa, eval_pinus2$Kappa, n=1, xlab="Kappa", ylim=c(0, 0.6))
dev.off()

# TPR
svg(file = "Figures/TPR_acacia_all.svg", width=7, height=6)
plot_bean(eval_acacia$TPR, eval_acacia2$TPR, n=1, xlab="TPR", ylim=c(0.3, 1))
dev.off()
svg(file = "Figures/TPR_ulex_all.svg", width=7, height=6)
plot_bean(eval_ulex$TPR, eval_ulex2$TPR, n=1, xlab="TPR", ylim=c(0.3, 1))
dev.off()
svg(file = "Figures/TPR_pinus_all.svg", width=7, height=6)
plot_bean(eval_pinus$TPR, eval_pinus2$TPR, n=1, xlab="TPR", ylim=c(0.3, 1))
dev.off()

# TNR
svg(file = "Figures/TNR_acacia_all.svg", width=7, height=6)
plot_bean(eval_acacia$TNR, eval_acacia2$TNR, n=1, xlab="TNR", ylim=c(0.4, .9))
dev.off()
svg(file = "Figures/TNR_ulex_all.svg", width=7, height=6)
plot_bean(eval_ulex$TNR, eval_ulex2$TNR, n=1, xlab="TNR", ylim=c(0.4, .9))
dev.off()
svg(file = "Figures/TNR_pinus_all.svg", width=7, height=6)
plot_bean(eval_pinus$TNR, eval_pinus2$TNR, n=1, xlab="TNR", ylim=c(0.4, .9))
dev.off()


#############
# get variable importance 

GetVarImp <- function(pattern, pattern2="structexthyper", model="all"){ 
  setwd("H:/results2")
  # list of files matching the pattern
  if (model == "all"){
    listt <- list.files("models/all/", pattern=pattern, full.names = T)
    listt <- listt[ grep(pattern2, listt) ]
    load(listt[[1]])
    imp = matrix(nrow=length(listt), ncol=length(fit@results[grep("permutation", rownames(fit@results))]) )
    colnames(imp) = rownames(fit@results)[grep("permutation", rownames(fit@results))]
    for (i in 1:length(listt)){ 
      load(listt[[i]])
      p = fit@results[ grep("permutation", rownames(fit@results)) ]
      imp[i,] <- p
      }
  }
  if (model == "sunny"){
    listt <- list.files("models/sunny/", pattern=pattern, full.names = T) 
    listt <- listt[ grep(pattern2, listt) ]
    load(listt[[1]])
    imp = matrix(nrow=length(listt), ncol=length(fit2@results[grep("permutation", rownames(fit2@results))]) )
    colnames(imp) = rownames(fit2@results)[grep("permutation", rownames(fit2@results))]
    for (i in 1:length(listt)){ 
      load(listt[[i]])
      p = fit2@results[ grep("permutation", rownames(fit2@results)) ]
      imp[i,] <- p
      }
  }
  
  imp
}

imp_acacia <- GetVarImp(pattern="acacia", model="all")
imp_acacia2 <- GetVarImp(pattern="acacia", model="sunny")
imp_ulex <- GetVarImp(pattern="ulex", model="all")
imp_ulex2 <- GetVarImp(pattern="ulex", model="sunny")
imp_pinus <- GetVarImp(pattern="pinus", model="all")
imp_pinus2 <- GetVarImp(pattern="pinus", model="sunny") 

imp_acacia_dat = data.frame(var = colnames(imp_acacia),
                            imp_all = apply(imp_acacia, 2, FUN = median), 
                            imp_sunny = apply(imp_acacia2, 2, FUN = median),
                            imp_all_sd = apply(imp_acacia, 2, FUN = sd), 
                            imp_sunny_sd = apply(imp_acacia2, 2, FUN = sd),
                            type=c("Spectral","Spectral","Structure","Structure","Structure",
                                   "Structure","Texture","Texture","Texture","Texture","Texture"))
imp_acacia_dat$var <- gsub("*.permutation.importance","", rownames(imp_acacia_med) )

imp_ulex_dat = data.frame(var = colnames(imp_ulex),
                          imp_all = apply(imp_ulex, 2, FUN = median), 
                          imp_sunny = apply(imp_ulex2, 2, FUN = median),
                          imp_all_sd = apply(imp_ulex, 2, FUN = sd), 
                          imp_sunny_sd = apply(imp_ulex2, 2, FUN = sd),
                          type=c("Spectral","Spectral","Spectral","Spectral","Structure",
                                 "Structure","Texture","Texture","Texture","Texture"))
imp_ulex_dat$var <- gsub("*.permutation.importance","", rownames(imp_ulex_med))

imp_pinus_dat = data.frame(var = colnames(imp_pinus),
                           imp_all = apply(imp_pinus, 2, FUN = median), 
                           imp_sunny = apply(imp_pinus2, 2, FUN = median),
                           imp_all_sd = apply(imp_pinus, 2, FUN = sd), 
                           imp_sunny_sd = apply(imp_pinus2, 2, FUN = sd),
                           type=c("Spectral","Spectral","Spectral","Spectral",
                                  "Structure","Structure","Texture",
                                  "Texture","Texture","Texture") )
imp_pinus_dat$var <- gsub("*.permutation.importance","", rownames(imp_pinus_med) )

##############################
### plot varImp

library(ggplot2)
library(gridExtra)

x11()
p1 = ggplot(imp_acacia_dat, aes(x=var, y=imp_all))+
     labs(title = "A. dealbata overall", x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_all-imp_all_sd, ymax=imp_all+imp_all_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))

p2 = ggplot(imp_acacia_dat, aes(x=var, y=imp_sunny))+
     labs(title="A. dealbata sunny",x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_sunny-imp_sunny_sd, ymax=imp_sunny+imp_sunny_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))


p3 = ggplot(imp_ulex_dat, aes(x=var, y=imp_all))+
     labs(title="U. europaeus overall", x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_all-imp_all_sd, ymax=imp_all+imp_all_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))


p4 = ggplot(imp_ulex_dat, aes(x=var, y=imp_sunny))+
     labs(title = "U. europaeus sunny", x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_sunny-imp_sunny_sd, ymax=imp_sunny+imp_sunny_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))

p5 = ggplot(imp_pinus_dat, aes(x=var, y=imp_all))+
     labs(title="P. radiata overall", x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_all-imp_all_sd, ymax=imp_all+imp_all_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))


p6 = ggplot(imp_pinus_dat, aes(x=var, y=imp_sunny))+
     labs(title = "P. radiata sunny", x="Variables", y = "Importance")+
     geom_bar(stat="identity", aes(fill = type))+
     scale_fill_brewer(palette="Greens") + theme_classic()+
     geom_errorbar(aes(ymin=imp_sunny-imp_sunny_sd, ymax=imp_sunny+imp_sunny_sd), width=.2,
                   position=position_dodge(.9))+
     theme(axis.text.x = element_text(angle = 25, hjust = 1),axis.text.y=element_text(size=14),
           text=element_text(size=15), plot.title = element_text(size=18, hjust = .5))

out <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2) 
                                                                
ggsave("Figures/varImport.svg", out, width = 10, height = 15)

###############################
## significance test

# x: type of evaluation (1=overall; 2=sunny; 3=shadows)
significanceTest <- function(model1, model2, x){
  
  listNames <- c("rgb", "texture", "struct", "hyper", "strcttext", "structrgb",
                 "structhyper", "textrgb", "texthyper",
                 "structextrgb", "structexthyper")
  outall <- list()
  for (j in 1:length(listNames)){
    # bootstrap pair diferences
    AUC <- model2$AUC[[j]][[x]] - model1$AUC[[j]][[x]]
    kappa <- model2$Kappa[[j]][[x]] - model1$Kappa[[j]][[x]]
    TPR <- model2$TPR[[j]][[x]] - model1$TPR[[j]][[x]]
    TNR <- model2$TNR[[j]][[x]] - model1$TNR[[j]][[x]]
    
    # prepare output
    output <- list(AUC, kappa, TPR, TNR)
    
    # matrix of significances
    a = matrix(nrow = length(output), ncol = 3)
    colnames(a) <- c("0.1", "0.05", "0.001")
    rownames(a) <- c("AUC", "kappa", "TPR", "TNR")
    for (i in 1:nrow(a)){
      # 0.1
      if ( sign(quantile(output[[i]], probs=c(0.1))) == sign(quantile(output[[i]], probs=c(0.9))) ){
        a[i,1] = "True"
      } else{
        a[i,1] = "False"
      }
      # 0.05
      if ( sign(quantile(output[[i]], probs=c(0.05))) == sign(quantile(output[[i]], probs=c(0.95))) ){
        a[i,2] = "True"
      } else{
        a[i,2] = "False"
      }
      # 0.001
      if ( sign(quantile(output[[i]], probs=c(0.001))) == sign(quantile(output[[i]], probs=c(0.995))) ){
        a[i,3] = "True"
      } else{
        a[i,3] = "False"
      }
    }
    outall[[j]] <- a
  }
  names(outall) <- listNames
  outall
}

## differences at sunny canopies
significanceTest(eval_acacia, eval_acacia2, 2)
significanceTest(eval_ulex, eval_ulex2, 2)
significanceTest(eval_pinus, eval_pinus2, 2)

## differences at shadowed canopies
significanceTest(eval_acacia, eval_acacia2, 3)
significanceTest(eval_ulex, eval_ulex2, 3)
significanceTest(eval_pinus, eval_pinus2, 3)

### significance test between models
combn(seq(1,11,1),2)
significance_models <- function(model, x){ 
  # see all possible combinations
  combinations = combn(seq(1,11,1),2)
  
  outall <- list()
  for (j in 1:ncol(combinations)){
    # bootstrap pair diferences
    AUC <- model$AUC[[combinations[,j][1]]][[x]] - model$AUC[[combinations[,j][2]]][[x]]
    kappa <- model$Kappa[[combinations[,j][1]]][[x]] - model$Kappa[[combinations[,j][2]]][[x]]
    TPR <- model$TPR[[combinations[,j][1]]][[x]] - model$TPR[[combinations[,j][2]]][[x]]
    TNR <- model$TNR[[combinations[,j][1]]][[x]] - model$TNR[[combinations[,j][2]]][[x]]
    
    # prepare output
    output <- list(AUC, kappa, TPR, TNR)
    
    # matrix of significances
    a = matrix(nrow = length(output), ncol = 3)
    colnames(a) <- c("0.1", "0.05", "0.001")
    rownames(a) <- c("AUC", "kappa", "TPR", "TNR")
    for (i in 1:nrow(a)){
      # 0.1
      if ( sign(quantile(output[[i]], probs=c(0.1))) == sign(quantile(output[[i]], probs=c(0.9))) ){
        a[i,1] = "True"
      } else{
        a[i,1] = "False"
      }
      # 0.05
      if ( sign(quantile(output[[i]], probs=c(0.05))) == sign(quantile(output[[i]], probs=c(0.95))) ){
        a[i,2] = "True"
      } else{
        a[i,2] = "False"
      }
      # 0.001
      if ( sign(quantile(output[[i]], probs=c(0.001))) == sign(quantile(output[[i]], probs=c(0.995))) ){
        a[i,3] = "True"
      } else{
        a[i,3] = "False"
      }
    }
    outall[[j]] <- a
  }
  auc_sig = matrix(nrow=ncol(combinations), ncol=6); colnames(auc_sig)=c("AUC", "Kappa", "TPR", "TNR", "model1", "model2")
  for (i in 1:55){
    # AUC
    if (outall[[i]][1,1] == "True") auc_sig[i,1]<-1
    if (outall[[i]][1,2] == "True") auc_sig[i,1]<-2
    if (outall[[i]][1,3] == "True") auc_sig[i,1]<-3
    # kappa
    if (outall[[i]][2,1] == "True") auc_sig[i,2]<-1
    if (outall[[i]][2,2] == "True") auc_sig[i,2]<-2
    if (outall[[i]][2,3] == "True") auc_sig[i,2]<-3
    # TPR
    if (outall[[i]][3,1] == "True") auc_sig[i,3]<-1
    if (outall[[i]][3,2] == "True") auc_sig[i,3]<-2
    if (outall[[i]][3,3] == "True") auc_sig[i,3]<-3
    # TNR
    if (outall[[i]][4,1] == "True") auc_sig[i,4]<-1
    if (outall[[i]][4,2] == "True") auc_sig[i,4]<-2
    if (outall[[i]][4,3] == "True") auc_sig[i,4]<-3
    # models
    auc_sig[i,5]<-combinations[,i][1]
    auc_sig[i,6]<-combinations[,i][2]
  }
  auc_sig
}

acacia_models_test <- significance_models(eval_acacia2, 2)
ulex_models_test <- significance_models(eval_ulex2, 2)
pinus_models_test <- significance_models(eval_pinus2, 2)

pinus_models_test[grep("10", acacia_models_test[,5]),]


###############################
## maps

listt <- list.files("H:/results2/preds", full.names = T)  

list_acacia <- listt[ grep("acacia", listt) ]
list_ulex <- listt[ grep("ulex", listt) ]
list_pinus <- listt[ grep("pinus", listt) ]

listNames <- c("rgb", "texture", "struct", "hyper", "strcttext", "structrgb",
               "structhyper", "textrgb", "texthyper",
               "structextrgb", "structexthyper")

loadRaster <- function(object){
  load(object) 
  beginCluster(2)
  med1 <- clusterR( stack(pred_out[[1]]), function(x) calc(x, median) )
  med2 <- clusterR( stack(pred_out[[2]]), function(x) calc(x, median) )
  cv1 <-  clusterR( stack(pred_out[[1]]), function(x) calc(x, cv) )
  cv2 <-  clusterR( stack(pred_out[[2]]), function(x) calc(x, cv) )
  endCluster()
  out <- list(med1, med2, cv1, cv2)
  names(out) <- c("median_all", "median_sunny", "CV_all", "CV_sunny")
  out
}

### acacia
acacia_med_img_all <- list()
acacia_cv_img_all <- list()
acacia_med_img_sunny <- list()
acacia_cv_img_sunny <- list()

for (i in 1:length(listNames)){
  img <- loadRaster( list_acacia[ grep(paste0("/",listNames[[i]],"_"), list_acacia) ] )
  acacia_med_img_all[[i]] <- img$median_all
  acacia_med_img_sunny[[i]] <- img$median_sunny
  acacia_cv_img_all[[i]] <- img$CV_all
  acacia_cv_img_sunny[[i]] <- img$CV_sunny
}
names(acacia_med_img_all) <- listNames
names(acacia_cv_img_all)  <- listNames
names(acacia_med_img_sunny) <- listNames
names(acacia_cv_img_sunny) <- listNames

### ulex
ulex_med_img_all <- list()
ulex_cv_img_all <- list()
ulex_med_img_sunny <- list()
ulex_cv_img_sunny <- list()

for (i in 1:length(listNames)){
  img <- loadRaster( list_ulex[ grep(paste0("/",listNames[[i]],"_"), list_ulex) ] )
  ulex_med_img_all[[i]] <- img$median_all
  ulex_med_img_sunny[[i]] <- img$median_sunny
  ulex_cv_img_all[[i]] <- img$CV_all
  ulex_cv_img_sunny[[i]] <- img$CV_sunny
}
names(ulex_med_img_all) <- listNames
names(ulex_cv_img_all)  <- listNames
names(ulex_med_img_sunny) <- listNames
names(acacia_cv_img_sunny) <- listNames

### acacia
pinus_med_img_all <- list()
pinus_cv_img_all <- list()
pinus_med_img_sunny <- list()
pinus_cv_img_sunny <- list()

for (i in 1:length(listNames)){
  img <- loadRaster( list_pinus[ grep(paste0("/",listNames[[i]],"_"), list_pinus) ] )
  pinus_med_img_all[[i]] <- img$median_all
  pinus_med_img_sunny[[i]] <- img$median_sunny
  pinus_cv_img_all[[i]] <- img$CV_all
  pinus_cv_img_sunny[[i]] <- img$CV_sunny
}
names(pinus_med_img_all) <- listNames
names(pinus_cv_img_all)  <- listNames
names(pinus_med_img_sunny) <- listNames
names(pinus_cv_img_sunny) <- listNames

save.image("Analysis.RData")

###############################
## Obtain binary maps accordint to Kappa's threshold

getBinaryClassification <- function(pattern, model="all"){ 
  # list of files matching the pattern
  if (model == "all"){
    list_models <- list.files("H:/results2/eval/all/", pattern=pattern, full.names = T) 
  }
  if (model == "sunny"){
    list_models <- list.files("H:/results2/eval/sunny/", pattern=pattern, full.names = T) 
  }
  list_models <- list_models [ grep("sunny", basename(list_models)) ] 

  # obtain thresholds
  thr <- numeric()
  for (i in 1:length(list_models)){
    load(list_models[[i]])
    thr[i] <- threshold(eval_sunny)[[1]]
  }
  
  # apply threshold to the predictions
  list_img = list.files("H:/results2/preds/", full.names = T)
  img = grep(pattern, list_img)
  load(list_img[[img]])
  img_stack = list()
  for (i in 1:100){ 
    if (model == "all"){
      pred = pred_out[[1]][[i]]
      pred[ pred < thr[i] ] <- 0; pred[ pred > thr[i] ] <- 1
      img_stack[[i]] <- pred
    }
    if (model == "sunny"){
      pred = pred_out[[2]][[i]]
      pred[ pred < thr[i] ] <- 0; pred[ pred > thr[i] ] <- 1
      img_stack[[i]] <- pred
    }
  }
  rm(pred_out)
  img_stack <- stack(img_stack)
  img_summe <- calc(img_stack, fun = sum)

  return(img_summe)
  
}


thr_acacia <- getBinaryClassification("structextrgb_acacia_f1", model="all")
thr_ulex   <- getBinaryClassification("hyper_ulex_f3", model="all")
thr_pinus  <- getBinaryClassification("structextrgb_pinus_f8", model="all")

thr_acacia2 <- getBinaryClassification("structextrgb_acacia_f1", model="sunny")
thr_ulex2   <- getBinaryClassification("hyper_ulex_f3", model="sunny")
thr_pinus2  <- getBinaryClassification("structextrgb_pinus_f8", model="sunny")


writeRaster(thr_acacia, filename = "H:/results2/bin_acacia_all.tif", format="GTiff", overwrite=T)
writeRaster(thr_acacia2, filename = "H:/results2/bin_acacia_sunny.tif", format="GTiff", overwrite=T)

writeRaster(thr_ulex, filename = "H:/results2/bin_ulex_all.tif", format="GTiff", overwrite=T)
writeRaster(thr_ulex2, filename = "H:/results2/bin_ulex_sunny.tif", format="GTiff", overwrite=T)

writeRaster(thr_pinus, filename = "H:/results2/bin_pinus_all.tif", format="GTiff", overwrite=T)
writeRaster(thr_pinus2, filename = "H:/results2/bin_pinus_sunny.tif", format="GTiff", overwrite=T)
