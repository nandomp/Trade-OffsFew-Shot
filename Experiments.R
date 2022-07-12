# ------------------------------------------------------------------------------------------------
# Packages ---------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(plotly)
library(reshape2)
library(xtable)
library(ggrepel)
library(data.table)
library(ggridges)
library(latex2exp)
library(ggnewscale)
library(geometry) 

options(scipen=999)

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}

# ------------------------------------------------------------------------------------------------
# Data/results -----------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

reli <- readRDS("training_complete_2AUG21.RDS")

head(reli)
#             problem domain  engine nshot threshold                   query   target output         prob correct incorrect reject
# 1: addPunctuation-1  dates davinci     0         0 Input: 290386\\nOutput: 29-03-86 290104 0.0008881487       0         1      0
# 2: addPunctuation-1  dates davinci     0         0 Input: 250374\\nOutput: 25-03-74 250375 0.0790378712       0         1      0
# 3: addPunctuation-1  dates davinci     0         0 Input: 170615\\nOutput: 17-06-15 170616 0.2326278483       0         1      0
# 4: addPunctuation-1  dates davinci     0         0 Input: 170905\\nOutput: 17-09-05 170905 0.2391367844       0         1      0
# 5: addPunctuation-1  dates davinci     0         0 Input: 241206\\nOutput: 24-12-06 241206 0.0750996451       0         1      0
# 6: addPunctuation-1  dates davinci     0         0 Input: 290607\\nOutput: 29-06-07 290600 0.0054967851       0         1      0



# ------------------------------------------------------------------------------------------------
# 1. Explore if these problems are solvable with language models in a few-shot fashion and 
# determine whether there is a saturation point in the number of supplied examples.
# Summary table of results for increasing values of $n_s$ (just wright/wrong), with no inspection. 
# Here we determine that $n_s=10$ is enough.
# ------------------------------------------------------------------------------------------------


agg.reli <- reli %>% filter(threshold == 0, engine == "davinci") %>% group_by(problem, domain, engine, nshot) %>% summarise(acc = mean(correct))
agg.reli$acc <- round(agg.reli$acc,2)

# Table 

table1 <- agg.reli %>% select(domain, problem, nshot, nshot, acc) %>% dcast(domain+ problem ~ nshot) 
print(xtable(table1), include.rownames = FALSE)
colMeans(table1[,-c(1,2)])
write.csv(table1, file = "Results.Table1_extended.csv", row.names = F)

table1s <- agg.reli %>% group_by(domain, nshot) %>% summarise(Acc = mean(acc), sd = sd(acc))
table1s <- as.data.frame(table1s)  
table1s$Acc <- round(table1s$Acc,2) 
table1s$sd <- round(table1s$sd,2) 
table1s$AccSD <- paste0(table1s$Acc,"±",table1s$sd)
table1s.d <- table1s %>% select(domain, nshot, AccSD) %>% dcast(domain ~ nshot)

print(xtable(table1s.d), include.rownames = FALSE)
write.csv(table1s.d, file = "Results.Table1_short.csv", row.names = F)


library(latex2exp)
# install.packages("latex2exp")

Exp1.plot <- ggplot(table1s, aes(nshot, Acc, colour = domain)) + 
  
  geom_ribbon(aes(ymin = Acc - sd, ymax = Acc + sd, fill = domain), alpha = 0.05, colour = "white") + 
  geom_line(size = 2, alpha = .8) +
  geom_point(size = 2) +  
  geom_label_repel(data =  filter(table1s, nshot == "10"), aes(nshot, Acc, label = domain),
                   size = 6,
                   nudge_x = 1.1,
                   hjust = "right",
                   direction = "y",
                   na.rm = TRUE, 
                   alpha = 0.8, 
                   show.legend = F) + 
  ylab("Accuracy") + xlab(TeX("n_s")) + 
  # facet_grid(domain ~.) +
  scale_x_continuous(breaks = c(seq(0,10,1)), labels = c(seq(0,10,1)), expand = c(0.01, 0.8)) +
  scale_colour_viridis_d() + 
  theme_minimal() + 
  theme(legend.position = "none",
        strip.text = element_text(
          size = 18, color = "black", face = "bold"),
        axis.text  = element_text(size = 16),
        axis.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16))


openPDFEPS("Exp1.plot",width=10,height=4)
print(Exp1.plot)
dev.off()


# ------------------------------------------------------------------------------------------------
# (2) Study whether the number of examples provided to the model affects not only the accuracy of 
# the outputs but also their confidence ($\hat{p}$), so there is a trade-off between $n_s$ vs $n_c$ 
# and $n_w$.This should be some more complete analysis (a summary table and perhaps 
# all plots) than just Fig.\ref{fig:rejectExample
# ------------------------------------------------------------------------------------------------

reli <- readRDS("training_complete_2AUG21.RDS")
colnames(reli)


plotAreas <- function(reli, en = "davinci", do = "all", ns = c(0:10), w = 10, h = 7, legend = "bottom", ncol = 2){
  
  reli.agg <- reli %>% group_by(threshold, domain, engine, nshot) %>% summarise(correct = sum(correct), incorrect = sum(incorrect), reject = sum(reject))
  reli.agg.m <- reli.agg %>% melt(id.vars = c("threshold", "domain", "engine", "nshot"))
  
  # responses <- c("#f5cac3","#f28482","#84a59d", "black")
  # responses <- c("#5ac6c0","#e85f9f","#284b63", "#ffe4eb") # reject, wrong, accept, s
  responses <- c("#cfdbd5","#7F0101","#017F01", "#01017F") # reject, wrong, accept, s
  # responses <- c("#d3d3d3","#9e2a2b","#57cc99", "#22577a") # reject, wrong, accept, s
  
  
  
  # reli.agg.mf <- reli.agg.m %>% filter(engine == "davinci", nshot %in% c(1:4))
  # reli.agg.mf <- reli.agg.m %>% filter(engine == "davinci", domain == "dates", nshot %in% c(1:4))
  # reli.agg.mf <- reli.agg.m %>% filter(engine == "davinci", nshot %in% c(1:4))
  
  if(do == "all"){
    reli.agg.mf <- reli.agg.m %>% filter(engine == en, nshot %in% ns)
    
  }else{
    reli.agg.mf <- reli.agg.m %>% filter(engine == en, domain == do, nshot %in% ns)
    
  }
  
  reli.agg.mf$variable <- factor(as.character(reli.agg.mf$variable), levels = c("reject", "incorrect", "correct"))
  levels(reli.agg.mf$variable) <- c("r", "w", "a")
  
  
  # bands n_s
  diffExs <-  reli.agg.mf %>% group_by(domain, threshold, nshot) %>% summarise(tot = sum(value)) 
  oneshotexs <- select(data.frame(diffExs), domain, nshot, tot) %>% unique() %>% group_by(domain) %>% summarise(n = tot[1]-tot[2])
  print(diffExs)
  
  threshold = unique(reli.agg.mf$threshold)
  domain = unique(reli.agg.mf$domain)    
  engine = unique(reli.agg.mf$engine)    
  nshot = unique(reli.agg.mf$nshot)    
  variable = "s"    
  df <-expand.grid(threshold = threshold, domain  = domain, engine = engine, nshot = nshot, variable = variable)
  df <- merge(df, oneshotexs)
  df$value <- df$nshot * df$n
  reli.agg.mf.bands <- rbind(reli.agg.mf, select(df, -n))
  
  
  
  
  reli.agg.mf.bands$nshotB <- factor(reli.agg.mf.bands$nshot)
  sh <- c("0-shot", "1-shot", "2-shot", "3-shot", "4-shot", "5-shot", "6-shot", "7-shot", "8-shot", "9-shot", "10-shot")
  levels(reli.agg.mf.bands$nshotB) <- sh[ns+1]
  
  
  reliP <- ggplot(reli.agg.mf.bands, aes(threshold, value, fill = variable)) + 
    geom_area(position = "fill", alpha = 0.7) 
  
  if(do == "all"){
    reliP <- reliP + facet_grid(domain ~ nshotB)
  }else{
    reliP <- reliP + facet_wrap(nshotB ~ ., ncol = ncol)
  }
  reliP <- reliP + 
    scale_fill_manual("", values = responses) + 
    ylab("Percentage") +
    scale_x_continuous(breaks = c(0,0.5,1), labels = c(0,0.5,1)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1)) +
    theme_minimal() + 
    theme(legend.position = legend,
          strip.text = element_text(
            size = 18, color = "black", face = "bold"),
          axis.text  = element_text(size = 18),
          axis.title = element_text(size = 18,face = "bold"),
          legend.text = element_text(size = 18), 
          legend.background = element_rect(fill = "white"),
          legend.title = element_blank())
  
  # openPDFEPS(paste0("DW_reliability_",en,"_",do),width=w,height=h)
  return(list(reliP,reli.agg.mf.bands))
  # dev.off()
  
}

unique(reli$domain)
# plotAreas(reli, do = "emails", ns = c(1:4), w = 10, h = 7, legend = c(0.93, 0.85), ncol = 4)
# plotAreas(reli, do = "names", ns = c(1:4), w = 10, h = 7, legend = c(0.93, 0.85))
# plotAreas(reli, do = "names", ns = c(1:6), w = 15, h = 10, legend = c(0.93, 0.85), ncol = 3)

openPDFEPS(paste0("DW_reliability_daVinci_Emails"),width=12,height=4)
plotAreas(reli, do = "emails", ns = c(1:4), w = 10, h = 7, legend = c(0.93, 0.85), ncol = 4)
dev.off()

openPDFEPS(paste0("Exp2.plot"),width=24,height=26)
plotAreas(reli, do = "all", ns = c(0:10), legend = "bottom")[[1]]
dev.off()





df <- plotAreas(reli, do = "all", ns = c(0:10), legend = "bottom")[[2]]
# maxV <- df %>% group_by(domain, engine, nshot, nshotB, variable) %>% summarise(maxValue = max(value))
# temp <- merge(df,maxV)
# temp$valueP <- temp$value/temp$maxValue
# minsT <- temp[which(temp$variable == "w" & temp$valueP <= 0.25),] %>% group_by(domain, engine, nshot, variable) %>% summarise(minT = min(threshold))
# minsT <- df[which(df$variable == "w" & df$value <= 5),] %>% group_by(domain, engine, nshot, variable) %>% summarise(minT = min(threshold))


dfc <- dcast(as.data.frame(df),threshold + domain +engine + nshot + nshotB ~variable)
dfc$aP <- (dfc$a / (dfc$a + dfc$w + dfc$r))
dfc$wP <- (dfc$w / (dfc$a + dfc$w + dfc$r))
dfc$rP <- (dfc$r / (dfc$a + dfc$w + dfc$r))

# FACETS by threshold, all domains
dfc %>% filter(!(threshold == 1)) %>% 
  ggplot(aes(nshot, aP, colour = domain)) + 
  geom_line(size = 2) + 
  facet_wrap(threshold ~.) +
  scale_x_continuous(breaks = c(seq(0,10,1)), labels = c(seq(0,10,1)), expand = c(0.01, 0.8)) +
  scale_colour_viridis_d() + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        strip.text = element_text(
          size = 18, color = "black", face = "bold"),
        axis.text  = element_text(size = 16),
        axis.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16))


# Fixed threshold, all domains
dfc %>% filter(threshold == 0.45) %>% 
  # filter(domain == "dates") %>%
  ggplot(aes(nshot, aP, colour = domain, fill = domain, group=interaction(domain, threshold))) + 
  geom_line(size = 2, alpha = 0.9) + 
  # geom_smooth(aes(group = domain), alpha = 0.1, size = 2) +
  
  scale_x_continuous(breaks = c(seq(0,10,1)), labels = c(seq(0,10,1)), expand = c(0.01, 0.8)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() + 
  
  theme_minimal() + 
  theme(legend.position = "bottom",
        strip.text = element_text(
          size = 20, color = "black", face = "bold"),
        axis.text  = element_text(size = 20),
        axis.title = element_text(size = 20,face = "bold"),
        legend.text = element_text(size = 18))


#Fixed domain, al thresholds
dfc3 <- dfc %>% filter(domain == "dates" ) %>% 
  # filter(domain == "dates") %>%
  ggplot(aes(nshot, aP, group=interaction(domain, threshold)
             )) + 
  geom_line(aes(colour = threshold, linetype = "Accurate"), size = 2, alpha = 0.75) + 
  # geom_smooth(aes(group = 1), se = F, linetype = "dashed") +
  scale_colour_viridis_c(option = "plasma") +
  
  # new_scale_color() +
  # geom_line(aes(y = rP, colour = threshold, linetype = "Reject"), size = 2, alpha = 0.15) + 
  # # geom_smooth(aes(y = rP,group = 1), se = F, linetype = "dashed") +
  # scale_colour_viridis_c(option = "cividis") +
  
  new_scale_color() +
  geom_line(aes(y = wP, colour = threshold, linetype = "Wrong"), size = 2, alpha = 0.75) + 
  # geom_smooth(aes(y = wP,group = 1), se = F, linetype = "dashed") +
  scale_colour_viridis_c(option = "plasma") +
  
  # geom_boxplot() + 
  # geom_smooth(aes(group = domain), alpha = 0.1, size = 2) +
  
  scale_linetype("category", labels = c(TeX("n_a"), TeX("n_w"))) + 
  scale_x_continuous(breaks = c(seq(0,10,1)), labels = c(seq(0,10,1)), expand = c(0.01, 0.8)) +

  # scale_fill_viridis() + 
  xlab(TeX("n_s")) + ylab ("Percentage") + 
  theme_minimal() + 
  theme(legend.position = c(0.88,0.35),
        strip.text = element_text(
          size = 18, color = "black", face = "bold"),
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20,face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))


openPDFEPS(paste0("Exp2.plot1B"),width=12,height=5)
print(dfc3)
dev.off()








head(reli)
reli.agg.probs <- reli %>% filter(threshold == 0, engine == "davinci") %>% group_by(domain, engine, nshot, problem) %>% summarise(Prob = mean(prob), sd = sd(prob))


Exp2B <- ggplot(reli.agg.probs, aes(x = Prob, y = nshot, colour = domain, fill = domain)) +
  # geom_boxplot() + 
  geom_density_ridges(scale = 2, alpha = 0.7, show.legend = T) +
  facet_grid(nshot ~ ., scales = "free")  +
  scale_fill_viridis_d("") +
    scale_colour_viridis_d("") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))  +
    ylab("") + xlab("Confidence")  +
    guides(fill=guide_legend(ncol=1)) +
    theme_minimal() +
    theme(legend.position = c(0.07,0.18),
          strip.text = element_text(
            size = 14, color = "black", face = "bold"),
          axis.text.x  = element_text(size = 18),
          axis.text.y  = element_text(size = 8),
          axis.title = element_text(size = 18,face = "bold"),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 14))


openPDFEPS(paste0("Exp2.plot2"),width=10,height=6)
print(Exp2B)
dev.off()


# ggplot(reli.agg.probs, aes(x = Prob, y = domain, fill = domain, colour = domain)) +
#   # geom_boxplot() + 
#   geom_density_ridges(scale = 2, alpha = 0.7, show.legend = F) +
#   facet_grid(nshot ~ ., scales = "free") +
#   scale_fill_viridis_d(option = "plasma") +
#   scale_colour_viridis_d(option = "plasma") +
#   ylab("") + xlab("Difficulty")  +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         strip.text = element_text(
#           size = 14, color = "black", face = "bold"),
#         axis.text  = element_text(size = 14),
#         axis.title = element_text(size = 14,face = "bold"),
#         legend.text = element_text(size = 14))




# ------------------------------------------------------------------------------------------------
# (3) Determine how close the static and dynamic algorithms can get to the optimal cost.
# A combination of plots and a summary table with the costs being aggregated in a 
# uniform way (after the transformation of axes?). We will use static (perhaps with a couple of 
# different $\alpha$) and the dynamic (with $s_^*=\bot$ y $s_^*=10$)} and perhaps a fixed method
#  (e.g., $n_s = 5$ and $t_i$ inspecting half of the examples independently of the cost).+¡
# ------------------------------------------------------------------------------------------------

h <- function(x){
  val <- 1-2.718281^(-x)
  return(val)
}


getVolume=function(df) {
  #find triangular tesselation of (x,y) grid
  res=delaunayn(as.matrix(df[,-3]),full=TRUE,options="Qz")
  #calulates sum of truncated prism volumes
  sum(mapply(function(triPoints,A) A/3*sum(df[triPoints,"z"]),
             split.data.frame(res$tri,seq_along(res$areas)),
             res$areas))
}

res2plot <- readRDS("res2plot010_FInal.RDS")
res2plot <- res2plot[-1,]

res2plot$hci <- h(res2plot$ci)
res2plot$hcs <- h(res2plot$cs)

problem_ <- unique(reli$problem)

volTasks <- data.frame(problem = NA, volOpt = NA, volStat = NA, volDelta = NA, volPhi = NA)
for(task in problem_){
  
  fdf <- filter(res2plot, problem == task)
  
  dfQ <- data.frame(x = fdf$hci, y = fdf$hcs, z = fdf$qOptimal)
  volOpt <- getVolume(dfQ)
  
  dfStatic <- data.frame(x = fdf$hci, y = fdf$hcs, z = fdf$qStatic)
  volStat <- getVolume(dfStatic)
  
  dfDelta <- data.frame(x = fdf$hci, y = fdf$hcs, z = fdf$qDelta)
  volDelta <- getVolume(dfDelta)
  
  dfPhi <- data.frame(x = fdf$hci, y = fdf$hcs, z = fdf$qPhi)
  volPhi <- getVolume(dfPhi)
  
  volTasks <- rbind(volTasks, c(task, volOpt, volStat, volDelta, volPhi))
  
}

volTasks$volOpt <- as.numeric(volTasks$volOpt)
volTasks$volStat <- as.numeric(volTasks$volStat)
volTasks$volDelta <- as.numeric(volTasks$volDelta)-1
volTasks$volPhi <- as.numeric(volTasks$volPhi)
colMeans(volTasks[,-1], na.rm = T)



volTasks <- volTasks[-1,]

head(reli)
reliDP <- unique(select(reli, domain, problem))
volTasks.all <- merge(reliDP, volTasks)


volTasks.all.agg <- volTasks.all %>% group_by(domain) %>% summarise(volOpt.m = mean(volOpt, na.rm = T),  volOpt.sd = sd(volOpt, na.rm = T),
                                                                    volStat.m = mean(volStat, na.rm = T),  volStat.sd = sd(volStat, na.rm = T),
                                                                    volDelta.m = mean(volDelta, na.rm = T), volDelta.sd = sd(volDelta, na.rm = T),
                                                                    volPhi.m = mean(volPhi, na.rm = T), volPhi.sd = sd(volPhi, na.rm = T))


m <- select(volTasks.all.agg, domain, volOpt.m , volStat.m , volDelta.m , volPhi.m )
s <- select(volTasks.all.agg, domain, volOpt.sd , volStat.sd , volDelta.sd , volPhi.sd )
library(reshape2)
mm <- melt(m, id.vars = c("domain"))
sm <- melt(s, id.vars = c("domain"))

c2p <- data.frame(domain = mm$domain, variable = mm$variable, mean = mm$value, sd = sm$value)
levels(c2p$variable) <- c("Optimal","Static","Dynamic", "Fixed")

Exp3A <-ggplot(c2p, aes(domain, mean, fill = variable)) +
  geom_col(position = "dodge", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, colour = variable), width=0.5, size = 1,
                position=position_dodge(.9)) +
  geom_text(aes(y = 1.00*mean,label=round(mean,2)), vjust=0, hjust = -0.25, color="black", size=5,  position = position_dodge(0.9), angle = 90)+
  
  ylab("Mean Expected Cost") + xlab("") + 
  scale_fill_viridis_d("", option = "cividis") + 
  scale_colour_viridis_d("", option = "cividis") + 
  
  theme_minimal() +
  theme(legend.position = c(0.15,0.9),
        strip.text = element_text(
          size = 14, color = "black", face = "bold"),
        axis.text.x  = element_text(size = 18),
        axis.text.y  = element_text(size = 18),
        axis.title = element_text(size = 18,face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))
  
openPDFEPS(paste0("Exp3.plot1"),width=10,height=5)
print(Exp3A)
dev.off()


  
  
avgCostDom <- data.frame(domain = volTasks.all.agg$domain,
                           Optimal = paste0(round(volTasks.all.agg$volOpt.m,2), "±", round(volTasks.all.agg$volOpt.sd,2)),
                           Static = paste0(round(volTasks.all.agg$volStat.m,2), "±", round(volTasks.all.agg$volStat.sd,2)),
                           Dynamic = paste0(round(volTasks.all.agg$volDelta.m,2), "±", round(volTasks.all.agg$volDelta.sd,2)),
                           Fixed = paste0(round(volTasks.all.agg$volPhi.m,2), "±", round(volTasks.all.agg$volPhi.sd,2)))

volTasks.all$volOpt <- round(volTasks.all$volOpt,2)
volTasks.all$volStat <- round(volTasks.all$volStat,2)
volTasks.all$volDelta <- round(volTasks.all$volDelta,2)
volTasks.all$volPhi <- round(volTasks.all$volPhi,2)

volTasks.all <- volTasks.all[order(volTasks.all$domain),]

write.csv(as.data.frame(select(volTasks.all, domain, problem, volOpt, volStat, volDelta, volPhi)), file = "Exp3.data.csv", row.names = F)

colMeans(volTasks.all[,-c(1,2)])



# ------------------------------------------------------------------------------------------------
# (4) Derive the forms and use reasonable cost distributions for those cases where 
# the actual costs are not known, and study how results differ from the uniform case.
# The same as in 3 but for these cost distributions. We could also use for all the 
#   test takers compare optimal, static and dynamic variants
# ------------------------------------------------------------------------------------------------



res2plot <- readRDS("res2plot_HUMANS_31ERRORS.RDS")

res2plot <- res2plot[-1,]
nrow(res2plot)



res2plot.agg <- res2plot %>% group_by(domain) %>% summarise(Opt.m = mean(qOptimal, na.rm = T),  Opt.sd = sd(qOptimal, na.rm = T),
                                                                    Stat.m = mean(qStatic, na.rm = T),  Stat.sd = sd(qStatic, na.rm = T),
                                                                    Delta.m = mean(qDelta, na.rm = T), Delta.sd = sd(qDelta, na.rm = T),
                                                                    Phi.m = mean(qPhi, na.rm = T), Phi.sd = sd(qPhi, na.rm = T))



m <- select(res2plot.agg, domain, Opt.m , Stat.m , Delta.m , Phi.m )
s <- select(res2plot.agg, domain, Opt.sd , Stat.sd , Delta.sd , Phi.sd )
library(reshape2)
mm <- melt(m, id.vars = c("domain"))
sm <- melt(s, id.vars = c("domain"))

c2pH <- data.frame(domain = mm$domain, variable = mm$variable, mean = mm$value, sd = sm$value)
levels(c2pH$variable) <- c("Optimal","Static","Dynamic", "Fixed")



Exp4 <- ggplot(c2pH, aes(domain, mean, fill = variable)) +
  geom_col(position = "dodge", alpha = 0.6) + 
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, colour = variable), width=0.5, size = 1,
                # position=position_dodge(.9)) +
  geom_text(aes(y = 1.00*mean,label=paste(round(mean,2),"±",round(sd,1)), colour = variable),
            vjust=-0.10, hjust = -0.05, size=4,  position = position_dodge(0.9), angle = 90, fontface = "bold")+  
  ylab("Mean Expected Cost") + xlab("") + 
  # scale_fill_viridis_d("", option = "cividis") + 
  # scale_colour_viridis_d("", option = "cividis") + 
  scale_colour_manual("", values = MyPal) + 
  scale_fill_manual("", values = MyPal) + 
  
  theme_minimal() +
  theme(legend.position = c(0.25,0.9),
        strip.text = element_text(
          size = 14, color = "black", face = "bold"),
        axis.text.x  = element_text(size = 18),
        axis.text.y  = element_text(size = 18),
        axis.title = element_text(size = 18,face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))


openPDFEPS(paste0("Exp4.plot1_Errors"),width=10,height=5)
print(Exp4)
dev.off()



c2p$Experiments <- "Grid"
c2pH$Experiments <- "Humans"
allc2p  <- rbind(c2p, c2pH)


c2p$sd


MyPal <- c("#4895ef", "#3f37c9", "#006d77", "#f72585")
MyPal <- c("#0b3954", "#087e8b", "#ff5a5f", "#c81d25")

allExp4 <- ggplot(c2p, aes(domain, mean, fill = variable)) +
  geom_col(position = "dodge", alpha = 0.25) + 
  geom_col(data = c2pH, position = "dodge", alpha = 1) + 
  
  #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, colour = variable), width=0.5, size = 1,
  #              position=position_dodge(.9)) +
  geom_text(aes(y = 1.00*mean,label=paste(round(mean,2),"±",round(sd,1)), colour = variable),
            vjust=-0.30, hjust = -0.05, size=4,  position = position_dodge(0.9), angle = 75, fontface = "bold")+
  geom_text(data = c2pH, aes(y = 1.00*mean,label=paste(round(mean,2),"±",round(sd,1)), colour = variable), 
            vjust=0.2, hjust = -0.05, size=4,  position = position_dodge(0.9), angle = 75, fontface = "bold")+
  
  ylab("Mean Expected Cost") + xlab("") + 
  # scale_fill_viridis_d("", option = "cividis") + 
  # scale_colour_viridis_d("", option = "cividis") + 
  scale_colour_manual("", values = MyPal) + 
  scale_fill_manual("", values = MyPal) + 
  theme_minimal() +
  theme(legend.position = c(0.15,0.9),
        strip.text = element_text(
          size = 14, color = "black", face = "bold"),
        axis.text.x  = element_text(size = 18),
        axis.text.y  = element_text(size = 18),
        axis.title = element_text(size = 18,face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))

openPDFEPS(paste0("Exp4.plot.ALL"),width=10,height=5)
print(allExp4)
dev.off()




# Tables ---------------------------------------------------------------------------------


humans <- read.csv("humanCostsv1.csv")
head(humans)
humans1 <- humans[,-c(1,8,9,10,11)]
head(humans1)

humans1B <- read.csv("humanCostsv1B.csv")
head(humans1B)
# humansB <- humans[,-c(1,8,9,10,11)]
humans1B$responent <- humans1B$responent + 7


humans2 <- read.csv("humanCostsv2.csv")
head(humans2)
humans2$responent <- humans2$responent + 17

D <- rbind(humans1, humans1B, humans2)

EA <- read.csv("formsA-errades.csv")[1:14,]
EB <- read.csv("formsB-errades.csv")[1:14,]
Err <- rbind(EA,EB)

colnames(Err)[c(1,2,8)] <- c("domain", "Type", "Ave")
Err.vals <- Err %>% group_by(domain, Type) %>% summarise(Error = 1 - mean(Ave))
require(reshape2)
Err.vals.d <- dcast(Err.vals, domain ~Type)
Err.vals.d$domain  <- tolower(Err.vals.d$domain)

D <- left_join(D, Err.vals.d) 


form_loading = 15
num_items <- 5
D$ts <- (D$t_s - form_loading)/num_items
D$ti <- (D$t_i - form_loading)/num_items
D$cs <- D$ts*(D$c_op/3600)/D$c_e  + D$Supply # Comment the addition for table 4
D$ci <- D$ti*(D$c_op/3600)/D$c_e  + D$Inspect # Comment the addition for table 4


D$Supply <- as.numeric(D$Supply)
D$Inspect <- as.numeric(D$Inspect)
sapply(D,class)
colnames(D)[c(7,8)] <- c("e_i", "e_s")
# "responent" "domain"    "t_s"       "t_i"       "c_op"      "c_e"       "t_s.5"     "t_i.5"     "c_s.c_e"   "c_i.c_e"   "ts"        "ti"        "cs"        "ci"       

D <- as.data.frame(D)

skimr::skim(D)

sd2 <- function(x){
  return(sqrt(sum((x - mean(x))^2) / (n - 1)))
}

D.l <- D %>% group_by(domain) %>% summarise(
                                      Ts = mean(ts, na.rm = T),  Ts.sd = sd(ts, na.rm = T),
                                      Ti = mean(ti, na.rm = T),  Ti.sd = sd(ti, na.rm = T),
                                      Xt = mean(c_op, na.rm = T),  Xt.sd = sd(c_op, na.rm = T),
                                      Xw = mean(c_e, na.rm = T),  Xw.sd = sd(c_e, na.rm = T),
                                      "ei" = mean(e_i, na.rm = T),  "ei.sd" = sd(e_i),
                                      "es" = mean(e_s, na.rm = T),  "es.sd" = sd(e_s),
                                      Cs = mean(cs, na.rm = T),  Cs.sd = sd(cs, na.rm = T),
                                      Ci = mean(ci, na.rm = T),  Ci.sd = sd(ci, na.rm = T)
                                     )

Err.vals.sd <- Err %>% group_by(domain, Type) %>% summarise(sd = mean(SD))

D.l$ei.sd <- filter(Err.vals.sd, Type == "Inspect")$sd
D.l$es.sd <- filter(Err.vals.sd, Type == "Supply")$sd



D.lat <- data.frame(Domain = D.l$domain,
           Ts = paste0(round(D.l$Ts,2), "±", round(D.l$Ts.sd,2)),
           Ti = paste0(round(D.l$Ti,2), "±", round(D.l$Ti.sd,2)),
           Xt = paste0(round(D.l$Xt,2), "±", round(D.l$Xt.sd,2)),
           Xw = paste0(round(D.l$Xw,2), "±", round(D.l$Xw.sd,2)),
           es = paste0(round(D.l$es,2), "±", round(D.l$es.sd,2)),
           ei = paste0(round(D.l$ei,2), "±", round(D.l$ei.sd,2)),
           cs = paste0(round(D.l$Cs,3), "±", round(D.l$Cs.sd,3)),
           ci = paste0(round(D.l$Ci,3), "±", round(D.l$Ci.sd,3)))


D.l.median <- D %>% group_by(domain) %>% summarise(Ts = median(ts, na.rm = T),
                                            Ti = median(ti, na.rm = T),  
                                            Xt = median(c_op, na.rm = T),  
                                            Xw = median(c_e, na.rm = T), 
                                            es = median(e_s, na.rm = T),  
                                            ei = median(e_i, na.rm = T), 
                                            Cs = median(cs, na.rm = T),  
                                            Ci = median(ci, na.rm = T))


require(xtable)
print(xtable(D.lat), include.rownames = FALSE)
print(xtable(D.l.median, digits = 3), include.rownames = FALSE)

