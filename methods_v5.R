# ------------------------------------------------------------------------------------------------
# Packages ---------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(plotly)
library(geometry) 


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
# Static methods----------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

optimNs <- function(n, cs, ci){
  
  myfun<-function(x,y,cs, ci)
  {
    return(cs*(x + y*(0.5 - x/(2*n))) +
             ci*y + (n - y - x)*((1/3) - x/(3*n)))
  }
  
  df <- gtools::permutations(n+1,2,0:n,repeats.allowed = TRUE,set = FALSE)
  df<-df[rowSums(df)<=n,]
  res<-mapply(myfun, df[,1], df[,2], cs, ci)
  
  
  q <- min(res)
  ns <- df[which.min(res),1]
  ni <- df[which.min(res),2]
  return(list(q, ns, ni))
  
}

computeT_o <- function(Cs, Ci, n = 10, all.results, domain_, problem_){ # n = 10 as we only have till 10-shot
  
  qbest = Inf
  ns = Inf 
  cost.evol <- c()
  thres.evol <- c()
  nums.evol <- c()
  for(numS in 1:n){
    
    # print(paste0("-------- ",numS))
    
    results.f <- filter(all.results, domain == domain_, engine == "davinci", nshot == numS, problem == problem_, threshold == 0.00)
    # results.f <- filter(all.results, engine == "davinci", nshot == numS, problem == problem_, threshold == 0.00)
    
    results.f.sort <- results.f[order(results.f$prob, decreasing = F),]
    results.f.sort$pReal <- as.numeric(results.f.sort$target == results.f.sort$output)
    
    for (j in 1:(nrow(results.f.sort)+1)){
      
      q <- Cs * numS
      
      if (j == 1){
        q <- q + Cs * 0 + Ci * 0 + sum(1-results.f.sort$pReal[j:nrow(results.f.sort)])
      }else{
        if (j == (nrow(results.f.sort)+1)){
          
          q <- q + Cs * sum(1-results.f.sort$pReal[1:(j-1)]) + Ci * (j-1) + 0
          
        }else{
          q <- q + Cs * sum(1-results.f.sort$pReal[1:(j-1)]) + Ci * (j-1) + sum(1-results.f.sort$pReal[(j):nrow(results.f.sort)])
        }
      }
      
      nums.evol <- c(nums.evol, numS)
      cost.evol <- c(cost.evol,q)
      thres.evol <-  c(thres.evol, (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2)
      
      if (q < qbest){
        qbest <- q
        
        if(j <=1){
          ti <- (results.f.sort$prob[j]+0)/2
        }else{
          if(j ==(nrow(results.f.sort)+1)){
            ti <- (1+results.f.sort$prob[j-1])/2  
          }else{
            ti <- (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2  
          }
        }
        
        
        jota <- j
        ns <- numS
        results <- results.f.sort 
      }
    }
    
  }
  
  # print(plot(thres.evol))
  # print(plot(cost.evol))
  # print(qbest)
  # print(ti)
  return(list(qbest = qbest, ti = ti, ns = ns, results = results, jota = jota))
}

computeT_sigma <- function(Cs, Ci, n, all.results, domain_, problem_){
  
  ns <- optimNs(n, Cs, Ci)[[2]]
  if(ns > 10){ns <- 10}
  
  results.f <- filter(all.results, domain == domain_, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
  # results.f <- filter(all.results, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
  
  # subset(results.f, domain == domain & engine == "davinci" & nshot == ns & problem == problem & threshold == 0.00)
  
  results.f.sort <- results.f[order(results.f$prob, decreasing = F),]
  
  qbest <- Inf
  cost.evol <- c()
  thres.evol <- c()
  
  
  
  for (j in 1:(nrow(results.f.sort)+1)){
    
    q <- Cs * ns
    
    if (j == 1){
      q <- q + Cs * 0 + Ci * (j-1) + sum(1-results.f.sort$prob[j:nrow(results.f.sort)])
    }else{
      if (j == (nrow(results.f.sort)+1)){
        
        q <- q + Cs * sum(1-results.f.sort$prob[1:(j-1)]) + Ci * (j-1) + 0
        
      }else{
        q <- q + Cs * sum(1-results.f.sort$prob[1:(j-1)]) + Ci * (j-1) + sum(1-results.f.sort$prob[j:nrow(results.f.sort)])
      }
    }
    cost.evol <- c(cost.evol,q)
    thres.evol <-  c(thres.evol, (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2)
    
    if (q < qbest){
      qbest <- q
      if(j <=1){
        ti <- (results.f.sort$prob[j]+0)/2
      }else{
        if(j ==(nrow(results.f.sort)+1)){
          ti <- (1+results.f.sort$prob[j-1])/2  
          
        }else{
          ti <- (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2  
        }
        
        
      }
    }
  }
  # print(plot(thres.evol))
  # print(plot(cost.evol))
  # print(qbest)
  # print(ti)
  return(list(qbest = qbest, ti = ti, ns = ns, results = results.f.sort))
}

computeT_phi <- function(ns, ti, all.results, domain_, problem_){
  
  if(ns > 10){ns <- 10}
  
  # results.f <- filter(all.results, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
  results.f <- filter(all.results, domain == domain_, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
  
  
  results.f.sort <- results.f[order(results.f$prob, decreasing = F),]
  
  return(list(ti = ti, ns = ns, results = results.f.sort))
  
}

computeT_delta <- function(Cs, Ci, all.results, domain_, problem_, ns = 1, iplus = 1){
  
  if(ns > 10){ns <- 10}
  
  q_s <- Cs * ns
  q_i <- 0
  qbest <- Inf
  n_v <- 0
  qold <- Inf
  
  iter = 0 
  # while(qbest >= qold){
  repeat{  
    iter = iter + 1
    print(paste0("------------------ iter: ", iter))
    
    qold <- qbest
    
    # results.f <- filter(all.results, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
    results.f <- filter(all.results, domain == domain_, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
    
    results.f.sort <- results.f[order(results.f$prob, decreasing = F),]
    results.f.sort$pReal <- as.numeric(results.f.sort$target == results.f.sort$output)
    
    n_o  <- nrow(results.f)
    D_i <- results.f.sort[1:iplus]
    n_i <- nrow(D_i)
    q_i <- q_i + Ci * n_i # cost inspected
    
    
    D_v <- filter(D_i, pReal == 1)
    (n_v <- n_v + nrow(D_v))
    # print(paste0("- nv: ", n_v))
    
    D_c <- filter(D_i, pReal == 0)
    q_s <- q_s + Cs * nrow(D_c)
    
    
    # print(paste0("- ns: ", ns))
    (ns <- ns + nrow(D_i))
    qbest <- Inf
    
    for(j in (iplus + 1):(n_o+1)){
      
      # print(paste0("--- j: ", j))
      
      if (j <= (iplus + 1)){
        q <- Cs * 0 + Ci * (j-1) + sum(1-results.f.sort$prob[j:n_o])
        
      }else{
        if (j == (n_o+1)){
          q <- Cs * sum(1-results.f.sort$prob[1:(j-1)]) + Ci * (j-1) + 0
          
        }else{
          q <- Cs * sum(1-results.f.sort$prob[(iplus + 1):(j-1)]) + Ci * (j-1) + sum(1-results.f.sort$prob[j:n_o])
          
        }
      }
      
      # print(paste("- q: ", q))
      
      if (q < qbest){
        (qbest <- q)
        # print(paste("-- qbest: ", qbest))
        if(j <=1){
          ti <- (results.f.sort$prob[j]+0)/2
          
        }else{
          if(j ==(nrow(results.f.sort)+1)){
            ti <- (1+results.f.sort$prob[j-1])/2  
            
          }else{
            ti <- (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2  
          }
        }
      }
    }
    (qbest <- qbest + q_s + q_i)
    
    if((qbest > (2 * qold) || ns > 10)){
      break
    }
  }
  
  return(list(qbest = qbest, ti = ti, ns = ns, nv = n_v, ni = n_i, results = results.f.sort))
}

computeT_delta_2 <- function(Cs, Ci, all.results, domain_, problem_, ns = 1, iplus = 1, sstar = NA){
  
  if(ns > 10){ns <- 10}
  
  q_s <- Cs * ns
  q_i <- 0
  qold <- Inf
  qbest <- Inf
  n_v <- 0
  n_i <- 0
  skip <- FALSE
  iter = 0 
  skip = FALSE
  
  repeat{  
    iter = iter + 1
    # print(paste0("------------------ iter: ", iter))
    
    # qold <- qbest
    
    results.f <- filter(all.results, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
    results.f <- filter(all.results, domain == domain_, engine == "davinci", nshot == ns, problem == problem_, threshold == 0.00)
    
    results.f.sort <- results.f[order(results.f$prob, decreasing = F),]
    results.f.sort$pReal <- as.numeric(results.f.sort$target == results.f.sort$output)
    
    if (!is.na(sstar)){
      skip <- (sstar <= ns)
    }else{
      skip <- ((qbest != Inf) && (qbest >= qold))
    }
    
    # print(paste0("Skip = ",skip))
    if(!skip){
      
      n_o  <- nrow(results.f)
      D_i <- results.f.sort[1:iplus]
      n_i <- n_i + nrow(D_i)
      q_i <- Ci * n_i # cost inspected
      
      
      D_v <- filter(D_i, pReal == 1)
      (n_v <- n_v + nrow(D_v))
      # print(paste0("- nv: ", n_v))
      
      D_c <- filter(D_i, pReal == 0)
      q_s <- q_s + Cs * nrow(D_c)
      
      
      # print(paste0("- ns: ", ns))
      (ns <- ns + nrow(D_i))
      
      
    }
    
    qold <- qbest
    qbest <- Inf
    
    for(j in (iplus + 1):(n_o+1)){
      
      # print(paste0("--- j: ", j))
      
      if (j <= (iplus + 1)){
        q <- Cs * 0 + Ci * (j-1) + sum(1-results.f.sort$prob[j:n_o], na.rm = T )
        
      }else{
        if (j == (n_o+1)){
          q <- Cs * sum(1-results.f.sort$prob[1:(j-1)], na.rm = T ) + Ci * (j-1) + 0
          
        }else{
          q <- Cs * sum(1-results.f.sort$prob[(iplus + 1):(j-1)], na.rm = T ) + Ci * (j-1) + sum(1-results.f.sort$prob[j:n_o], na.rm = T )
          
        }
      }
      
      # print(paste("- q: ", q))
      
      if (q < qbest){
        (qbest <- q)
        # print(paste("-- qbest: ", qbest))
        if(j <=1){
          ti <- (results.f.sort$prob[j]+0)/2
          
        }else{
          if(j ==(nrow(results.f.sort)+1)){
            ti <- (1+results.f.sort$prob[j-1])/2  
            
          }else{
            ti <- (results.f.sort$prob[j]+results.f.sort$prob[j-1])/2  
          }
        }
        # print(paste("-- ti: ", ti))
      }
    }
    (qbest <- qbest + q_s + q_i)
    
    if(skip || ns > 10){
      break
    }
  }
  
  return(list(qbest = qbest, ti = ti, ns = ns, nv = n_v, ni = n_i, results = results.f.sort))
}

computeQ <- function(ns, ti, results.f.sort, Cs, Ci){
  
  ni.results <- filter(results.f.sort, prob <= ti)
  ni <- nrow(ni.results)  
  j = ni +1 # +1: ----------------------------------------------------------------------------------------------------------------------------
  
  results.f.sort$pReal <- as.numeric(results.f.sort$target == results.f.sort$output)
  # results.f.sort <- results.f.sort[order(results.f.sort$pReal, decreasing = F),]
  
  q <- Cs * ns
  
  if (j == 1){
    q <- q + Cs * 0 + Ci * 0 + sum(1-results.f.sort$pReal[j:nrow(results.f.sort)])
  }else{
    if (j == (nrow(results.f.sort)+1)){
      
      q <- q + Cs * sum(1-results.f.sort$pReal[1:(j-1)]) + Ci * (j-1) + 0
      
    }else{
      q <- q + Cs * sum(1-results.f.sort$pReal[1:(j-1)]) + Ci * (j-1) + sum(1-results.f.sort$pReal[(j):nrow(results.f.sort)])
    }
  }
  
  return(q)
  
}

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


# ------------------------------------------------------------------------------------------------
# Main -------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------


# domain_ = "dates" 
# # "dates"    "emails"   "freetext" "names"    "phones"   "times"    "units"
# 
# problem_ = c("addPunctuation-1","addPunctuation-2","changeFormat-1","changeFormat-2","changeFormat-3")
# problem_ <- "changeFormat-1"
# problem_ = c("changeFormat-4","reduceMonthName-1", "getBetweenCommas-1", "setPrefix-5")
# problem_ <- c("addPunctuation-2", "betweenSymbols-2")
# problem_ <- "addPunctuation-2"


# problem_ = c("changeFormat-1")
# [1] "addPunctuation-1"    "addPunctuation-2"    "changeFormat-1"      "changeFormat-2"      "changeFormat-3"      "changeFormat-4"     
# [7] "changePunctuation-1" "changePunctuation-2" "getDay-1"            "getDay-2"            "getDay-3"            "getDayOrdinal-1"    
# [13] "getDayOrdinal-2"     "getMonthName-1"      "getMonthName-2"      "getWeekDay-1"        "getWeekDay-2"        "reduceMonthName-1"  
# [19] "reduceMonthName-2"   "setFormat-1"         "setFormat-2"         "generate-1"          "generate-2"          "generate-3"         
# [25] "getAfterAt-1"        "getAfterAt-2"        "getAfterAt-3"        "getAfterAt-4"        "getDomain-1"         "getDomain-2"        
# [31] "someBeforeAt-NA"     "afterSymbol-1"       "afterSymbol-2"       "betweenSymbols-1"    "betweenSymbols-2"    "brackets-1"         
# [37] "brackets-2"          "deletePunctuation-1" "deletePunctuation-2" "deletePunctuation-3" "deletePunctuation-4" "deleteSpaces-1"     
# [43] "deleteSpaces-2"      "digitToEnd-1"        "digitToEnd-2"        "firstCharacter-1"    "firstCharacter-2"    "getAfterComma-1"    
# [49] "getAfterComma-2"     "getBetweenCommas-1"  "getBetweenCommas-2"  "getCaps-1"           "getCaps-2"           "getCaps-3"          
# [55] "toUpper-1"           "toUpper-2"           "addTitle-1"          "addTitle-2"          "getTitle-1"          "getTitle-2"         
# [61] "login-1"             "login-2"             "reduceName-1"        "reduceName-2"        "reduceName-3"        "reduceName-4"       
# [67] "reduceName-5"        "reduceName-6"        "reduceName-7"        "reduceName-8"        "reduceName-9"        "countryPrefix-1"    
# [73] "countryPrefix-2"     "countryPrefix-3"     "countryPrefix-7"     "countryPrefix-8"     "countryPrefix-9"     "deleteParentheses-1"
# [79] "deleteParentheses-2" "getNumber-1"         "getNumber-2"         "setPrefix-1"         "setPrefix-2"         "setPrefix-3"        
# [85] "setPrefix-4"         "setPrefix-5"         "setPrefix-6"         "setPunctuation-1"    "setPunctuation-2"    "addTime-1"          
# [91] "addTime-2"           "appendTime-1"        "appendTime-2"        "appendTime-3"        "appendTime-4"        "convert-1"          
# [97] "convert-10"          "convert-2"           "convert-3"           "convert-4"           "convert-5"           "convert-6"          
# [103] "convert-7"           "convert-8"           "convert-9"           "deleteTime-1"        "deleteTime-2"        "getHour-1"          
# [109] "getHour-2"           "getMinutes-1"        "getMinutes-2"        "getTime-1"           "getTime-2"           "getSystem-1"        
# [115] "getSystem-2"         "getUnits-1"          "getUnits-2"          "getValue-1"          "getValue-2"    



problem_ <- unique(reli$problem)
prob_domain_ <- unique(reli[, c("domain", "problem")])

res2plot <- data.frame(scene = 0, domain = NA, problem = NA,  ci= NA, cs= NA, qStatic= NA, qOptimal = NA, qDelta = NA, qPhi = NA,
                       qStatic.best = NA, qOptimal.best = NA, qDelta.best = NA)

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
D$cs <- D$ts*(D$c_op/3600)/D$c_e  + D$Supply
D$ci <- D$ti*(D$c_op/3600)/D$c_e  + D$Inspect


# newaxis <- seq(0,1, 0.0526)  # El ultimo punto es 0.9994, con lo que apuras bastante la esquina alta.
# oldaxis <- -log(1-newaxis)
# 
# 
# cost_s <- oldaxis
# cost_i <- oldaxis

scene = 0


for(dom in unique(prob_domain_$domain)){
  
  prob_domain_sel <- filter(prob_domain_, domain == dom)
  
  for (task in prob_domain_sel$problem){
    
    print(paste0(":::::::::::::::::::", dom, " -- ", task, ":::::::::::::::::::"))
    
    q.t.s <- c(); q.est.s <- c(); ti.t.s <- c(); 
    q.t.p <- c(); ti.t.p <- c(); 
    ci.t <- c(); cs.t <- c(); shot.t <- c(); 
    q.t.o <- c(); q.est.o <- c(); ti.t.o <- c(); 
    q.t.d <- c(); q.est.d <- c(); ti.t.d <- c(); 
    
    costs_domain <- filter(D, domain == dom)
    
    for (costs_index in 1:nrow(costs_domain)){
      
      i <- costs_domain[costs_index, "ci"]
      s <- costs_domain[costs_index, "cs"]
      
      print(paste0("- ",i," ,",s))
      # Cs = s; Ci = i; n = 32; all.results = reli; domain = domain_; problem = task
      resSigma <- computeT_sigma(Cs = s, Ci = i, n = 32, all.results = reli, domain_ = dom, problem_ = task)
      (qb.s <- resSigma[["qbest"]])
      (ns.s = resSigma[["ns"]])
      (ti.s = resSigma[["ti"]])
      (results.f.sort.s= resSigma[["results"]])
      (q_real_Sigma <- computeQ(ns = ns.s, ti = ti.s, results.f.sort = results.f.sort.s, Cs = s, Ci = i))
      # print(paste0("Sigma ns: ", ns.s))
      
      resPhi <- computeT_phi(ns = 5, ti = 0.5, all.results = reli, domain_ = dom, problem_ = task)
      (ns.p = resPhi[["ns"]])
      (ti.p = resPhi[["ti"]])
      (results.f.sort.p= resPhi[["results"]])
      (q_real_Phi <- computeQ(ns = ns.p, ti = ti.p, results.f.sort = results.f.sort.p, Cs = s, Ci = i))
      # print(paste0("Sigma ns: ", ns.s))
      
      
      
      # Cs = s; Ci = i; n = 10; all.results = reli; domain = domain_; problem = task
      resOpt <- computeT_o(Cs = s, Ci = i, n = 10, all.results = reli, domain_ = dom, problem_ = task)
      (qb.o <- resOpt[["qbest"]])
      (ns.o = resOpt[["ns"]])
      (ti.o = resOpt[["ti"]])
      (results.f.sort.o = resOpt[["results"]])
      # ns = ns.o; ti = ti.o; results.f.sort = results.f.sort.o; Cs = s Ci = i
      (q_real_opt <- computeQ(ns = ns.o, ti = ti.o, results.f.sort = results.f.sort.o, Cs = s, Ci = i))
      
      
      # Cs, Ci, all.results, domain_, problem_, ns = 1, iplus = 1
      # Cs = s, Ci = i, all.results = reli, problem = task, ns = 1, iplus = 1
      resDelta <- computeT_delta_2(Cs = s, Ci = i, all.results = reli, domain_ = dom, problem_ = task, ns = 1, iplus = 1)
      (qb.d <- resDelta[["qbest"]])
      (ns.d = resDelta[["ns"]])
      (ti.d = resDelta[["ti"]])
      (nv.d = resDelta[["nv"]])
      (ni.d = resDelta[["ni"]])
      # print(paste0("Delta ns: ", ns.d))
      (results.f.sort.d = resDelta[["results"]])
      # ns = ns.d; ti = ti.d; results.f.sort = results.f.sort.d; Cs = s; Ci = i
      q_real_delta <- computeQ(ns = ns.d, ti = ti.d, results.f.sort = results.f.sort.d, Cs = s, Ci = i) - s * nv.d + i * ni.d
      
      
      
      q.t.s <- c(q.t.s, q_real_Sigma); q.est.s <- c(q.est.s, qb.s); ti.t.s <- c(ti.t.s, ti.s) 
      q.t.p <- c(q.t.p, q_real_Phi); ti.t.p <- c(ti.t.p, ti.p);
      ci.t <- c(ci.t, i); cs.t <- c(cs.t, s)
      q.t.o <- c(q.t.o, q_real_opt); q.est.o <- c(q.est.o, qb.o); ti.t.o <- c(ti.t.o, ti.o)
      q.t.d <- c(q.t.d, q_real_delta); q.est.d <- c(q.est.d, qb.d); ti.t.d <- c(ti.t.d, ti.d)
      
      
      # shot.t <- c(shot.t, shot)
    }
    
    
    scene <- scene + 1
    res2plot.temp <- data.frame(scene  = scene, domain = dom, problem = task, ci= ci.t, cs= cs.t, 
                                qStatic= q.t.s, qOptimal = q.t.o, qDelta = q.t.d, qPhi = q.t.p,
                                qStatic.best = q.est.s, qOptimal.best = q.est.o, qDelta.best = q.est.d)
    res2plot <- rbind(res2plot, res2plot.temp)
    
  }
}



saveRDS(res2plot, file = "res2plot.RDS")

# ------------------------------------------------------------------------------------------------
# Plots ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------


res2plot <- res2plot[-1,]

res2plot$hci <- h(res2plot$ci)
res2plot$hcs <- h(res2plot$cs)

res2plot$scene  <- paste0("scene",res2plot$scene)
colnames(res2plot)[6] <- "Q"


axx <- list(
  nticks = 5,
  range = c(-0,1)
)

axy <- list(
  nticks = 5,
  range = c(0,1)
)

axz <- list(
  title = "Q",
  nticks = 5)


d2p <- as.data.frame(filter(res2plot, problem == "addPunctuation-2"))
levels(d2p$problem)
# as.data.frame(filter(res2plot, problem == "addPunctuation-2")) 
colnames(d2p)[6] <- "Q"

# betweenSymbols-2
d2p %>%
  split(d2p$problem) %>% 
  lapply(function(x) {
    plot_ly(data = x, scene= ~scene) %>%  
      add_trace(x = ~hci, y = ~hcs, z = ~Q, intensity = ~Q, type="mesh3d", name = "Q",  showlegend = FALSE, colorscale = list(c(0, 0.33, 0.66, 1), c("#820263", "#d90368", "#ff8fab", "#ffc2d1"))) %>%  
      add_trace(x = ~hci, y = ~hcs, z = ~Q, type = "scatter3d", name = "Q", showlegend = FALSE, marker = list(
        color = 'rgba(255, 255, 255,0.75)',
        size = 2,
        line = list(
          color = 'rgba(255, 255, 255,0.75)',
          width = 2
        ))) %>% 
      add_trace(x = ~hci, y = ~hcs, z = ~qStatic, intensity = ~qStatic, type="mesh3d", showlegend = FALSE, colorscale = list(c(0, 0.33, 0.66, 1), c("#03045e", "#0077b6", "#00b4d8", "#90e0ef"))) %>%
      add_trace(x = ~hci, y = ~hcs, z = ~qStatic, type = "scatter3d", showlegend = FALSE,marker = list(
        color = 'rgba(255, 255, 255,0.75)',
        size = 2,
        line = list(
          color = 'rgba(255, 255, 255,0.75)',
          width = 2
        ))) %>%
      
      
      add_trace(x = ~hci, y = ~hcs, z = ~qDelta, intensity = ~qDelta, type="mesh3d", showlegend = FALSE, colorscale = list(c(0, 0.33, 0.66, 1), c("#004b23", "#007200", "#38b000", "#9ef01a"))) %>%
      add_trace(x = ~hci, y = ~hcs, z = ~qDelta, type = "scatter3d", showlegend = FALSE,marker = list(
        color = 'rgba(255, 255, 255,0.75)',
        size = 2,
        line = list(
          color = 'rgba(255, 255, 255,0.75)',
          width = 2
        ))) %>%
      
      
      # add_trace(x = ~hci, y = ~hcs, z = ~qPhi, intensity = ~qPhi, type="mesh3d", showlegend = FALSE, colorscale = list(c(0, 0.33, 0.66, 1), c("#ff7b00", "#ff9500", "#ffaa00", "#ffc300"))) %>%
      # add_trace(x = ~hci, y = ~hcs, z = ~qPhi, type = "scatter3d", showlegend = FALSE,marker = list(
      #   color = 'rgba(255, 255, 255,0.75)',
      #   size = 2,
      #   line = list(
      #     color = 'rgba(255, 255, 255,0.75)',
      #     width = 2
      #   ))) %>%
      
    
    add_annotations(
      text = ~problem,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>%
      layout(scene1 = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectmode='cube'),
             scene2 = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectmode='cube'),
             scene3 = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectmode='cube'),
             scene4 = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectmode='cube'),
             scene5 = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectmode='cube'))
  }) %>% 
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)







