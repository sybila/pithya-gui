veloc <- 1000
colorThres <- 0.05
plotSize <- 650

empty_sign <- " "
range_granul <- 100
zoom_granul <- 0.01
transitions_line_type <- "solid" #"dashed" #"dotted"
num_of_flow_points <- 400
size_of_flow_points <- 3
progressMaxLength <- 10
hover_delay_limit <- 10
brush_delay_limit <- 700
param_space_clicked_point <- list(type=4, color="blue", size=3, width=2)
no_param_const <- 1
rounding_in_hover <- 6

positive_color <- "green" 
#positive_color <- "darkgreen"
negative_color <- "red"
neutral_color <- "black"
#positive_color <- "black"
#negative_color <- "black"

examples_dir <- "example"

conj <- function(x) apply(x,1,function(i) !(F %in% i))

disj <- function(x) apply(x,1,function(i) (T %in% i))

chunk <- function(d,n) split(d, ceiling(seq_along(d)/n))

hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
Hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)
Hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)

# Approx <- function(m,l) {
#     apply(m,c(1,2),function(s)
#     ifelse(s <= l[[1]][1], l[[1]][2],
#            ifelse(s >= l[[length(l)]][1], l[[length(l)]][2],
#                   {
#                       thr <- sapply(l,function(x) x[1])
#                       val <- sapply(l,function(x) x[2])
#                       a <- max(which(thr <= s))
#                       b <- min(which(thr >= s))
#                       div <- (thr[b]-thr[a])*(val[b]-val[a])
#                       val[a]+(s-thr[a])/ifelse(div==0,1,div)
#                   }))
#     )
# }
Approx <- function(m,l) {
    apply(as.matrix(m),c(1,2),function(s) {
        if(s <= l[[1]][1]) return(l[[1]][2])
        if(s >= l[[length(l)]][1]) return(l[[length(l)]][2])
        for(i in 2:length(l)) {
            a<-l[[i-1]]
            b<-l[[i]]
            if(s >= a[1] && s <= b[1]) return(a[2]+(s-a[1])/(b[1]-a[1])*(b[2]-a[2]))
        }
    })
}
# Approx <- function(m,l) {
#     dt <- data.table(t(sapply(l,function(x)as.numeric(c(x[1],x[2])))))
#     setnames(dt,c("V1","V2"),c("h","hv"))
#     ndt <- dt[1:(nrow(dt)-1)]
#     setnames(ndt,c("h","hv"),c("l","lv"))
#     ndt <- cbind(ndt,dt[2:nrow(dt)])
#     apply(as.matrix(m),c(1,2),function(s) ifelse(s <= l[[1]][1], l[[1]][2], ifelse(s >= l[[length(l)]][1], l[[length(l)]][2], ndt[s >= l & s <= h, lv+(s-l)/(h-l)*(hv-lv)])))
# }
#ramp <- function(x,a,b,c,d) max(0,min(1,(x - a)/(b-a))) * abs(c-d)
#ramp <- function(x,a,b,c,d) ifelse(c<d,max(0,min(1,(x - a)/(b-a)))*abs(c-d),(1-max(0,min(1,(x - a)/(b-a))))*abs(c-d))
#ramp <- function(x,a,b,c,d) ifelse(c<d,max(0,min(1,(x - a)/(b-a)))*abs(c-d),ifelse(min(1,max(0,1-(x - a)/(b-a))) > 0 & min(1,max(0,1-(x - a)/(b-a))) < 1,(1-min(1,max(0,1-(x - a)/(b-a))))*abs(c-d),min(1,max(0,1-(x - a)/(b-a)))*abs(c-d)))
# ramp <- function(x,min,max,min_value,max_value) {
#     if (x >= min && x < max) {
#         res <- (x - min) / (max - min)
#         if (min_value > max_value) {
#             return(min_value - res * abs(max_value - min_value))
#         } else {
#             return(min_value + res * abs(max_value - min_value))
#         }
#     } else {
#         return(0)
#     }
# }
# ramp <- function(value,min,max,a,b) {
#     if(a < b) {
#         #positive
#         if(value < min) return(0)
#         if(value > max) return(b-a)
#         return((value-min)/(max-min)*(b-a))
#     } else {
#         #negative
#         if(value < min) return(a-b)
#         if(value > max) return(0)
#         return((1-(value-min)/(max-min))*(a-b))
#     }
# }

ramp <- function(value,min,max,a,b) {
    if(a < b) {
        #positive
        if(value < min) return(0)
        if(value > max) return(0)
        return(a+(value-min)/(max-min)*(b-a))
    } else {
        #negative
        if(value < min) return(0)
        if(value > max) return(0)
        return((a-(value-min)/(max-min))*(a-b))
    }
}

# if (value <= thresholds.first()) return values.first()
# if (value >= thresholds.last()) return values.last()
# val position = Arrays.binarySearch(thresholds, value)
# if (position >= 0) {    //evaluated value is one of the thresholds
#                         return values[position]
# } else {                //position points to -1 * (upper threshold)
#                         val iH = -position-1  //note that this must be a valid index, otherwise some conditions above would fire
#                         val iL = iH-1
#                         return values[iL] + (value - thresholds[iL]) / (thresholds[iH] - thresholds[iL]) * (values[iH] - values[iL])
# }

######## RESULT PART ##########

rounding_precission <- 8

ps_grey_colors <- function(x,n,a) {
    #if(n == 1) grey(0,alpha = 0.1)
    #else 
        grey.colors(n,start = 0.4, end = 0,alpha = a)[x]
}


# oneStep <- 0.2
# max <- 15
# maxThr <- c(max,max,max,max,max,max)
#  
# xRange <- seq(0,max,oneStep)
# yRange <- seq(0,max,oneStep)
# zRange <- seq(0,max,oneStep)
# 
# rRange <- seq(0,max,oneStep)

# list_of_names6 <- list("TCP","R-DCP","ECH","CPD","GDL","GLY",empty_sign)
# 
# func6 <- list()
# func6[[1]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) -1*k2*p2*hillp(x,K2,1)")
# func6[[2]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) k1*p1*hillp(y,K1,1) - k2*p2*hillp(x,K2,1)")
# #func6[[3]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) k1*p1*hillp(y,K1) - k2*p2*hillp(x,K2,1)")
# func6[[3]] <- parse(text="function(i) i$k1*i$p1*hillp(i$y,i$K1) - i$k2*i$p2*hillp(i$x,i$K2,1)")
# #func6[[3]] <- parse(text="function(i) i[[3]]*i[[7]]*hillp(i[[1]],i[[5]]) - i[[4]]*i[[8]]*hillp(i[[2]],i[[6]],1)")
# func6[[4]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) k1*p1*hillp(y,K1,1) - k2*p2*hillp(x,K2,1)")
# func6[[5]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) k1*p1*hillp(y,K1,1) - k2*p2*hillp(x,K2,1)")
# func6[[6]] <- parse(text="function(y,x,k1,k2,K1,K2,p1,p2) k1*p1*hillp(y,K1,1)")
# 
# func6_names <- c("TCP","R-DCP","ECH","CPD","GDL","GLY")
# 
# func7 <- vector(length=7)
#func7[1] <- function(x,k1,k2,K,p) func6[1](x,k1,K,p) - k2*p*hillp(x,K,1)
#func7[2] <- function(x,y,k1,k2,K1,K2,p1,p2) func6[2](x,y,k1,k2,K1,K2,p1,p2)
#func7[3] <- function(x,y,z,k1,k2,k3,K1,K2,K3,p1,p2) func6[3](x,y,k1,k2,K1,K2,p1,p2) + k3*p1*hillp(z,K3,1)
#func7[4] <- function(x,y,k1,k2,K1,K2,p1,p2) func6[4](x,y,k1,k2,K1,K2,p1,p2)
#func7[5] <- function(x,y,k1,k2,K1,K2,p1,p2) func6[5](x,y,k1,k2,K1,K2,p1,p2)
#func7[6] <- function(y,k,K,p) func6[6](y,k,K,p)
#func7[7] <- function(x,y,k1,k2,K1,K2,p1,p2) func6[2](x,y,k1,k2,K1,K2,p1,p2)

# func7_names <- c("TCP","R-DCP","ECH","CPD","GDL","GLY","S-DCP")

#vytvorit strukturu, ktora bude mat indexovane vsetky nastavenia parametrov aj pre 6/7 variantu aj pre vsetky 3 DhaA varianty
# param_version <- 1
# 
# input6 <- list(
#             list(list(k1=NULL, k2=0.04, K1=NULL, K2=1.01),
#                list(k1=0.04, k2=1.81, K1=1.01, K2=2.19),
#                list(k1=1.81, k2=14.37, K1=2.19, K2=0.09),
#                list(k1=14.37, k2=2.38, K1=0.09, K2=0.86),
#                list(k1=2.38, k2=3.96, K1=0.86, K2=3.54),
#                list(k1=3.96, k2=NULL, K1=3.54, K2=NULL)),
#             list(list(k1=NULL, k2=0.58, K1=NULL, K2=1.79),
#                list(k1=0.58, k2=1.81, K1=1.79, K2=2.19),
#                list(k1=1.81, k2=14.37, K1=2.19, K2=0.09),
#                list(k1=14.37, k2=2.38, K1=0.09, K2=0.86),
#                list(k1=2.38, k2=3.96, K1=0.86, K2=3.54),
#                list(k1=3.96, k2=NULL, K1=3.54, K2=NULL)),
#             list(list(k1=NULL, k2=0.19, K1=NULL, K2=12.56),
#                list(k1=0.19, k2=1.81, K1=12.56, K2=2.19),
#                list(k1=1.81, k2=14.37, K1=2.19, K2=0.09),
#                list(k1=14.37, k2=2.38, K1=0.09, K2=0.86),
#                list(k1=2.38, k2=3.96, K1=0.86, K2=3.54),
#                list(k1=3.96, k2=NULL, K1=3.54, K2=NULL))
# )
