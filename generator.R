if(!require(data.table,quietly = T)) install.packages("data.table",quiet = T); library(data.table,quietly = T)

hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
Hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)
Hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)


linearApproximation <- function(x_points,y_points,n_segments) {
    dbg <- F
    
    n_points <- dim(y_points)[1]    # number of rows in y_points
    n_curves <- dim(y_points)[2]    # number of cols in y_points
    
    m_cost <- matrix(Inf,n_points,n_segments)
    m_cost[2,1] <- 0.0
    h_cost <- matrix(Inf,n_points,n_points)
    father <- matrix(0,n_points,n_segments)
    
    for (n in 2:n_points) {
        m_cost[n,1] <- max(-Inf, sapply(1:n_curves,function(ic) segmentError(x_points,y_points[,ic],1,n)) )
        # std::cout << " n=" << n << " mCost[" << n << "][0]=" << temp << " father[" << n << "][0]=" << father[n][0] << "\n";
    }
    
    for (m in 2:n_segments) {        
        # std::cout << m << "... ";
        for (n in 3:n_points) {
            min_error <- m_cost[n-1,m-1]
            min_index <- n - 1
            if(m <= (n-2))
            for (i in m:(n-2)) {
                if (h_cost[i,n] == Inf) h_cost[i,n] <- max(-Inf, sapply(1:n_curves,function(ic) segmentError(x_points,y_points[,ic],i,n)) )
                curr_error <- m_cost[i,m-1] + h_cost[i,n]
                if (curr_error < min_error) {
                    min_error <- curr_error
                    min_index <- i
                }
            }
            m_cost[n,m] <- min_error
            father[n,m] <- min_index
            # std::cout << " n=" << n << " mCost[" << n << "][" << m << "]=" << mCost[n][m] << " father[" << n << "][" << m << "]=" << father[n][m] << "\n";
        }
    }
    # std::cout << "\n";

    for (i in (n_segments+1):1) {
        if(i == (n_segments+1)) ib[i] <- n_points
        else                    ib[i] <- father[ib[i+1],i]
    }
    
#     if(dbg) {
#         # std::cout << "\tSegments thresholds found:\n";
#         for (int i = 0; i < n_segments + 1; i++) {
#             std::cout << "\t" << xb[i] << "\n";
#         }
#     }
    return(x_points[ib])
}


# x, y : are vectors of x and y coordinates (they must have same length)
# first, last : are indices into these vectors
segmentError <- function(x, y, first, last) {
    # Compute line segment coefficients
    a <- (y[last] - y[first]) / (x[last] - x[first])
    b <- (y[first] * x[last] - y[last] * x[first]) / (x[last] - x[first])
    
    # Compute error for the line segment
#     e <- 0.0
#     for(k in first:last) e = e + (y[k] - a * x[k] - b) * (y[k] - a * x[k] - b)
#     e = e/(a*a + 1)
    e <- sum(sapply(first:last,function(k) (y[k] - a * x[k] - b) * (y[k] - a * x[k] - b))) / (a*a + 1)    
    return(e)
}


np <- 1500
points <- seq(0,100,length.out=np)
y <- hillp(points,5)
plot(range(points),range(y),type="n")
points(points,y,pch=".",cex=1)
thr <- linearApproximation(points,as.matrix(y),5)
thr_y <- hillp(thr,5)
points(thr,thr_y,pch=4,col="red",ps=2, lwd=2)


