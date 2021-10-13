gen_logis <- function(t,a,k,b,m,v){
    value <- a + (k-a)/( (1 + exp(-b*(t-m)))^(1/v) )
    return(value)
}

# a: the lower asymptote;
# k: the upper asymptote
# b: the growth rate;
# ν: >0 , affects near which asymptote maximum growth occurs.
# m: starting time. Doesn't change the shape, just shifts the logistic on the right/left. If you want the logistic to be simmetric (entire S) on positive domain, set it equal to time/2


get_close_cells <- function(mx,i,k,ray){
    near.ind <- cbind(rep(i + -ray:ray,length(-ray:ray)),
                      rep(k + -ray:ray,each=length(-ray:ray)))
    near.ind.val <- near.ind[  # eliminate out of bound values, or the actual x,y coord itself
        !(
            near.ind[, 1] < 1 | near.ind[, 1] > nrow(mx)  |
                near.ind[, 2] < 1 | near.ind[, 2] > ncol(mx)  |
                (near.ind[, 1] == i & near.ind[, 2] == k)
        ),
        ]
    colnames(near.ind.val)    <- c("row", "col") 
    return(near.ind.val)
}


# Determine the ray of separation between two cells
ray_separation <- function(x1,y1,x2,y2){
    o <- 1
    repeat{
        neighb <- get_close_cells(world[,,t,p],x1,y1,o)
        if(sum(neighb[,"row"]==x2 & neighb[,"col"]==y2)==1){
            break
        }
        o <- o + 1
    }
    return(o)
}

# Determine which cell is closest in the ray sense
ray_closest_cell <- function(property,x2,y2){ #property is the matrix containing lat and lon of all cells to choose between with, minimizing the distance wrt x2,y2
    distances <- array(0, dim = nrow(property))
    for (o in 1:nrow(property)) {
        distances[o] <- ray_separation(property[o,"row"],property[o,"col"],x2,y2)
    }
    minimum <- min(distances)
    index <- sample(which(distances==minimum),1) #if cells are equidistant, random draw
    return(property[index,])
}


get_ray_exact_cells <- function(mx,i,k,ray){
    near.ind <- cbind(rep(i + -ray:ray,length(-ray:ray)),
                      rep(k + -ray:ray,each=length(-ray:ray)))
    near.ind.old<- get_close_cells(mx,i,k,ray-1)
    log_sub <- cbind(near.ind[,1] %in% near.ind.old[,1],near.ind[,2] %in% near.ind.old[,2])
    near.ind <- near.ind[!(log_sub[,1]&log_sub[,2]),]
    near.ind.val <- near.ind[  # eliminate out of bound values, or the actual x,y coord itself
        !(
            near.ind[, 1] < 1 | near.ind[, 1] > nrow(mx)  |
                near.ind[, 2] < 1 | near.ind[, 2] > ncol(mx)  |
                (near.ind[, 1] == i & near.ind[, 2] == k)
        ),
        ]
    colnames(near.ind.val)    <- c("row", "col") 
    return(near.ind.val)
}


divisors <- function(x){
    #  Vector of numberes to test against
    y <- seq_len(x)
    #  Module division. If remainder is 0 that number is a divisor of x so return it
    y[ x%%y == 0 ]
}
