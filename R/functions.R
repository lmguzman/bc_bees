## pollinator abundance function
abundance <- function(m, v.mat) sum(rowSums(v.mat[,m]))

## pollinator richness function
richness <- function(m, v.mat) sum(rowSums(v.mat[,m]>0)>0)

geometric.mean <- function(x){
  (prod(x))^(1/length(x))
}
## pollinator phenology function
phenology <- function(m, bloom.times) {

  possible.coverage <- unique(unlist(bloom.times))
  coverage <- rle(sort(unlist(bloom.times[m])))
  tmp <- rep(0, 52)
  tmp[coverage$values] <- coverage$lengths
  geometric.mean(tmp+1)
}

## number of fully supported species
numsupported <- function(m, v.mat, bloom.times, flight.times) {
  plants.by.polli <-
    apply(v.mat, 1, function(x) colnames(v.mat)[x>=1])
  m <- colnames(v.mat)[m]
  present <-
    lapply(plants.by.polli, function(x) intersect(x, m))
  any.coverage <- which(sapply(present, length)>0)
  coverage <-
    lapply(present[any.coverage], function(x)
      unique(unlist(sapply(x, function(x) bloom.times[x]))))
  covered <-
    !mapply(function(a, b) any(!a%in%b),
            flight.times[any.coverage], coverage)
  sum(covered)
}

## combination functions

abundance.phenology.richness <- function(m, v.mat, bloom.times)
  abundance(m, v.mat)*phenology(m, bloom.times)*richness(m, v.mat)

#abundance.numsupported.richness <- function(m, ...)
#  abundance(m, ...)*numsupported(m, ...)*richness(m, ...)

## compute score for model m
score <- function(f.list, m) {
  g <- function(f.list, m) sapply(f.list, function(y) y(m))
  prod(g(f.list, m))
}




#### (1) Initial state:
## initial.popn randomly generates N solutions for n.plants.


initial.popn <- function(N, n.plants, n.plants.tot, fitness, v.mat, bloom.times){
  
  make.model <- function(x, n.plants, n.plants.tot) {
    m <- rep(FALSE, n.plants.tot)
    m[sample(n.plants.tot, n.plants)] <- TRUE
    m
  }

  popn <- lapply(1:N, make.model, n.plants, n.plants.tot)
  w <- sapply(popn, fitness, v.mat, bloom.times)
  list(popn=popn,
       w=w,
       best.w=max(w),
       best.model=popn[[which.max(w)]])
}

#### (2) Mutation
## Next, take a population 'popn', with a fitness vector 'w' (where
## the ith element in 'w' gives the fitness of the ith model in
## 'popn').
##
## First, draw binomial random deviates to determine how many
## mutations will happen per model (mutations happen with probability
## 'p.mutate', and there are 'n.plants' available for mutation).
##
## Mutate the models that had at least one mutation, by passing them
## through to 'mutate'.
##
## Only mutated models need their fitness recalculated.
##
## Finally, return a list with the new population of models and their
## fitness vector.
mutate <- function(state, p.mutate, fitness, v.mat, bloom.times) {
  
  popn <- state$popn
  w <- state$w
  nmutants <- rbinom(length(popn), length(popn[[1]]), p.mutate)
  nmutants <- sapply(nmutants, function(x) min(sum(popn[[1]]), x))
  mutated <- which(nmutants>0)
  
  if ( length(mutated) > 0 ) {
    popn[mutated] <-
      mapply(mutate.1, popn[mutated], nmutants[mutated],
             SIMPLIFY=FALSE)
    w[mutated] <- sapply(popn[mutated], fitness, v.mat, bloom.times)
  }
  list(popn=popn,
       w=w,
       best.w=state$best.w,
       best.model=state$best.model)
}

## Take a model 'x', specifying which 'n.plants', and mutate it.  To
## do this (for a single mutant), randomly swap one included plant for
## one not included (i.e. a model cannot, therefore, mutate to
## itself).
mutate.1 <- function(x, n.mutants) {
  if(n.mutants > sum(x))
    cat("Mutation rate too high. Too many mutants\n")
  x1 <- sample(which(x), size=n.mutants)
  x2 <- sample(which(!x), size=n.mutants)
  x[x1] <- FALSE
  x[x2] <- TRUE
  x
}

#### (3) Selection:
## This selection regime is fairly straightforward; sample with
## replacement s*N models from the population, then take the N best
## models.
select <- function(state, s) {
  N <- length(state$popn)
  i <- sample(N, ceiling(s*N), TRUE)
  take <- i[order(state$w[i], decreasing=TRUE)[1:N]]
  list(popn=state$popn[take],
       w=state$w[take],
       best.w=state$best.w,
       best.model=state$best.model)
}

#### DIFFERENT SELECTION ALGORITHM
## select.weighted <- function(popn, w) {
##   N <- length(popn)
##   take <- sample(N, N, TRUE, w-min(w)+1)
##   list(popn=popn[take], w=w[take])
## }

#### (4) Recombiation
## Select a fraction of the population to be recombined.  These are
## recombined with randomly selected individuals in the previous
## generation (popn0).  The fitness of the recombinants is then
## calculated.
recombine <- function(N, state, popn0, p.sex, p.rec, fitness, v.mat, bloom.times) {
  popn1 <- state$popn
  w1 <- state$w
  N <- length(popn1)
  nrec <- rbinom(1, N, p.sex)
  i <- sample(N, nrec)
  if ( length(i) > 0 ) {
    b <- popn0[sample(N, nrec, TRUE)]
    popn1[i] <- mapply(recombine.1, popn1[i], b, p=p.rec,
                       SIMPLIFY=FALSE)
    w1[i] <- sapply(popn1[i], fitness, v.mat, bloom.times)
  }
  list(popn=popn1,
       w=w1,
       best.w=state$best.w,
       best.model=state$best.model)
}

## recombination a pair of models:
recombine.1 <- function(a, b, p) {
  m.a <- which(a)
  m.b <- which(b)
  in.both <- intersect(m.a, m.b)
  
  m.a <- setdiff(m.a, in.both)
  m.b <- setdiff(m.b, in.both)
  
  rr <- NULL
  
  if(length(m.a)>0) {
    rec <- runif(length(m.a)) < p
    if ( any(rec) ) {
      parent <- c(cumsum(rec)%%2)+1
      recombinant <- m.a
      recombinant[parent==2] <- m.b[parent==2]
      recombinant <- sort(c(recombinant, in.both))
      a <- rep(FALSE, length(a))
      a[recombinant] <- TRUE
      a
    }
  }
  a
}



#### (5) Step the population through one full generation:
ga.step <- function(N, state, s, p.mutate, p.sex, p.rec, fitness, v.mat, bloom.times) {
  ## Mutation
  state.m <- update.state(state, mutate(state=state,
                                        p.mutate=p.mutate,
                                        fitness=fitness, v.mat, bloom.times))
  
  ## Selection
  state.s <- update.state(state, select(state=state.m, s=s))
  
  ## Recombination
  state.r <- update.state(state, recombine(N, state=state.s,
                                           popn0=state$popn,
                                           p.sex=p.sex, p.rec=p.rec,
                                           fitness=fitness, v.mat, bloom.times))
  state.r
}

## Bookkeeping to make sure that we keep the best model
update.state <- function(s0, s1) {
  if ( max(s1$w) > s0$best.w ) {
    s1$best.w     <- max(s1$w)
    s1$best.model <- s1$popn[[which.max(s1$w)]]
  } else {
    s1$best.w     <- s0$best.w
    s1$best.model <- s0$best.model
  }
  s1
}

## Run the GA for ngens generations, collecting the best fitness over
## time, and returning the state and best model.
run.ga <- function(n.plants,
                   n.plants.tot,
                   N,
                   n.gens,
                   s,
                   p.mutate,
                   p.sex,
                   p.rec,
                   fitness, v.mat, bloom.times) {
  x <- initial.popn(N, n.plants, n.plants.tot,
                    fitness=fitness, v.mat, bloom.times)
  out <- vector("numeric", n.gens)
  for ( i in seq_len(n.gens) ){
    x <- ga.step(N, x, s, p.mutate, p.sex, p.rec, fitness, v.mat, bloom.times)
    out[i] <- x$best.w
  }
  list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)
}

## run the GA with default values
find.mix <- function(f=f,
                     k=k,
                     N=100,
                     n.gens=1000,
                     s=5,
                     p.mutate=0.01,
                     p.sex=0.5,
                     p.rec=0.25,
                     fitness=f, v.mat, bloom.times) {
  res <- run.ga(n.plants=k,
                n.plants.tot=ncol(v.mat),
                N=N,
                n.gens=n.gens,
                s=s,
                p.mutate=p.mutate,
                p.sex=p.sex,
                p.rec=p.rec,
                fitness=f, v.mat, bloom.times)
  colnames(v.mat)[which(res$best.model)]
}









### forcing including crop ###

ga.step.2 <- function(N, state, s, p.mutate, p.sex, p.rec, fitness, v.mat, bloom.times, crop.n) {
  ## Mutation
  state.m <- update.state(state, mutate.2(state=state,
                                        p.mutate=p.mutate,
                                        fitness=fitness, v.mat, bloom.times, crop.n))
  
  ## Selection
  state.s <- update.state(state, select(state=state.m, s=s))
  
  ## Recombination
  state.r <- update.state(state, recombine(N, state=state.s,
                                           popn0=state$popn,
                                           p.sex=p.sex, p.rec=p.rec,
                                           fitness=fitness, v.mat, bloom.times))
  state.r
}


initial.popn.2 <- function(N, n.plants, n.plants.tot, fitness, v.mat, bloom.times, crop){
  
  crop.n <- which(names(bloom.times) == crop)
  
  make.model <- function(x, n.plants, n.plants.tot) {
    m <- rep(FALSE, n.plants.tot)
    m[sample(n.plants.tot, n.plants)] <- TRUE
    m[crop.n] <- TRUE
    m
  }
  
  popn <- lapply(1:N, make.model, n.plants, n.plants.tot)
  w <- sapply(popn, fitness, v.mat, bloom.times)
  list(popn=popn,
       w=w,
       best.w=max(w),
       best.model=popn[[which.max(w)]])
}

state <- initial.popn.2(N, n.plants, n.plants.tot,
                  fitness=fitness, v.mat, bloom.times,crop)
out <- vector("numeric", n.gens)
for ( i in seq_len(n.gens) ){
  x <- ga.step.2(N, x, s, p.mutate, p.sex, p.rec, fitness, v.mat, bloom.times, crop.n)
  out[i] <- x$best.w
}


mutate.1.2 <- function(x, n.mutants, crop.n) {
  if(n.mutants > sum(x))
    cat("Mutation rate too high. Too many mutants\n")
  
  x1 <- sample(which(x)[which(x) != crop.n], size=n.mutants)
  x2 <- sample(which(!x), size=n.mutants)
  x[x1] <- FALSE
  x[x2] <- TRUE
  x
}

mutate.2 <- function(state, p.mutate, fitness, v.mat, bloom.times, crop.n) {
  
  popn <- state$popn
  w <- state$w
  nmutants <- rbinom(length(popn), length(popn[[1]]), p.mutate)
  nmutants <- sapply(nmutants, function(x) min(sum(popn[[1]]), x))
  mutated <- which(nmutants>0)
  
  if ( length(mutated) > 0 ) {
    popn[mutated] <-
      mapply(mutate.1.2, popn[mutated], nmutants[mutated],
             SIMPLIFY=FALSE, crop.n)
    w[mutated] <- sapply(popn[mutated], fitness, v.mat, bloom.times)
  }
  list(popn=popn,
       w=w,
       best.w=state$best.w,
       best.model=state$best.model)
}



