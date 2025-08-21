# Rscript for simulating data

########################################
# Community simulation functions #######
########################################
# Inverse logit function
inv.logit <- function(x)
  exp(x) / (exp(x)+1)
# Environmental filtering
make.env.comm <- function(trait, env, occupancy, tree, rescale=TRUE){
  data <- expand.grid(species=names(trait), site=names(env), stringsAsFactors=FALSE)
  data$trait <- trait[data$species]
  data$env <- env[data$site]
  data$occupancy <- occupancy[data$species]
  data$num_species <- as.numeric(gsub("s","",data$species))
  data$num_sites <- as.numeric(factor(data$site))
  data$prob <- data$trait * data$env + data$occupancy
  if(rescale)
    data$prob <- inv.logit(data$prob)
  data$presence <- rbinom(nrow(data), 1, data$prob)
  return(data)
}
# Scramble competition
make.comp.comm <- function(comp.trait, n.spp, env, tree, scale=2){
  # Make overdispersed VCV from trait
  vcv <- as.matrix(dist(comp.trait))
  vcv <- max(vcv) - vcv
  vcv <- diag(length(env)) %x% solve(vcv)
  
  # Make overdispersed VCV from phylogeny
  # vcv <- solve(vcv(tree))/scale
  # vcv <- diag(length(env)) %x% vcv
  
  # Draw presences/absences
  pres.abs <- matrix(inv.logit(rmvnorm(1, sigma=vcv)), nrow=length(env), byrow=TRUE)
  for(i in seq_len(nrow(pres.abs)))
    pres.abs[i,] <- ifelse(pres.abs[i,]>=sort(pres.abs[i,], decreasing=TRUE)[sample(n.spp,1)], 1, 0)
  comm <- matrix(as.numeric(t(pres.abs)), nrow=length(env), ncol=length(comp.trait), dimnames=list(names(env), tree$tip.label), byrow=TRUE)
  
  # Format data and return
  data <- comparative.comm(tree, comm, traits=data.frame(trait=comp.trait), env=data.frame(env=env))
  data <- as.data.frame(data)
  data$site <- factor(data$site)
  data$species <- factor(data$species)
  data$num_species <- as.numeric(gsub("s","",data$species))
  data$num_sites <- as.numeric(factor(data$site))
  return(data)
}
# Association simulation
make.assoc <- function(base.data, base.tree, base.trait, partner.trait){
  # Make prob. of association matrix
  assoc <- 1 - plnorm(abs(outer(base.trait, partner.trait, FUN=`-`)))
  
  # Add in partner interactions on basis of probabilities
  partner.comm <- matrix(0, nrow=nrow(base.data), ncol=length(partner.trait))
  for(i in seq_len(nrow(base.data))){
    if(base.data$presence[i]==1)
      partner.comm[i,] <- as.numeric(runif(length(partner.trait)) <= assoc[base.data$num_species[i],])
  }
  
  # Reformat data and return
  data <- base.data[rep(seq_len(nrow(base.data)), length(partner.trait)),]
  names(data)[c(6,8,9)] <- c("num_base_species","base_prob","base_presence")
  data$num_partner_species <- rep(seq_along(partner.trait), each=nrow(partner.comm))
  data$partner_presence <- as.numeric(partner.comm)
  return(data)
}

# simulate_data
simulate_data <- function(seed) {
  
  set.seed(seed)
  
  # Basics
  n.spp     <- 50
  tree      <- sim.bdtree(n = n.spp, seed = seed)
  n.sites   <- 50
  site.names<- apply(as.matrix(expand.grid(letters, letters, stringsAsFactors = FALSE)),
                     1, paste, collapse = "")[seq_len(n.sites)]
  env.sites <- setNames(rnorm(n.sites), site.names)
  
  # Overall occupancies and traits
  cons.occ   <- sim.char(tree, .05, root = 0)[, , 1]
  lab.occ    <- setNames(sample(cons.occ), names(cons.occ))
  cons.trait <- sim.char(tree, .05, root = 0)[, , 1]
  lab.trait  <- setNames(sample(cons.trait), names(cons.trait))
  comp.occ   <- setNames(rmvnorm(1, rep(0, n.spp), solve(vcv(tree)) / 10)[1, ],
                         tree$tip.label)
  comp.trait <- setNames(rmvnorm(1, rep(0, n.spp), solve(vcv(tree)) / 10)[1, ],
                         tree$tip.label)
  
  # Datasets
  cons.env <- make.env.comm(cons.trait, env.sites, lab.occ, tree)
  ghost.comp <- make.env.comm(comp.trait, env.sites, lab.occ, tree)
  s.tree <- drop.tip(tree, 16:50)
  cons.comp <- make.comp.comm(cons.occ[1:15], c(2,5), env.sites[1:15], s.tree)
  
  # Association dataset
  base.n.spp      <- 10
  partner.n.spp   <- 10
  base.tree       <- sim.bdtree(n = base.n.spp,    seed = seed + 111)
  partner.tree    <- sim.bdtree(n = partner.n.spp, seed = seed + 222)
  assoc.n.sites   <- 10
  assoc.env.sites <- setNames(rnorm(assoc.n.sites),
                              letters[seq_len(assoc.n.sites)])
  
  base.env.trait     <- sim.char(base.tree, .05, root = 0)[, , 1]
  base.occ           <- setNames(sample(sim.char(base.tree, .05, root = 0)[, , 1]),
                                 names(base.env.trait))
  base.assoc.trait   <- sim.char(base.tree, .05, root = 0)[, , 1]
  partner.assoc.trait<- sim.char(partner.tree, .05, root = 0)[, , 1]
  
  base.env   <- make.env.comm(base.env.trait, assoc.env.sites, base.occ, base.tree)
  assoc.data <- make.assoc(base.env, base.tree, base.assoc.trait, partner.assoc.trait)
  
  # Compile data needed for each model
  one_two_data = list(Ntotal = nrow(cons.env), Nspp = n.spp, presence = cons.env$presence, env = cons.env$env, spp = cons.env$num_species)
  one_three_data = list(Ntotal = nrow(cons.env), Nspp = n.spp, presence = cons.env$presence, env = cons.env$env, spp = cons.env$num_species, Vphy = vcv(tree))
  two_one_data = list(Ntotal = nrow(ghost.comp), Nspp = max(ghost.comp$num_species), presence = ghost.comp$presence, env = ghost.comp$env, spp = ghost.comp$num_species, Vphy = vcv(tree), inv_Vphy = solve(vcv(tree)))
  two_two_data = list(Ntotal = nrow(cons.comp), Nspp = max(cons.comp$num_species), Nsite = max(cons.comp$num_sites), presence = cons.comp$presence, spp = cons.comp$num_species, InvPhySite = (solve(vcv(s.tree)) / 10) %x% diag(max(cons.comp$num_sites)))
  three_one_data_b = list(Ntotal = nrow(assoc.data), Nspp = base.n.spp, presence = assoc.data$base_presence, env = assoc.data$env, spp = assoc.data$num_base_species, Vphy = vcv(base.tree), Npartner = partner.n.spp, partner = assoc.data$num_partner_species, VpartnerPhy = vcv(partner.tree))
  three_one_data_p = list(Ntotal = nrow(assoc.data), Nspp = partner.n.spp, presence = assoc.data$partner_presence, env = assoc.data$env, spp = assoc.data$num_partner_species, Vphy = vcv(partner.tree), Npartner = base.n.spp, partner = assoc.data$num_base_species, VpartnerPhy = vcv(base.tree))
  
  # Explicitly return ALL relevant variables
  list(
    # datasets
    cons.env   = cons.env,
    ghost.comp = ghost.comp,
    cons.comp  = cons.comp,
    assoc.data = assoc.data,
    
    # supporting objects
    tree         = tree,
    s.tree       = s.tree,
    base.tree    = base.tree,
    partner.tree = partner.tree,
    cons.trait   = cons.trait,
    lab.occ      = lab.occ,
    n.spp        = n.spp,
    
    # Explicitly return these numbers
    base.n.spp    = base.n.spp,
    partner.n.spp = partner.n.spp,
    
    # “true” parameters
    true = list(
      cons.trait = cons.trait,
      lab.occ    = lab.occ,
      comp.trait = comp.trait,
      # SECTION THAT NEEDS WORK:
      base_env_trait        = base.env.trait,
      base_occ              = base.occ,
      base_assoc_trait      = base.assoc.trait,
      partner_assoc_trait   = partner.assoc.trait),
    # Model compilations
    one_two_data = one_two_data,
    one_three_data = one_three_data,
    two_one_data = two_one_data,
    two_two_data = two_two_data,
    three_one_data_b = three_one_data_b,
    three_one_data_p = three_one_data_p
  )
}