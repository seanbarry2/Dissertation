# Models

########################################
########################################
# Models to be fitted ##################
########################################
########################################

# Stan model code strings
one.two.stan.code <- "
// Tell rstan what our data are
data{
int Ntotal;              // Number of rows in our dataset
int Nspp;                // Number of species
int presence[Ntotal];    // The response variable (presence/absence)
real env[Ntotal];        // An explanatory variable (environment)
int spp[Ntotal];         // An explanatory variable (species)
}
// The parameters/coefficients we want to estimate and report back
parameters{
vector[Nspp] spp_int;  // Each species' overall occupancy
vector[Nspp] spp_slp;      // Each species' environmental response
}
// Some calculations rstan is going to perform internally to help model-fitting
transformed parameters{
vector[Ntotal] predictions;   // Make a variable to hold our model predictions
// Loop over all our input data and specify our model, which is:
// a species' intercept (overall occupancy) +  env response x the environment
for (i in 1:Ntotal)
predictions[i] = spp_int[spp[i]] + spp_slp[spp[i]]*env[i];
}
// Fit our model to our predictions
model{
// Species' occupancies and responses are drawn from uninformative priors
spp_int ~ normal(0, 0.5); 
spp_slp ~ normal(0, 0.5);
// The model itself: our presences are drawn from our predictions
presence ~ bernoulli_logit(predictions);
}
////////////////////////////////////////////////////////////////
////////  >>>  FOR ESTIMATING PARETO-K AFTERWARDS  <<<  ////////
generated quantities {
  vector[Ntotal] log_lik;
  for (i in 1:Ntotal)
    log_lik[i] = bernoulli_logit_lpmf(presence[i] | predictions[i]);
}
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
"

one.three.stan.code <- "
// Function to transform a phylogenetic VCV according to Pagel's Lambda
// - and multiply through by overall sigma
functions {
matrix lambda_vcv(matrix vcv, real lambda, real sigma){
matrix[rows(vcv),cols(vcv)] local_vcv; // Make a local copy of the VCV to modify
local_vcv = vcv * lambda;              // Lambda transforms are just a multiplier
for(i in 1:rows(local_vcv))            // ... but we do have to loop over the matrix
local_vcv[i,i] = vcv[i,i];           // ... and make the diagonal the same as it was before
return(local_vcv * sigma);             // Return the transformed matrix x sigma (overall variation)
}
}
data {
int Ntotal;
int Nspp;
int presence[Ntotal];
real env[Ntotal];
int spp[Ntotal];
//    
matrix[Nspp,Nspp]Vphy;     // Give the phylogeny as data
}
parameters{
vector[Nspp] spp_int;
vector[Nspp] spp_slp;
// Lambda transforms for the intercepts and slopes
real<lower=0, upper=1> lam_int;     // (priors --> cannot be negative) 
real<lower=0, upper=1> lam_slp;
// Coefficients for the NON-phylogenetically-derived variance in model terms
real<lower=0> null_int;    // (priors --> cannot be negative)
real<lower=0> null_slp;
// Coefficients for the mean intercepts/slopes
real mean_int;
real mean_slp;
}
transformed parameters{
vector[Ntotal] predictions;
for (i in 1:Ntotal)
predictions[i] = spp_int[spp[i]] + spp_slp[spp[i]]*env[i];
}
model{
// Specify priors
mean_int ~ normal(0,0.5);
mean_slp ~ normal(0,0.5);
lam_int ~ normal(0,0.5);
lam_slp ~ normal(0,0.5);
null_int ~ normal(0,0.5);
null_slp ~ normal(0,0.5);

// Now we draw our species coefficients, incorporating the lambda-transformed phylogeny
spp_int ~ multi_normal(rep_vector(mean_int,Nspp), lambda_vcv(Vphy,lam_int,null_int));
spp_slp ~ multi_normal(rep_vector(mean_slp,Nspp), lambda_vcv(Vphy,lam_slp,null_slp)); 
//
presence ~ bernoulli_logit(predictions);
}
////////////////////////////////////////////////////////////////
////////  >>>  FOR ESTIMATING PARETO-K AFTERWARDS  <<<  ////////
generated quantities {
  vector[Ntotal] log_lik;
  for (i in 1:Ntotal)
    log_lik[i] = bernoulli_logit_lpmf(presence[i] | predictions[i]);
}
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
"

two.one.stan.code <- "
data{
int Ntotal;
int Nspp;
int presence[Ntotal];
real env[Ntotal];
int spp[Ntotal];
matrix[Nspp,Nspp] Vphy;
matrix[Nspp,Nspp] inv_Vphy; // The inverse of the phylogeny
//                             (used to measure repulsive evolution)
}
parameters{
vector[Nspp] spp_int;
vector[Nspp] spp_slp;
real<lower=0> phy_int;
real<lower=0> phy_slp;
real<lower=0> null_int;
real<lower=0> null_slp;
// Coefficients for the inverse-phylogenetically-derived variance in model terms
real<lower=0> invphy_int;       // (with priors specified too) 
real<lower=0> invphy_slp;       //
real mean_int;
real mean_slp;
}
transformed parameters{
vector[Ntotal] predictions;
for (i in 1:Ntotal)
predictions[i] = spp_int[spp[i]] + spp_slp[spp[i]]*env[i];
}
model{
// Specify priors
mean_int ~ normal(0,0.5);
mean_slp ~ normal(0,0.5);
phy_int ~ normal(0,0.5);
phy_slp ~ normal(0,0.5);
invphy_int ~ normal(0,0.5);
invphy_slp ~ normal(0,0.5);
null_int ~ normal(0,0.5);
null_slp ~ normal(0,0.5);

// Now we draw our species coefficients to measure the importance of phylogeny and its inverse
// - notice how we're drawing from a covariance matrix with null (non-phylogenetic), phylogenetic, and the inverse of the phylogenetic, components
spp_int ~ multi_normal(rep_vector(mean_int,Nspp), diag_matrix(rep_vector(null_int,Nspp)) + phy_int*Vphy + invphy_int*inv_Vphy); 
spp_slp ~ multi_normal(rep_vector(mean_slp,Nspp), diag_matrix(rep_vector(null_slp,Nspp)) + phy_slp*Vphy + invphy_slp*inv_Vphy);
presence ~ bernoulli_logit(predictions);
}
////////////////////////////////////////////////////////////////
////////  >>>  FOR ESTIMATING PARETO-K AFTERWARDS  <<<  ////////
generated quantities {
  vector[Ntotal] log_lik;
  for (i in 1:Ntotal)
    log_lik[i] = bernoulli_logit_lpmf(presence[i] | predictions[i]);
}
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
"

two.two.stan.code <- "
data{
int Ntotal;
int presence[Ntotal];
matrix[Ntotal,Ntotal] InvPhySite;   // Matrix of phylogeny x sites to detect competition
}
parameters{
real<lower=0.0001, upper=10> null_sites;       // Null variance of sites
real<lower=0.0001, upper=10> inv_sites;        // Phylogenetic repulsion of sites
vector[Ntotal] predictions;
}
model{
predictions ~ multi_normal(rep_vector(0,Ntotal), diag_matrix(rep_vector(fabs(null_sites),Ntotal)) + fabs(inv_sites)*InvPhySite);
presence ~ bernoulli_logit(predictions);
}
////////////////////////////////////////////////////////////////
////////  >>>  FOR ESTIMATING PARETO-K AFTERWARDS  <<<  ////////
generated quantities {
  vector[Ntotal] log_lik;
  for (i in 1:Ntotal)
    log_lik[i] = bernoulli_logit_lpmf(presence[i] | predictions[i]);
}
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
"

three.one.stan.code <- "
functions {
matrix lambda_vcv(matrix vcv, real lambda, real sigma){
matrix[rows(vcv),cols(vcv)] local_vcv; // Make a local copy of the VCV to modify
local_vcv = vcv * lambda;              // Lambda transforms are just a multiplier
for(i in 1:rows(local_vcv))            // ... but we do have to loop over the matrix
local_vcv[i,i] = vcv[i,i];           // ... and make the diagonal the same as it was before
return(local_vcv * sigma);             // Return the transformed matrix x sigma (overall variation)
}
}
data{
int Ntotal;
int Nspp;
int presence[Ntotal];
real env[Ntotal];
int spp[Ntotal];
matrix[Nspp,Nspp]Vphy;
//
int Npartner;                              // # of partner species (c.f., Nspp)
int partner[Ntotal];                       // Give partner presence as data
matrix[Nspp,Nspp]VpartnerPhy;              // Partner phylogeny
}
parameters{
vector[Nspp] spp_int;
vector[Nspp] spp_slp;
vector[Nspp] partner_int;                    // Allow for partner presence/absence to predict base species' presence/absence
real<lower=0, upper=1> lam_int;
real<lower=0, upper=1> lam_slp;
real<lower=0> null_int;
real<lower=0> null_slp;
real mean_int;
real mean_slp;
// Coefficients for the partner-derived variance in model terms
real<lower=0, upper=1> lam_partner_int;     // (phylogenetic component)
real<lower=0> null_partner_int;    // (non-phylogenetic)
real mean_partner_int;             // (and the overall mean here)
}
transformed parameters{
vector[Ntotal] predictions;
for (i in 1:Ntotal)
predictions[i] = spp_int[spp[i]] + spp_slp[spp[i]]*env[i] + partner_int[partner[i]]*partner[i];
// Notice extra term added at the end for the partner (analogous to spp_intercepts)
}
model{
// Priors
lam_int ~ normal(0, 0.5);
lam_slp ~ normal(0, 0.5);
null_int ~ normal(0, 0.5);
null_slp ~ normal(0, 0.5);
lam_partner_int ~ normal(0, 0.5);
null_partner_int ~ normal(0, 0.5);
mean_int ~ normal(0, 0.5);
mean_slp ~ normal(0, 0.5);
mean_partner_int ~ normal(0, 0.5);
spp_int ~ multi_normal(rep_vector(mean_int,Nspp), lambda_vcv(Vphy,lam_int,null_int));
spp_slp ~ multi_normal(rep_vector(mean_slp,Nspp), lambda_vcv(Vphy,lam_slp,null_slp));
partner_int ~ multi_normal(rep_vector(mean_partner_int,Npartner), lambda_vcv(VpartnerPhy,lam_partner_int,null_partner_int));
// Notice the intercepts term is the same as the above, but now we address the partner species
presence ~ bernoulli_logit(predictions);
}
////////////////////////////////////////////////////////////////
////////  >>>  FOR ESTIMATING PARETO-K AFTERWARDS  <<<  ////////
generated quantities {
  vector[Ntotal] log_lik;
  for (i in 1:Ntotal)
    log_lik[i] = bernoulli_logit_lpmf(presence[i] | predictions[i]);
}
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
"