#######
## POWELL Problem
#######

#solnp(pars, fun, eqfun = NULL, eqB = NULL, ineqfun = NULL, ineqLB = NULL,
#      ineqUB = NULL, LB = NULL, UB = NULL, control = list(), ...)
# Arguments
#    pars     The starting parameter vector.
#    fun      The main function which takes as first argument the parameter vector and 
#             returns a single value. to be MINIMIZED
#    eqfun    The equality constraint function returning the vector of evaluated 
#             equality constraints.
#    eqB      The equality constraints.
#    ineqfun  The inequality constraint function returning the vector of evaluated
#             inequality constraints.
#    ineqLB   The lower bound of the inequality constraints.
#    ineqUB   The upper bound of the inequality constraints.
#    LB       The lower bound on the parameters.
#    UB       The upper bound on the parameters.
#
#   *eqfunとeqBはセットで使う。等号制約条件（Optional）
#   *ineqfunとineqLBとineqUBはセットで使う。不等号制約条件（Optional）
#   *LBとUBはセットで使う。変数の下限と上限(Optional)
# Value
#   A list containing the following values:
#     pars        Optimal Parameters.
#     convergence Indicates whether the solver has converged (0) or not (1 or 2).
#     values      vector of function values during optimization with last one the value at the optimal.
#     lagrange    The vector of Lagrange multipliers.
#     hessian     The Hessian of the augmented problem at the optimal solution.
#     ineqx0      The estimated optimal inequality vector of slack variables used for
#                 transforming the inequality into an equality constraint.
#     nfuneval    The number of function evaluations.
#     elapsed     Time taken to compute solution.

#Function to be MINIMIZED
fn1=function(x)
{
  exp(x[1]*x[2]*x[3]*x[4]*x[5])
}

#Equality constraint function
eqn1=function(x){
  z1=x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
  z2=x[2]*x[3]-5*x[4]*x[5]
  z3=x[1]*x[1]*x[1]+x[2]*x[2]*x[2]
  return(c(z1,z2,z3))
}
#Starting parameter vector
x0 = c(-2, 2, 2, -1, -1)

# 制約条件
#   x1^2+x2^2+x3^2+x4^2+x5^2 = 10
#   x2*x3-5*x4*x5 = 0
#   x1^3+x2^3 = -1
(powell=solnp(x0, fun = fn1, eqfun = eqn1, eqB = c(10, 0, -1)))
#結果は基本的にLocal Minimum。複雑な問題の場合は初期値を色々変えて試す

###EOF POWELL Problem

#######
##  startpars Example
#######

#Funtion to be MINIMIZED
gofn = function(dat, n)
{
  x = dat[1:n]
  y = dat[(n+1):(2*n)]
  z = dat[(2*n+1):(3*n)]
  ii = matrix(1:n, ncol = n, nrow = n, byrow = TRUE)
  jj = matrix(1:n, ncol = n, nrow = n)
  ij = which(ii<jj, arr.ind = TRUE)
  i = ij[,1]
  j = ij[,2]
  # Coulomb potential
  potential = sum(1.0/sqrt((x[i]-x[j])^2 + (y[i]-y[j])^2 + (z[i]-z[j])^2))
  potential
}

#Equality constraint function
goeqfn = function(dat, n)
{
  x = dat[1:n]
  y = dat[(n+1):(2*n)]
  z = dat[(2*n+1):(3*n)]
  apply(cbind(x^2, y^2, z^2), 1, "sum")
}

n_cl=20
#The lower bound on the parameters.ここではすべての変数が-1
LB=rep(-1,3*n_cl)
#The upper bound on the parameters.ここではすべての変数が1
UB=rep(1,3*n_cl)
#等号制約条件のすべての等号値が1
eqB=rep(1, n_cl)

#Starting Paramter?̃T???v???Q???????֐?
# startpars(pars = NULL, fixed = NULL, fun, eqfun = NULL, eqB = NULL,
#          ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, LB = NULL, UB = NULL,
#          distr = rep(1, length(LB)), distr.opt = list(), n.sim = 20000, cluster = NULL,
#          rseed = NULL, bestN = 15, eval.type = 1, trace = FALSE, ...)
# Arguments
#
#   pars      The starting parameter vector. This is not required unless the fixed option 
#             is also used.
#   fixed     The numeric index which indicates those parameters which should stay fixed
#             instead of being randomly generated.
#   fun       The main function which takes as first argument the parameter vector and returns
#             a single value.
#   eqfun     The equality constraint function returning the vector of evaluated equality constraints.
#   eqB       The equality constraints.
#   ineqfun   The inequality constraint function returning the vector of evaluated inequality constraints.
#   ineqLB    The lower bound of the inequality constraints.
#   ineqUB    The upper bound of the inequality constraints.
#   LB        The lower bound on the parameters. This is not optional in this function.
#   UB        The upper bound on the parameters. This is not optional in this function.
#   distr     A numeric vector of length equal to the number of parameters, indicating the
#             choice of distribution to use for the random parameter generation. Choices are
#             uniform (1), truncated normal (2), and normal (3).
#   distr.opt If any choice in distr was anything other than uniform (1), this is a list equal
#             to the length of the parameters with sub-components for the mean and sd, which
#             are required in the truncated normal and normal distributions.
#   bestN     The best N (less than or equal to n.sim) set of parameters to return.
#   n.sim     The number of random parameter sets to generate.
#   cluster   If you want to make use of parallel functionality, initialize and pass a cluster
#             object from the parallel package (see details), and remember to terminate it!
#   rseed     A seed to initiate the random number generator, else system time will be used.
#   eval.type Either 1 (default) for the direction evaluation of the function (excluding inequality
#             constraint violations) or 2 for the penalty barrier method.
#   trace     Whether to display the progress of the function evaluation.
#   ...       Additional parameters passed to the main, equality or inequality functions
#
# Value
#   A matrix of dimension bestN x (no.parameters + 1). The last column is the evaluated function value.

# ここでは上記の...の引数としてn=n_cl(25)が渡されている事に注意
# n=c_cl(25)はpars以外の引数として、最適化関数(gofn)と等号制約条件関数(goeqfn)
# に引数として追加で渡される。
best_n_cl<-15
sp = startpars(pars = NULL, fixed = NULL, fun = gofn , eqfun = goeqfn,
               eqB = eqB, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, LB = LB, UB = UB,
               distr = rep(1, length(LB)), distr.opt = list(), n.sim = 2000,
               bestN = best_n_cl, eval.type = 2, n = n_cl)

# the last column is the value of the evaluated function (here it is the barrier
# function since eval.type = 2)
#
# print(round(apply(sp, 2, "mean"), 3))

# remember to remove the last column
# sp[1,-76:-76]はmatrix spの76列を除去したもの。上の最後の列をremoveせよとの指示と合っている
# 例えば、sp[1,-73]はmatrix spの73列を除去したもの
#         sp[1,-72:-76]はmatrix spの72から76列を除去したもの
for(i in 1:best_n_cl){
  eq2 = solnp(pars=sp[1,-n_cl:-n_cl],fun = gofn , eqfun = goeqfn , eqB = eqB, ineqfun = NULL,
              ineqLB = NULL, ineqUB = NULL, LB = LB, UB = UB, n = n_cl)
  if(i==1){
    eq_v <- c(eq2)
  }
 else {
    eq_v <- append(eq_v,eq2)
 }
}
#eq2 = solnp(pars=sp[2,-n_cl:-n_cl],fun = gofn , eqfun = goeqfn , eqB = eqB, ineqfun = NULL,
#                ineqLB = NULL, ineqUB = NULL, LB = LB, UB = UB, n = n_cl)

#eq2[_ii] = solnp(pars=sp[_ii,-n_cl:n_cl],fun = gofn , eqfun = goeqfn , eqB = eqB, ineqfun = NULL,
#               ineqLB = NULL, ineqUB = NULL, LB = LB, UB = UB, n = n_cl)
# should get a value of around 243.8162 for the case of n==25
## End(Not run)
