# Mixed integer nonlinear programming
# Designing a pressure vessel
# Case B: solved according to the original problem statements
# steel plate available in thicknesses multiple
# of 0.0625 inch
#
# wall thickness of the
# shell 1.1 [18*0.0625] <= x1 <= 12.5 [200*0.0625]
# heads 0.6 [10*0.0625] <= x2 <= 12.5 [200*0.0625]
# 0.0 <= x3 <= 240.0, 0.0 <= x4 <= 240.0
# The global optimum is (x1, x2, x3, x4; f) =
# (1.125 [18*0.0625], 0.625 [10*0.0625],
# 58.29016, 43.69266; 7197.729).

#JDEoptim(lower, upper, fn,
#         constr = NULL, meq = 0, eps = 1e-05,
#         NP = 10*d, Fl = 0.1, Fu = 1,
#         tau1 = 0.1, tau2 = 0.1, tau3 = 0.1,
#         jitter_factor = 0.001,
#         tol = 1e-15, maxiter = 200*d, fnscale = 1,
#         FUN = c("median", "max"),
#         add_to_init_pop = NULL, trace = FALSE, triter = 1,
#         details = FALSE, ...)
#
#  constr an optional function for specifying the nonlinear constraints under which we
#         want to minimize fn. They should be given in the form hi(x) = 0; gi(x) <= 0.
#         This function takes the vector of parameters as its first argument and returns a
#         real vector with the length of the total number of constraints. It defaults to NULL,
#         meaning that bound-constrained minimization is used.
#  meq    an optional positive integer specifying that the first meq constraints are treated
#         as equality constraints, all the remaining as inequality constraints. Defaults to 0
#         (inequality constraints only).

pressure_vessel_B <-
  list(
    # Objecttive Function to be MINIMIZED. List element 1
    obj = function(x) {
      # floor, trunc, roundの違い
      # 利用する問題によってどれを使うのが適切か判断する。
      # Optionの売買最適化問題では、
      #  LongとShortに中立ならround()
      #  Shortを中心に探すならfloor()
      #  Longを中止に探すならceiling()
      #  Positionを取ることにPenaltyを与える（特にATM)にはtrunc()
      # ( x1 <- seq(-2, 2, by = .25) )
      # -2.00 -1.75 -1.50 -1.25 -1.00 -0.75 -0.50 -0.25  0.00  0.25  0.50  0.75  1.00  1.25  1.50  1.75  2.00
      # round(x1)
      #    -2    -2    -2    -1    -1    -1     0     0     0     0     0     1     1     1     2     2     2
      # floor(x1)
      #    -2    -2    -2    -2    -1    -1    -1    -1     0     0     0     0     1     1     1     1     2
      # ceiling(x1)
      #    -2    -1    -1    -1    -1     0     0     0     0     1     1     1     1     2     2     2     2
      # trunc(x1)
      #    -2    -1    -1    -1    -1     0     0     0     0     0     0     0     1     1     1     1     2
      
      # ここでは、x1とx2だけ整数制約を課す
      # 
      x1 <- floor(x[1])*0.0625
      x2 <- floor(x[2])*0.0625
      x3 <- x[3]; x4 <- x[4]
      0.6224*x1*x3*x4 + 1.7781*x2*x3^2 +
        3.1611*x1^2*x4 + 19.84*x1^2*x3
   },
   # Constraint を表現する式のVector
   #   0.0193*x3 - x1
   #   0.00954*x3 - x2
   #   750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3
  con = function(x) {
    x1 <- floor(x[1])*0.0625
    x2 <- floor(x[2])*0.0625
    x3 <- x[3]; x4 <- x[4]
    c(0.0193*x3 - x1,
      0.00954*x3 - x2,
      750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3)
  })

# 最初の2つのVectorはx1:x4 の下限と上限
#  18 <(=) x1 < 201, 10 <(=)x2 < 201, 0.0 < x3 < 240.0, 0.0 < x4 < 240.0
#    (=) は floor()の影響でx1は18(x2の場合10)の値は取りうる。
#  
# meq (Default) = 0
#  最初の0個までの制約が等式(=0)条件
#   この場合0なので、すべての条件は不等式(<=0)条件
#     0.0193*x3 - x1 <= 0
#     0.00954*x3 - x2 <= 0
#     750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3 <= 0
#  上記目的関数でx1 と x2は整数制約を課していることに注意 

res <- JDEoptim(c( 18, 10, 0.0, 0.0),
                c(200+1, 200+1, 240.0, 240.0),
                fn = pressure_vessel_B$obj,
                constr = pressure_vessel_B$con,
                tol = 1e-7, trace = TRUE, triter = 50)
res
# Now convert to integer x1 and x2
c(floor(res$par[1:2]), res$par[3:4])
