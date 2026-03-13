!#####
! MODULE TO STORE GLOBAL VARIABLES
!#####
module globals
    
implicit none


! MODEL SIZE

! maximum number of children
integer, parameter :: NN = 3

! number of fertile periods
integer, parameter :: TT = 8

! number of simulated couples
integer, parameter :: SS = 100000

! number of wage levels
integer, parameter :: WW = 6

! gender definitions
integer, parameter :: f = 0
integer, parameter :: m = 1

! education definitions
integer, parameter :: al = 0
integer, parameter :: hs = 1
integer, parameter :: co = 2


! MODEL PARAMETERS

! economies of scale
real*8 :: alpha

! time discount factor
real*8 :: beta

! child preferences
real*8 :: mu(0:NN-1, f:m, hs:co), sig(f:m), rho, pi

! conception rates
real*8 :: nu(0:NN, f:m, f:m, hs:co) = 0d0

! utility child cost distribution (cultural)
real*8 :: chi(f:m), chi_base

! utility and monetary cost of children
real*8 :: phi_c, phi_u

! fictitious child care wage
real*8 :: w_y, w_y_base

! statistics of the female wage
real*8 :: w_f_bar(hs:co), dist_hc(hs:co), p_cost

! subsidies to child costs (for explanation see policy_analysis.f90)
real*8 :: sub(0:5, 0:NN-1) = 0d0

!$omp threadprivate(mu, sig, rho, pi, chi, phi_u, w_y, p_cost)


! MODEL SOLUTIONS

! the actual preferences
real*8 :: v_g(f:m, SS, 0:NN-1, hs:co)

! wage of the female spouse
real*8 :: w_f(WW, hs:co), dist_w(WW, hs:co)

! decisions at the respective points
integer :: i_g(f:m, SS, 0:NN, TT, WW, hs:co)

! decisions at the respective points
real*8 :: eta_g(f:m, SS, 0:NN)

! choice of labor force participation of the woman
integer :: h(SS, 0:NN, TT, WW, hs:co)

! utility levels
real*8 :: X_t(f:m, SS, 0:NN, TT+1, WW, hs:co)

! the distributions
real*8 :: distrib(SS, 0:NN, TT, WW, al:co)

! total distribution over number of kids
real*8 :: tot_dist(0:NN, al:co)

! should wages be discretizes
logical :: do_wages = .true.

!$omp threadprivate(v_g, w_f, i_g, eta_g, h, X_t, distrib, tot_dist)


! MODEL AGGREGATES

! fertility intention matrices by number of children
real*8 :: fert_int(f:m, f:m, -1:NN, al:co)

! fertility outcomes by number of children
real*8 :: fert_out(f:m, f:m, -1:NN, al:co)

! conditional distribution tables
real*8 :: fert_int_cond(f:m, f:m, f:m, f:m, al:co)

! fertility rates of the population
real*8 :: fert_rat(al:co)

! avergae age at first and last birth
real*8 :: fert_age_first(al:co), fert_age_last(al:co)

! female labor force participation rate
real*8 :: LFP_with(-1:NN-1, al:co), LFP_without(-1:NN-1, al:co)

!$omp threadprivate(fert_int, fert_out, fert_int_cond, fert_rat, LFP_with, LFP_without)


! OTHER STUFF

! simulated normal distribution for wage heterogeneity
real*8 :: realizations(SS, 2)

! present value costs
real*8 :: t_phi_u(f:m, 0:NN-1), t_phi_c(f:m, 0:NN-1)

! total cost to women and men under differnt child rearing arrangements
real*8 :: t_phi(0:1, f:m, 0:NN-1, WW, hs:co)

! limited commitment thresholds
real*8 :: limited(f:m, 0:NN-1, WW, hs:co)

!$omp threadprivate(t_phi_u, t_phi_c, t_phi, limited)


! SIMULATION PARAMETERS

logical :: estimate = .false.
logical :: out = .false.
logical :: policy = .false.
logical :: simulate_new_shocks = .false.
logical :: limited_commitment = .true.


! ESTIMATION

! number of parameters to estimate
integer, parameter :: NEST = 21

! minimum and maximum of parameters, etc.
real*8 :: p(NEST), RSS_est = 0d0

! storage values for estimation
real*8 :: sig_base(f:m), mu_base(NN,f:m, hs:co), t_phi_u_base(f:m, NN)

! blocking variables
real*8 :: fblock(0:NN-1, 2), mblock(0:NN-1, 2), LFP_with_cc(2)

!$omp threadprivate(fblock, mblock, LFP_with_cc)



! TARGETS

! fertility intentions
real*8 :: fert_int_targ(f:m, f:m, 0:NN-1, hs:co)

! persistence in fertility decisions
real*8 :: persist_targ(f:m, f:m)

! labor force participation
real*8 :: LFP_without_targ(hs:co), LFP_with_targ(hs:co)

! cross country
real*8 :: chi_cc(2), w_y_cc(2), LFP_cc(2), fbshare_cc(1:2, 2), mbshare_cc(1:2, 2), cons_cc(2), beta_cc(2)



! DIFFERENT CHIS

! number of differen chi values
integer, parameter :: NCHI = 20

! chi setup
real*8 :: chi_l, chi_u, stemp, fb1_l, fb1_u, fb2_l, fb2_u

! female and male blockers
real*8 :: fblock_chi(0:NCHI), mblock_chi(0:NCHI)
real*8 :: fblock_chi1(0:NCHI), mblock_chi1(0:NCHI)
real*8 :: fblock_chi2(0:NCHI), mblock_chi2(0:NCHI)
real*8 :: fblock_data1(0:NCHI), mblock_data1(0:NCHI)
real*8 :: fblock_data2(0:NCHI), mblock_data2(0:NCHI)
integer :: ichi
    
end module
