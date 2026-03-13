!#####
! PROGRAM TO SOLVE AND ESTIMATE FERTILITY DECISIONS
!#####
program FertDec

use globals
use toolbox
use model
use output
use estimation
use policy_analysis

implicit none
integer, parameter :: nsub = 30
integer :: iss
real*8 :: maxsub = 3d0
real*8 :: x_sub(0:nsub), y_sub(0:nsub), z_sub(0:nsub)

! INITIALIZE VARIABLES

! estimate or simulate
policy = .true.
estimate = .false.
simulate_new_shocks = .false.


! SIMULATE A NEW SERIES OF SHOCKS (PROGRAM WILL STOP AFTERWARDS)
if(simulate_new_shocks)call simulate_shocks()


! EXOGENOUS

! economies of scale
alpha = 0.4d0

! time discount factor
beta = 0.95d0

! male participation of child care (baseline)
chi_base = 0.30673904d0

! monetary cost of children  (5000 see paper, 30000 = wage of hs woman, Germany)
phi_c = 5000d0 / 30000d0

! wage of the female spouse (mean)
w_f_bar = (/1.0d0, 1.5d0/)

! how many college type women
dist_hc(co) = 0.2534d0

! labor force participation without children
LFP_without(-1, hs:co) = (/62.59615d0, 80.49617d0/)

! conception probabilities (high school)
nu(0, 0, :, hs) = (/ .1789176717197569, .1789176717197569/)
nu(0, 1, :, hs) = (/ .1789176717197569, .4021394778241374/)

nu(1, 0, :, hs) = (/ .1305865488254912, .1305865488254912/)
nu(1, 1, :, hs) = (/ .2360278539287936, .3983988978195017/)

nu(2, 0, :, hs) = (/ .0428344406053852, .0428344406053852/)
nu(2, 1, :, hs) = (/ .1220761109723249, .3615216294526126/)

! conception probabilities (college)
nu(0, 0, :, co) = (/ .1702864617346444, .1702864617346444/)
nu(0, 1, :, co) = (/ .1702864617346444, .4377648898880003/)

nu(1, 0, :, co) = (/ .1142364241909013, .1142364241909013/)
nu(1, 1, :, co) = (/ .2666842193124367, .4247963593590841/)

nu(2, 0, :, co) = (/ .024787697107474, .024787697107474/)
nu(2, 1, :, co) = (/ .024787697107474, .3090825559183003/)


! ENDOGENOUS

! mean of child preferences
mu(0, f, hs) =                5.06895443d0 ; mu(0, m, hs) =                3.63503709d0
mu(1, f, hs) =                1.78695119d0 ; mu(1, m, hs) =               -6.44256185d0
mu(2, f, hs) =               -0.15113675d0 ; mu(2, m, hs) =              -15.53940557d0
 
mu(0, f, co) =                5.77527295d0 ; mu(0, m, co) =                4.84667182d0
mu(1, f, co) =                3.05908870d0 ; mu(1, m, co) =               -0.00001244d0
mu(2, f, co) =                0.05275858d0 ; mu(2, m, co) =              -14.63174255d0
 
! variance of fertility preferences
sig(f)   = (               3.07058582d0)**2
sig(m)   = (              12.72206793d0)**2
 
! correlation
rho      =                0.80214353d0
 
! persistence of child preferences
pi       =                0.28971537d0
 
! utility cost of children
phi_u    =                1.00000000d0
 
! ficticious child care wage
w_y_base =                0.58407903d0
 
! child care wage in cross country analysis
w_y_cc(1)=                0.41961090d0
w_y_cc(2)=                0.76702966d0
 
! participation cost
p_cost   =                0.35808630d0


! SOME BASELINE SETUP

! calculate wage distribution
do_wages = .true.

! set the baseline chi_m
chi(m) = chi_base
    
! distribution of high school individuals
dist_hc(hs) = 1d0 - dist_hc(co)


! ESTIMATION

! set up target moments
call setup_targets()

! call basic model setup
call basic_setup()

! set up estimation procedure
call setup_estimation(p)

!iter = 0
RSS_est = estimation_eq(p)
call solve_model

call write_params('output/estim_parameters.out')
call write_estimation_results('output/estim_values.out')

if(estimate)then
    call do_estimation()
endif


! SIMULATION

! solve model
out = .true.
call tic
call solve_model
call toc

! write output
call write_output('output/res_output_all.out', al)
call write_output('output/res_output_hs.out', hs)
call write_output('output/res_output_col.out', co)
call tex_output('output/res_output.tex')

if(estimate)stop


! stop recalculation of wages
do_wages = .false.


! POLICY
if(policy)call do_policy


contains


! SETS UP BASIC AND PARAMETER INDEPENDENT MODEL FEATURES
subroutine basic_setup()    
        
    implicit none
    integer :: is
    
    ! load random variable data
    open(621,file='input/random.inp')
    do is = 1, SS
        read(621,'(2f25.15)')realizations(is, :)
    enddo    
    close(621)
    
    ! set the baseline chi_m
    chi(m) = chi_base
    
    ! distribution of high school individuals
    dist_hc(hs) = 1d0 - dist_hc(co)
    
    ! set the baseline w_y
    w_y = w_y_base
    
    ! set up all model parameters
    call setup(.false.)
    
end subroutine


! a function for the standard deviation
function std(x)

    real*8, intent(in) :: x(:)
    integer :: T
    real*8 :: mean, std
    
    T = size(x, 1)    
    mean = sum(x)/dble(T)
    std = sum((x-mean)**2)/dble(T-1)
    std = sqrt(std)

end function


! SIMULATE INDEPENDENT UNIFORM SHOCKS FOR PREFERENCES AND WAGES
subroutine simulate_shocks()

    integer :: ic, ic1, ic2, ii
    real*8, allocatable :: series(:, :)

    if(allocated(series))deallocate(series)
    allocate(series(SS, 2))

    ! simulate the basis for preferences and wage distribution
    do ii = 1, 2
        ic = 1
        do
            ! stop after 100000 shocks
            if(ic > SS/2)exit
        
            ! get shock values to simulate
            ic1 = 1 + 2*(ic-1)
            ic2 = ic1 + 1
        
            ! simulate shock
            call simulate_uniform(series(ic1, ii))
    
            ! use symmetric pairs for mean stability
            series(ic2, ii) = 1d0 - series(ic1, ii)
            ic = ic + 1
        enddo
        
        write(*,'(a)')'--------------------------------------------------------------'
        write(*,'(a)')'| SIMULATION STATISTICS                                      |'
        write(*,'(a,f10.6,a)')'| MEAN =        ', sum(series(:, ii))/dble(SS),'                                   |'
        write(*,'(a,f10.6,a)')'| VARIANCE =    ', sum(series(:, ii)**2)/dble(SS) - (sum(series(:, ii))/dble(SS))**2,'                                   |'
        write(*,'(a,f10.6,a)')'| MINIMUM =     ', minval(series(:, ii)),'                                   |'
        write(*,'(a,f10.6,a)')'| MAXIMUM =     ', maxval(series(:, ii)),'                                   |'
    enddo
    
    write(*,'(a)')'--------------------------------------------------------------'

    open(621,file='input/random.inp')
    do ic = 1, SS
        write(621,'(100f25.15)')series(ic, :)
    enddo    
    close(621)
    stop

end subroutine 

end program
