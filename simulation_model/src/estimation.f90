!#####
! FOR ESTIMATING THE MODEL
!#####
module estimation
    
use globals
use model
use output
use simulated_annealing

implicit none   

integer :: iter
real*8 :: minimum(NEST), maximum(NEST)

contains


! FOR ESTIMATING MODEL PARATETERS
subroutine do_estimation()

    use omp_lib
 
    real*8 :: p(NEST), fret
    
    ! set up estimation
    call setup_estimation(p)
    
    ! use simulated annealing to estimate model
    call SA_minsearch(p, fret, minimum, maximum, estimation_eq, output=.true.)
    
    ! set number of threads back to their original value
    call omp_set_num_threads(1)

end subroutine


! THE FUNCTION USED FOR ESTIMATION
function estimation_eq(p) result(RSS)

    real*8, intent(in) :: p(:)
    real*8 :: RSS
    real*8 :: p_help(NEST), t_phi(f:m)
    integer :: iest, in, ic, ie
    
    
    ! RESTRICTION AND INITIALIZATION
    
    ! restrict parameters to the relevant space
    do iest = 1, NEST
        p_help(iest) = min(p(iest), maximum(iest))
        p_help(iest) = max(p_help(iest), minimum(iest))
    enddo    
    
    ! initialize return value
    RSS = 0d0
        
    ! COPYING VARIABLES
    
    ! mean of child preferences for women and men
    mu(0, f, hs) = p_help(1)
    mu(0, m, hs) = p_help(2)
    
    mu(1, f, hs) = p_help(3)
    mu(1, m, hs) = p_help(4)
    
    mu(2, f, hs) = p_help(5)
    mu(2, m, hs) = p_help(6)
    
    mu(0, f, co) = p_help(7)
    mu(0, m, co) = p_help(8)
    
    mu(1, f, co) = p_help(9)
    mu(1, m, co) = p_help(10)
    
    mu(2, f, co) = p_help(11)
    mu(2, m, co) = p_help(12)

    ! correlation of preferences between partners
    rho      = p_help(13)    
    
    ! persistence of child preferences
    pi       = p_help(14)
    
    ! utility cost of children
    phi_u    = p_help(15)
    
    ! ficticious child care wage
    w_y_base = p_help(16)
    
    ! child care wages in cross country analysis
    w_y_cc(1)= p_help(17)
    w_y_cc(2)= p_help(18)
    
    ! participation cost
    p_cost   = p_help(19)
        
    ! variance of preferences
    sig(f) = (p_help(20))**2
    sig(m) = (p_help(21))**2  
    
    ! define chi again (just to be sure in parallel mode)
    chi(m) = chi_base
    chi(f) = 1d0- chi(m)
    
    ! define w_y (just to be sure)
    w_y = w_y_base
    
    
    ! SOLVING MODEL
    
    ! solve the model    
    call solve_model        
    
    ! GET FIRST SET OF RESIDUALS
    
    ! fertility intentions
    do ie = hs, co
        RSS = RSS + sum((fert_int(:, :, 0:NN-1, ie) - fert_int_targ(:, :, 0:NN-1, ie))**2)  
    enddo
    
    ! conditional fertility in the next period
    RSS = RSS + (fert_int_cond(0, 0, 0, 0, al) - persist_targ(0, 0))**2
    RSS = RSS + (fert_int_cond(1, 0, 1, 0, al) - persist_targ(1, 0))**2
    RSS = RSS + (fert_int_cond(0, 1, 0, 1, al) - persist_targ(0, 1))**2
    RSS = RSS + (fert_int_cond(1, 1, 1, 1, al) - persist_targ(1, 1))**2
    
    ! labor force participation
    RSS = RSS + sum((LFP_with(-1, hs:co) - LFP_with_targ)**2)
    
    
    ! CROSS COUNTRY SIMULATIONS
    do ic = 1, 2
        
        chi(m) = chi_cc(ic)
        chi(f) = 1d0- chi(m)
        w_y    = w_y_cc(ic)
            
        call solve_model
    
        fblock(:, ic) = fert_int(0, 1, 0:NN-1, al)/(100d0 - fert_int(0, 0, 0:NN-1, al))*100d0
        mblock(:, ic) = fert_int(1, 0, 0:NN-1, al)/(100d0 - fert_int(0, 0, 0:NN-1, al))*100d0
        
        LFP_with_cc(ic) = LFP_with(-1, al)
    
        RSS = RSS + (fblock(1, ic) - fbshare_cc(1, ic))**2
        RSS = RSS + (mblock(1, ic) - mbshare_cc(1, ic))**2
    
        RSS = RSS + (fblock(2, ic) - fbshare_cc(2, ic))**2
        RSS = RSS + (mblock(2, ic) - mbshare_cc(2, ic))**2
        
        RSS = RSS + (LFP_with(-1, al) - LFP_cc(ic))**2
            
    enddo 
    
    ! end of story
    chi(m) = chi_base
    chi(f) = 1d0-chi(m)
    w_y    = w_y_base
    RSS_est = RSS
    iter = iter + 1
    
end function


!####
! SETS UP MINIMUM AND MAXIMUM FOR ESTIMATION
!#####
subroutine setup_estimation(p)

    real*8 :: p(NEST)

    ! set up minimum and maximum for estimation minimizer
    p( 1) = mu(0, f, hs)
    p( 2) = mu(0, m, hs)
    p( 3) = mu(1, f, hs)
    p( 4) = mu(1, m, hs)
    p( 5) = mu(2, f, hs)
    p( 6) = mu(2, m, hs)
    p( 7) = mu(0, f, co)
    p( 8) = mu(0, m, co)
    p( 9) = mu(1, f, co)
    p(10) = mu(1, m, co)
    p(11) = mu(2, f, co)
    p(12) = mu(2, m, co)
    p(13) = rho         
    p(14) = pi          
    p(15) = phi_u       
    p(16) = w_y_base    
    p(17) = w_y_cc(1)   
    p(18) = w_y_cc(2)   
    p(19) = p_cost      
    p(20) = sqrt(sig(f))
    p(21) = sqrt(sig(m))
    
    ! set minimum and maximum around point estimates
    minimum = p - 0.5d0*abs(p)
    maximum = p + 0.5d0*abs(p)
    
    ! special case for labor related parameters
    minimum(15:19) = p(15:19) - 0d0*abs(p(15:19))
    maximum(15:19) = p(15:19) + 0d0*abs(p(15:19))
    
    ! iteration count
    iter = 1
        
    ! calculate cost structure
    call setup(.true.)
    
    ! values to store
    mu_base = mu
    sig_base = sig
    t_phi_u_base = t_phi_u
    
    ! stop output
    out = .false.

end subroutine


!#####
! SETS UP TARGETS FOR ESTIMATION
!#####
subroutine setup_targets()

    integer :: ie, in
          
    ! fertility intentions by education
    fert_int_targ(0, :, 0, hs)  =  (/ 56.36098480224609d0, 6.92296028137207d0/)
    fert_int_targ(1, :, 0, hs)  =  (/ 5.554523468017578d0, 31.16153144836426d0/)

    fert_int_targ(0, :, 1, hs)  =  (/ 66.05327606201172d0, 7.548864364624023d0/)
    fert_int_targ(1, :, 1, hs)  =  (/ 4.294620037078857d0, 22.10323524475098d0/)

    fert_int_targ(0, :, 2, hs)  =  (/ 90.25062561035156d0, 4.392962455749512d0/)
    fert_int_targ(1, :, 2, hs)  =  (/ 2.309686422348022d0, 3.046728610992432d0/)

    fert_int_targ(0, :, 0, co)  =  (/ 49.09407043457031d0, 7.038731575012207d0/)
    fert_int_targ(1, :, 0, co)  =  (/ 6.366603851318359d0, 37.50059509277344d0/)

    fert_int_targ(0, :, 1, co)  =  (/ 56.56251525878906d0, 9.915250778198242d0/)
    fert_int_targ(1, :, 1, co)  =  (/ 5.075285911560059d0, 28.44694709777832d0/)

    fert_int_targ(0, :, 2, co)  =  (/ 86.34160614013672d0, 5.779096126556396d0/)
    fert_int_targ(1, :, 2, co)  =  (/ 3.294629812240601d0, 4.584670066833496d0/)

    
    ! persistence
    persist_targ(0, :) = (/ 79.89d0, 25.42d0/)
    persist_targ(1, :) = (/ 22.63d0, 65.24d0/)
    
    ! labor force participation
    LFP_with_targ = (/ 22.14405d0, 43.16666d0/)
    LFP_without_targ = (/62.59615d0, 80.49617d0/)
    
    ! cross country data
    chi_cc     = (/ 0.3420135676860809d0, .2798670828342438d0 /) 
    
    LFP_cc     = (/66.97839d0-32.28906788349826, 66.97839d0-45.45092354602485d0/)
    
    fbshare_cc(1, :) = (/ 17.20000058412552d0, 30.95768392086029d0/)
    fbshare_cc(2, :) = (/ 36.26877069473267d0, 51.62337422370911d0/)
    
    mbshare_cc(1, :) = (/ 13.52643262157345d0, 10.29180800679795d0/)
    mbshare_cc(2, :) = (/ 25.71065045978197d0, 20.81469241623341d0/)
    
    cons_cc = (/0.1757039d0,  0.3727529d0/)
    beta_cc = (/-0.235114d0, -0.3188593d0/)    
    
    ! create target output
    fert_int(:, :, 0:NN-1, hs:co) = fert_int_targ(:, :, 0:NN-1, hs:co)
    fert_int(:, :, :, al) = dist_hc(hs)*fert_int(:, :, :, hs) + dist_hc(co)*fert_int(:, :, :, co)
    
    fert_out = 0d0
    do in = 0, 2
        do ie = hs, co        
            fert_out(:, :, in, ie) = nu(in, :, :, ie)*100d0
            fert_out(:, :, in, al) = fert_out(:, :, in, al) + dist_hc(ie)*nu(in, :, :, ie)*100d0
        enddo
    enddo
    
    LFP_with = 0d0
    LFP_with(-1, hs:co) = LFP_with_targ
    LFP_with(-1, al) = dist_hc(hs)*LFP_with(-1, hs) + dist_hc(co)*LFP_with(-1, co)
    
    LFP_without = 0d0
    LFP_without(-1, hs:co) = LFP_without_targ
    LFP_without(-1, al) = dist_hc(hs)*LFP_without(-1, hs) + dist_hc(co)*LFP_without(-1, co)
        
    fert_int_cond = 0d0
    fert_int_cond(0, 0, 0, 0, :) = persist_targ(0, 0)
    fert_int_cond(0, 1, 0, 1, :) = persist_targ(0, 1)
    fert_int_cond(1, 0, 1, 0, :) = persist_targ(1, 0)
    fert_int_cond(1, 1, 1, 1, :) = persist_targ(1, 1)
    
    
    call write_output('output/estim_targets_all.out', al)
    call write_output('output/estim_targets_hs.out', hs)
    call write_output('output/estim_targets_col.out', co)
    
    iter = 1

end subroutine
    
end module
