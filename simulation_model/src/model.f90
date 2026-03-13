!#####
! FOR CALCULATING THE RESPECTIVE MODEL SOLUTION
!#####
module model
    
use globals
use toolbox
use wage_distribution

implicit none

contains

! SETS UP MODEL PARAMETERS AND CALCULATES MODEL SOLUTION
subroutine solve_model

    implicit none
    
    ! set up model parameters
    call setup(.false.)
    
    ! get the decision rules
    call get_decisions()
    
    ! solve for the distribution
    call get_distributions()
    
    ! get aggregates
    call get_aggregate()

end subroutine


! SETS UP MODEL PARAMETERS
subroutine setup(short)

    logical, intent(in) :: short
    integer :: ig, in, is, ie, iw, ih
    real*8 :: pv_factor, w_yn, w_fn
    
    ! get the discretized wage distribution
    if(do_wages)then
        do ie = hs, co
            call discretize_wages(w_f(:, ie), dist_w(:, ie), p_cost, w_y, w_f_bar(ie), LFP_without_targ(ie)/100d0)
        enddo
    endif

    ! cost share of the female partner
    chi(f) = 1d0 - chi(m)
    
    ! calculate present value factor
    pv_factor = 1d0 + beta + beta**2 + beta**3 + beta**4 + beta**5
    
    ! per period cost of a child
    do ig = f, m
        do in = 0, NN-1
            t_phi_u(ig, in) = chi(ig)*(phi_u*pv_factor)
            t_phi_c(ig, in) = 0.5d0*(1d0+alpha)*(phi_c*pv_factor)
        enddo
    enddo
    
    ! subsidies to utility costs
    do in = 0, NN-1
        t_phi_u(f, in) = t_phi_u(f, in) - (1d0 + 0.5d0*alpha)*sub(4, in) - 0.5*alpha*sub(5, in)
        t_phi_u(m, in) = t_phi_u(m, in) - (1d0 + 0.5d0*alpha)*sub(5, in) - 0.5*alpha*sub(4, in)
    enddo
    
    ! total cost depending on h
    do ie = hs, co
        do iw = 1, WW
            do in = 0, NN-1
                
                ! calculate child care cost
                w_yn = w_y*(1d0 - sub(1, in))
                
                ! calculate net female wage
                w_fn = w_f(iw, ie)*(1d0 - sub(2, in) + sub(0, in))
                
                do ih = 0, 1
                    t_phi(ih, f, in, iw, ie) = t_phi_u(f, in) + t_phi_c(f, in) &
                                    - sub(0, in)*(w_f(iw, ie) + 0.5d0*alpha*(w_f(iw,ie) + 1.25d0*w_f_bar(ie))) &
                                    + 0.5d0*(dble(ih)*(2d0+alpha)*w_fn + (1d0-dble(ih))*(1d0+alpha)*w_yn)
                    t_phi(ih, m, in, iw, ie) = t_phi_u(m, in) + t_phi_c(m, in) &
                                    - sub(0, in)*(1.25d0*w_f_bar(ie) + 0.5d0*alpha*(w_f(iw,ie) + 1.25d0*w_f_bar(ie))) &
                                    + 0.5d0*(dble(ih)*alpha*w_fn + (1d0-dble(ih))*(1d0+alpha)*w_yn)
                enddo
            enddo
        enddo
    enddo
    
    ! limited commitment thresholds
    do ie = hs, co
        do iw = 1, WW            
            do in = 0, NN-1
                
                ! calculate child care cost
                w_yn = w_y*(1d0 - sub(1, in))
                
                ! calculate net female wage
                w_fn = w_f(iw, ie)*(1d0 - sub(2, in) + sub(0, in))
                
                limited(f, in, iw, ie) = t_phi_u(f, in) + t_phi_c(f, in) + 0.5d0*(nu(in, 1, 1, ie)*(2d0+alpha)*w_fn &
                                            - nu(in, 1, 0, ie)*(1d0+alpha)*w_yn)/(nu(in, 1, 1, ie) - nu(in, 1, 0, ie))
                limited(m, in, iw, ie) = t_phi_u(m, in) + t_phi_c(m, in) + 0.5d0*(nu(in, 1, 1, ie)*(1d0+alpha)*w_yn &
                                            - nu(in, 0, 1, ie)*alpha*w_fn)/(nu(in, 1, 1, ie) - nu(in, 0, 1, ie))
            enddo
        enddo
    enddo
    
    if(short)return    
    
    call get_preferences()

end subroutine


! CALCULATE THE PREFERENCE DISTRIBUTION
subroutine get_preferences()
    
    integer :: is, ie
    real*8 :: u_u, u_v, u_x(4), u_XX(2)
    real*8 :: l_unif, r_unif
    
    ! limits for the uniform distribution
    l_unif = max(0d0,    -rho)
    r_unif = min(1d0, 1d0-rho)
    
    ! generate simulated preferences
    v_g = 0d0
    do ie = hs, co
        do is = 1, SS
        
            ! get the draws of the standard uniform distribution with correlation rho
            u_u =  l_unif + realizations(is, 1)*(r_unif-l_unif)
            u_v = 2d0*realizations(is, 2)
        
            u_x(1) = min(u_v, u_u)
            u_x(2) = min(u_v-u_x(1), 1d0-u_u)
            u_x(3) = min(u_v-u_x(1)-u_x(2), u_u)
            u_x(4) = min(u_v-u_x(1)-u_x(2)-u_x(3), 1d0-u_u)
        
            u_XX(1) = (u_x(1) - u_x(3)) + (u_x(2) - u_x(4))
            u_XX(2) = u_u - (u_x(1) - u_x(3)) + (u_x(2) - u_x(4))
        
            ! calculate the actual preference realization
            v_g(f, is, :, ie) = mu(:, f, ie) + (u_XX(1)-0.5d0)*(sqrt(12d0*sig(f)))
            v_g(m, is, :, ie) = mu(:, m, ie) + (u_XX(2)-0.5d0)*(sqrt(12d0*sig(m)))
        enddo
    enddo

end subroutine


! CALCULATES THE DECISION RULES
subroutine get_decisions()

    implicit none
    integer :: is, in, ig, it, ie, iw, i_t(f:m), h_t
    real*8 :: t_phi_y(f:m), prob, E_eta(f:m, 0:NN), E_X(f:m, 0:NN), w_yn, w_fn
    
    i_g = 0
    X_t = 0d0
    
    ! iterate over education levels
    do ie = hs, co
        
        ! iterate over the different wage levels
        do iw = 1, WW
    
            ! iterate backward over time to get decisions
            do it = TT, 1, -1
                
                ! set continutation utility components to zero
                E_eta = 0d0
                E_X   = 0d0
                
                do in = 0, min(NN-1, it-1)
                    
                    ! calculate child care cost
                    w_yn = w_y*(1d0 - sub(1, in))
                    
                    ! calculate net female wage
                    w_fn = w_f(iw, ie)*(1d0 - sub(2, in) + sub(0, in))
                    
                    do is = 1, SS
                        
                        ! determine individual preferences
                        do ig = f, m
                            eta_g(ig, is, in) = v_g(ig, is, in, ie) + beta*X_t(ig, is, in, it, iw, ie)
                        enddo
            
                        ! wage of the woman too low relative to child care costs
                        if(w_fn <= (1d0+alpha)/(2d0+alpha)*w_yn)then
                            h_t = 1
                            
                        elseif(w_fn > (1d0+alpha)/alpha*w_yn)then
                            h_t = 0
                            
                        ! wage of woman lower than child care cost
                        elseif(w_fn <= w_yn)then
                            
                            ! limited commitment case                                                        
                            if(eta_g(f, is, in) > t_phi(0, f, in, iw, ie)  .and. &
                               eta_g(f, is, in) <= t_phi(1, f, in, iw, ie) .and. &
                               eta_g(m, is, in) > limited(m, in, iw, ie)   .and. limited_commitment)then
                                h_t = 0
                            else
                                h_t = 1
                            endif
                            
                        ! wage of woman lower greater child care cost
                        else
                            
                            ! limited commitment case                                                        
                            if(eta_g(m, is, in) > t_phi(1, m, in, iw, ie)  .and. &
                               eta_g(m, is, in) <= t_phi(0, m, in, iw, ie) .and. &
                               eta_g(f, is, in) > limited(f, in, iw, ie)   .and. limited_commitment)then
                                h_t = 1
                            else
                                h_t = 0
                            endif
                        endif
                        
                        ! calculate decision for each partner
                        do ig = f, m
                            if(eta_g(ig, is, in) > t_phi(h_t, ig, in, iw, ie))then
                                i_t(ig) = 1
                            else
                                i_t(ig) = 0
                            endif
                        enddo
                        
                        ! copy decisions
                        i_g(:, is, in, it, iw, ie) = i_t
                        h(is, in, it, iw, ie)      = h_t
            
                        ! get child bearing probability and help variables for continuation value
                        prob = nu(in, i_t(f), i_t(m), ie)
                        do ig = f, m
                            E_eta(ig, in) = E_eta(ig, in) + prob*(eta_g(ig, is, in) - t_phi(h_t, ig, in, iw, ie))/dble(SS)
                            E_X(ig, in)   = E_X(ig, in)   + X_t(ig, is, in, it, iw, ie)/dble(SS)
                        enddo
                    enddo
                enddo
                
                ! calculate previous period X_t values
                if(it > 1)then
                    do ig = f, m
                        do in = 0, min(NN-1, it-1)
                            do is = 1, SS
                                prob = nu(in, i_g(f, is, in, it, iw, ie), i_g(m, is, in, it, iw, ie), ie)
                                X_t(ig, is, in, it-1, iw, ie) = &
                                      E_eta(ig, in+1) - pi*prob*(eta_g(ig, is, in) - t_phi(h_t, ig, in, iw, ie)) &
                                    - (1d0-pi)*E_eta(ig, in) + beta*(pi*X_t(ig, is, in, it, iw, ie) + (1d0-pi)*E_X(ig, in))
                            enddo
                        enddo
                    enddo
                endif
            enddo
        enddo  
    enddo

end subroutine


! CALCULATES THE DISTRIBUTIONS
subroutine get_distributions()

    implicit none
    integer :: is, in, it, iw, ie
    real*8 :: dist, prob, edist(0:1)
    
    ! initialize distribution
    distrib = 0d0        
    
    ! iterate over education and wage levels
    do ie = hs, co
        do iw = 1, WW
            
            ! get initial distribution    
            distrib(:, 0, 1, iw, ie) = dist_w(iw, ie)/dble(SS)
    
            ! calculate forward distribution
            do it = 1, TT-1
                do in = 0, min(NN-1, it-1)
            
                    edist = 0d0
            
                    do is = 1, SS
            
                        ! copy stuff
                        dist = distrib(is, in, it, iw, ie)
                        prob = nu(in, i_g(f, is, in, it, iw, ie), i_g(m, is, in, it, iw, ie), ie)
            
                        ! case of having a child
                        edist(1) = edist(1) + prob*dist/dble(SS)
                
                        ! case of not having a child
                        distrib(is, in, it+1, iw, ie) = distrib(is, in, it+1, iw, ie) + (1d0-prob)*dist*pi
                        edist(0) = edist(0) + (1d0-prob)*dist/dble(SS)*(1d0-pi)
                    enddo
            
                    distrib(:, in, it+1, iw, ie)   = distrib(:, in, it+1, iw, ie)   + edist(0)
                    distrib(:, in+1, it+1, iw, ie) = distrib(:, in+1, it+1, iw, ie) + edist(1)
                enddo
        
                ! those who already have three children just move forward
                distrib(:, 3, it+1, iw, ie) = distrib(:, 3, it+1, iw, ie) + distrib(:, 3, it, iw, ie)
            enddo
        enddo
    
        ! calculate completed fertility distribution
        do in = 0, NN
            tot_dist(in, ie) = sum(distrib(:, in, TT, :, ie))
        enddo
    enddo    
    
    ! get aggregate distribution
    distrib(:, :, :, :, al) = dist_hc(hs)*distrib(:, :, :, :, hs) + dist_hc(co)*distrib(:, :, :, :, co)
    tot_dist(:, al)         = dist_hc(hs)*tot_dist(:, hs)         + dist_hc(co)*tot_dist(:, co)
    
end subroutine


! AGGREGATE INDIVIDUAL DECISIONS
subroutine get_aggregate()

    integer :: is, in, it, iw, ie, i_t(f:m), i_t2(f:m), iif, iim
    real*8 :: dist, fert_int_it(0:1, 0:1, 2:6), s_dist(-1:NN-1)
    real*8 :: dist_age_first(hs:co), dist_age_last(hs:co)
    
    ! calculate fertility intentions and fertility rate
    fert_int      = 0d0
    fert_rat      = 0d0
    fert_int_cond = 0d0
    fert_age_first= 0d0
    fert_age_last = 0d0
    dist_age_first= 0d0
    dist_age_last = 0d0
    
    ! iterate over education levels
    do ie = hs, co 
        
        fert_int_it = 0d0
        
        do iw = 1, WW
            do it = 1, TT
                do in = 0, min(NN-1, it-1)
                    do is = 1, SS
                
                        dist = distrib(is, in, it, iw, ie)
                        i_t(:) = i_g(:, is, in, it, iw, ie)
        
                        fert_int(i_t(f), i_t(m), -1, ie) = fert_int(i_t(f), i_t(m), -1, ie) + dist
                        fert_int(i_t(f), i_t(m), in, ie) = fert_int(i_t(f), i_t(m), in, ie) + dist
                
                        if(in == 0 .and. it >= 2 .and. it <= 6)then
                            fert_int_it(i_t(f), i_t(m), it) = fert_int_it(i_t(f), i_t(m), it) + dist
                        endif                    
                
                        fert_rat(ie) = fert_rat(ie) + nu(in, i_t(f), i_t(m), ie)*dist
                        
                        if(in == 0)then
                            fert_age_first(ie) = fert_age_first(ie) + nu(in, i_t(f), i_t(m), ie)*dist*dble(it)
                            dist_age_first(ie) = dist_age_first(ie) + nu(in, i_t(f), i_t(m), ie)*dist
                        elseif(in == NN-1)then
                            fert_age_last(ie)  = fert_age_last(ie)  + nu(in, i_t(f), i_t(m), ie)*dist*dble(it)
                            dist_age_last(ie)  = dist_age_last(ie)  + nu(in, i_t(f), i_t(m), ie)*dist
                        endif                        
                    enddo
                enddo
            enddo
        enddo
        
        fert_age_first(ie) = 19.5d0 + (fert_age_first(ie)/dist_age_first(ie)-1d0)*3d0
        fert_age_last(ie)  = 19.5d0 + (fert_age_last(ie) /dist_age_last(ie) -1d0)*3d0
        
        ! get the real averages
        do in = -1, NN
            fert_int(:, :, in, ie) = fert_int(:, :, in, ie)/max(sum(fert_int(:, :, in, ie)), 1d-10)*100d0
        enddo
    
        do it = 2, 6
            fert_int_it(:, :, it) = fert_int_it(:, :, it)/max(sum(fert_int_it(:, :, in)), 1d-10)
        enddo
    
        ! now calculate the conditional distributions
        i_g(:, :, NN, :, :, ie) = 0
        do iw = 1, WW
            do in = 0, NN
                do it = 1, 5
                    do is = 1, SS
                        dist = distrib(is, in, it, iw, ie)
                        i_t(:) = i_g(:, is, in, it, iw, ie)
                        i_t2(:) = i_g(:, is, in, it+1, iw, ie)
                
                        if(in < NN)then
                    
                            ! those who stay
                            fert_int_cond(i_t(f), i_t(m), i_t2(f), i_t2(m), ie) = &
                                fert_int_cond(i_t(f), i_t(m), i_t2(f), i_t2(m), ie) + &
                                (1d0-nu(0, i_t(f), i_t(m), ie))*dist*pi
            
                            ! those who get a new draw
                            fert_int_cond(i_t(f), i_t(m), :, :, ie) = &
                                fert_int_cond(i_t(f), i_t(m), :, :, ie) + &
                                (1d0-nu(0, i_t(f), i_t(m), ie))*dist*(1d0-pi)*fert_int_it(:, :, it+1)
                        else
                            ! those who stay
                            fert_int_cond(i_t(f), i_t(m), i_t2(f), i_t2(m), ie) = &
                                fert_int_cond(i_t(f), i_t(m), i_t2(f), i_t2(m), ie) + dist
                        endif
                    
                    enddo
                enddo
            enddo
        enddo
    
        do iif = 0, 1
            do iim = 0, 1
                fert_int_cond(iif, iim, :, :, ie) = fert_int_cond(iif, iim, :, :, ie)/max(sum(fert_int_cond(iif, iim, :, :, ie)), 1d-10)*100d0
            enddo
        enddo
    
        ! female labor force participation
        LFP_with(:, ie) = 0d0
        s_dist   = 0d0
        do iw = 1, WW
            do it = 1, TT
                do in = 0, min(NN-1, it-1)
                    do is = 1, SS
                    
                        i_t(:) = i_g(:, is, in, it, iw, ie)
                        dist = distrib(is, in, it, iw, ie) 
                    
                        LFP_with(-1, ie) = LFP_with(-1, ie) + nu(in, i_t(f), i_t(m), ie)*dist*(1d0-dble(h(is, in, it, iw, ie)))
                        s_dist(-1)   = s_dist(-1)   + nu(in, i_t(f), i_t(m), ie)*dist
                
                        LFP_with(in, ie) = LFP_with(in, ie) + nu(in, i_t(f), i_t(m), ie)*dist*(1d0-dble(h(is, in, it, iw, ie)))
                        s_dist(in)   = s_dist(in)   + nu(in, i_t(f), i_t(m), ie)*dist
                    enddo
                enddo
            enddo
        enddo
    
        LFP_with(:, ie) = LFP_with(:, ie)/s_dist*100d0
        
        LFP_without(:, ie) = (1d0 - dist_w(1, ie))*100d0
    
    enddo
    
    ! get overall aggregates
    fert_int(:, :, :, al)         = dist_hc(hs)*fert_int(:, :, :, hs)         + dist_hc(co)*fert_int(:, :, :, co)
    fert_int_cond(:, :, :, :, al) = dist_hc(hs)*fert_int_cond(:, :, :, :, hs) + dist_hc(co)*fert_int_cond(:, :, :, :, co)
    fert_rat(al)                  = dist_hc(hs)*fert_rat(hs)                  + dist_hc(co)*fert_rat(co)
    LFP_with(:, al)               = dist_hc(hs)*LFP_with(:, hs)               + dist_hc(co)*LFP_with(:, co)
    fert_age_first(al)            = dist_hc(hs)*fert_age_first(hs)            + dist_hc(co)*fert_age_first(co)
    fert_age_last(al)             = dist_hc(hs)*fert_age_last(hs)             + dist_hc(co)*fert_age_last(co)

end subroutine
    
end module
