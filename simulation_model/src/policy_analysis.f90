!#####
! MODULE FOR POLICY ANALYSIS
!#####
module policy_analysis
    
use globals
use toolbox
use model

implicit none

! 0 = money
! 1 = child care subsidy
! 2 = parental leave money
! 3 = parental leave + child care
! 4 = transfer to woman
! 5 = transfer to man
integer, parameter :: n_pol = 5

real*8, parameter :: tfr_increase = 0.10d0

real*8 :: subsi(0:n_pol, 0:NN-1), cost(0:n_pol, 0:NN-1), labor(0:n_pol, 0:NN-1), tfr(0:n_pol, 0:NN-1), tfr_base
real*8 :: age_first(0:n_pol, 0:NN-1), age_last(0:n_pol, 0:NN-1), favincome(0:n_pol, 0:NN-1)
character(LEN=6) :: name(0:n_pol)

real*8 :: guess_com
integer :: to_n_com

contains

! executes policy analysis
subroutine do_policy()

    integer :: to_n, ip, first
    real*8 :: p(2), fret
    
    ! save total fertility rate
    tfr_base = fert_rat(al)    
    
    ! set names
    name(0) = 'MONEY '
    name(1) = 'CC    '
    name(2) = 'PL    '
    name(3) = 'CC+PL '
    name(4) = 'FEMALE'
    name(5) = 'MALE  '

    ! standard policy
    call tic
    write(*,'(a)')'RUNNING POLICY ...'
    
    first = 1
    
    ! search for the policy that increases tfr by tfr_increase
    do to_n = 0, NN-1
        do ip = 0, n_pol
            guess_com = 0.7d0
            
            call search_policy(guess_com, to_n, ip)
            write(*,*)to_n, ip, 'DONE'
        enddo        
    enddo
    
    call write_policy_output()
    
    write(*,'(a/)')'DONE ...'
    call toc     
    
    stop
    
    ! now search for optimal policy mix between money, parental leave benefits and child care subsidy
    call tic
    write(*,'(a)')'RUNNING OPTIMAL POLICY ...'
    
    open(76152, file='output/res_policy_optimal.out')
    
    write(76152, '(a)')'to_n          tfr       money          cc      pleave        cost        flfp'
    
    do to_n = 0, NN-1
        
        ! communication variables
        to_n_com = to_n
        
        if(to_n == 0)then
            guess_com = 0.70d0
        elseif(to_n == 1)then
            guess_com = 0.80d0
        else
            guess_com = 3d0
        endif
        
        p(1) = 0.5d0
        p(2) = 1d0
        
        call settol_min(1d-5)
        
        !call fminsearch(p, fret, (/0d0, 0d0/), (/1d0, 1d0/), optimal_policy_fcn)
        call fminsearch(p(1), fret, 0d0, 1d0, optimal_policy_fcn_1)
        
        write(*,*)
        write(*,*)to_n, p, fret, 'DONE'
        write(*,*)
        
        fret = optimal_policy_fcn(p)
        
        write(76152, '(i5,7f12.5)')to_n+1, fert_rat(al)-tfr_base, tfr_base, 1d0-p(2), p(2)*p(1), p(2)*(1d0-p(1)), fret, LFP_with(-1, al)
    enddo
    
    close(76152)
    
    write(*,'(a/)')'DONE ...'
    call toc 
    
end subroutine


! SUBROUTINE TO FIND THE SUBSIDY NEEDED TO GET POLICIES RIGHT
subroutine search_policy(guess, to_n, ip)

    implicit none    
    real*8, intent(in) :: guess
    integer :: to_n, ip, it, in, is, iw, ie, i_t(f:m)
    real*8 :: dist, sumdist
    real*8 :: left, right, middle, w_yn, w_fn
    real*8 :: fleft, fright, fmiddle
    real*8 :: addterm = 0.25d0
    
    ! set policy to zero
    sub = 0d0
    
    ! get left interval value
    right = guess
    call solve_model
    fright = fert_rat(al)-tfr_base
    
    ! check whether we should walk left or right
    if(fright > tfr_increase)then
        addterm = -addterm
    endif
    
    ! determine left and right interval point
    do                        
        
        ! set right to left interval point
        left = right
        fleft = fright
        
        ! add to original left point
        middle = left + addterm
        
        ! get new right point
        right = middle
        if(ip /= 3)then
            sub(ip, to_n:NN-1) = right
        else
            sub(1:2, to_n:NN-1) = right
        endif
        
        call solve_model
        fright = fert_rat(al)-tfr_base

        write(*,*)right, fright
                
        if(addterm > 0 .and. fright >= tfr_increase)exit
        if(addterm < 0 .and. fright <= tfr_increase)then
            
            ! change left and right and exit
            middle = left
            fmiddle = fleft
            
            left = right
            fleft = fright
            
            right = middle
            fright = fmiddle
            exit
        endif
    enddo
    
    ! do bracketing for closer approximation
    do
        
        ! get new middle point
        middle = (left+right)/2d0
        if(ip /= 3)then
            sub(ip, to_n:NN-1) = middle
        else
            sub(1:2, to_n:NN-1) = middle
        endif
        call solve_model
        fmiddle = fert_rat(al)-tfr_base
        
        if(fmiddle <= tfr_increase .and. fright >= tfr_increase)then
            left = middle
            fleft = fmiddle
        else
            right = middle
            fright = fmiddle
        endif
        
        if(abs(right-left) < 1d-5)exit
            
    enddo  

    
    if(ip /= 3)then
        sub(ip, to_n:NN-1) = middle
    else
        sub(1:2, to_n:NN-1) = middle
    endif
    call solve_model
        
    tfr(ip, to_n) = tfr_base + fmiddle
    labor(ip, to_n) = LFP_with(-1, al)
    age_first(ip, to_n) = fert_age_first(al)
    age_last(ip, to_n) = fert_age_last(al)
    subsi(ip, to_n) = middle
    
    cost(ip, to_n) = 0d0
    do ie = hs, co
        do iw = 1, WW
            do it = 1, TT
                do in = to_n, min(NN-1, it-1)
                    
                    ! calculate child care cost
                    w_yn = w_y*(1d0 - sub(1, in))

                    
                    ! calculate net female wage
                    w_fn = w_f(iw, ie)*(1d0 - sub(2, in) + sub(0, in))
                    
                    do is = 1, SS
                
                        i_t(:) = i_g(:, is, in, it, iw, ie)                
                        dist = nu(in, i_t(f), i_t(m), ie)*distrib(is, in, it, iw, ie)*dist_hc(ie)            
                        cost(ip, to_n) = cost(ip, to_n) + dist*(sub(0, in)*(w_f(iw,ie) + 1.25d0*w_f_bar(ie)) + &
                                                                (w_y-w_yn)*(1d0-dble(h(is, in, it, iw, ie))) + &
                                                                (w_f(iw, ie)-w_fn)*dble(h(is, in, it, iw, ie)) + &
                                                                sub(4, in) + sub(5, in))
                    enddo
                enddo
            enddo
        enddo
    enddo
    
    favincome(ip, to_n) = 0d0
    do ie = hs, co
        do iw = 1, WW
            do it = 1, TT
                do in = 0, min(NN-1, it-1)
                    do is = 1, SS     
                        dist = distrib(is, in, it, iw, ie)*dist_hc(ie)
                        favincome(ip, to_n) = favincome(ip, to_n) + dist*(w_f(iw,ie)*(1d0-dble(h(is, in, it, iw, ie))) + 1.25d0*w_f_bar(ie))
                    enddo
                enddo
            enddo
        enddo
    enddo
    
end subroutine



! FUNCTION TO FIND THE SUBSIDY NEEDED TO GET POLICIES RIGHT (FOR OPTIMAL POLICY DESIGN)
function optimal_policy_fcn(p)

    implicit none    
    
    real*8, intent(in) :: p(:)
    real*8 :: optimal_policy_fcn
    
    integer :: to_n, ip, it, in, is, iw, ie, i_t(f:m)
    real*8 :: dist, sumdist
    real*8 :: left, right, middle
    real*8 :: fleft, fright, fmiddle
    real*8 :: addterm = 0.05d0
    
    ! copy to_n from communication variable
    to_n = to_n_com
    
    ! set policy to zero
    sub = 0d0
    
    ! get left interval value
    left = guess_com
    call solve_model
    fleft = fert_rat(al)-tfr_base
    
    ! check whether we should walk left or right
    if(fleft > tfr_increase)then
        addterm = -addterm
    endif
    
    ! determine left and right interval point
    do
        
        ! add to original left point
        middle = left + addterm
        
        ! set right to left interval point
        left = right
        fleft = fright
        
        ! get new right point
        right = middle
        
        sub(0, to_n:NN-1) = (1d0-p(2))     *right
        sub(1, to_n:NN-1) = p(2)*p(1)      *right
        sub(2, to_n:NN-1) = p(2)*(1d0-p(1))*right
        
        call solve_model
        fright = fert_rat(al)-tfr_base
        
        if(addterm > 0 .and. fright >= tfr_increase)exit
        if(addterm < 0 .and. fright <= tfr_increase)then
            
            ! change left and right and exit
            middle = left
            fmiddle = fleft
            
            left = right
            fleft = fright
            
            right = middle
            fright = fmiddle
            exit
        endif
    enddo
    
    ! do bracketing for closer approximation
    do
        
        ! get new middle point
        middle = (left+right)/2d0

        sub(0, to_n:NN-1) = (1d0-p(2))     *middle
        sub(1, to_n:NN-1) = p(2)*p(1)      *middle
        sub(2, to_n:NN-1) = p(2)*(1d0-p(1))*middle      
        
        call solve_model
        fmiddle = fert_rat(al)-tfr_base
        
        if(fmiddle <= tfr_increase .and. fright >= tfr_increase)then
            left = middle
            fleft = fmiddle
        else
            right = middle
            fright = fmiddle
        endif
        
        if(abs(right-left) < 1d-5)exit
            
    enddo  

    ! solve model again with the right policy
    sub(0, to_n:NN-1) = (1d0-p(2))     *middle
    sub(1, to_n:NN-1) = p(2)*p(1)      *middle
    sub(2, to_n:NN-1) = p(2)*(1d0-p(1))*middle 
    call solve_model
    
    optimal_policy_fcn = 0d0
    do ie = hs, co
        do iw = 1, WW
            do it = 1, TT
                do in = to_n, min(NN-1, it-1)
                    do is = 1, SS
                
                        i_t(:) = i_g(:, is, in, it, iw, ie)                
                        dist = nu(in, i_t(f), i_t(m), ie)*distrib(is, in, it, iw, ie)*dist_hc(ie)            
                        optimal_policy_fcn = optimal_policy_fcn + dist*(sub(0, in+1) + &
                                                                        sub(1, in+1)*(1d0-dble(h(is, in, it, iw, ie))) + &
                                                                        sub(2, in+1)*dble(h(is, in, it, iw, ie)))
                    enddo
                enddo
            enddo
        enddo
    enddo
    
    write(*,*)p, optimal_policy_fcn
    
end function


! ONE-DIMENSIONAL VERSION OF OPTIMAL POLICY FUNCTION
function optimal_policy_fcn_1(p)

    implicit none    
    
    real*8, intent(in) :: p
    real*8 :: optimal_policy_fcn_1
    
    optimal_policy_fcn_1 = optimal_policy_fcn((/p, 1d0/))

end function


! TO WRITE THE OUTPUT OF BASELINE POLICY REFORMS
subroutine write_policy_output()

    implicit none
    integer :: to_n, ip

    ! write results to the console
    open(76152, file='output/res_policy.out')
    
    do to_n = 0, NN-1
        write(76152, '(a,i1/)')'ONLY FROM CHILD #', to_n+1
        write(76152, '(a)')'name           tfr     subdidy        cost   age_first    age_last'
        do ip = 0, 3
            write(76152, '(a,6f12.5)')name(ip), tfr(ip, to_n)-tfr_base, subsi(ip, to_n)/subsi(0,0), cost(ip, to_n)/cost(0,0), age_first(ip, to_n), age_last(ip, to_n)
        enddo
        do ip = 4, 5
            write(76152, '(a,6f12.5)')name(ip), tfr(ip, to_n)-tfr_base, subsi(ip, to_n)/subsi(4,0), cost(ip, to_n)/cost(4,0), age_first(ip, to_n), age_last(ip, to_n)
        enddo
        write(76152,'(//)')        
    enddo
    
    close(76152)

end subroutine

end module
