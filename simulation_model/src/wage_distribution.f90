!#####
! TOOLS FOR DISCRETIZING THE WAGE DISTRIBUTION IN A SUITABLE WAY
!#####
module wage_distribution
    
use toolbox

implicit none

! communication variables
real*8 :: com_w, com_LFP, com_p

contains


! SUBROUTINE FOR FINDING PARAMETERS AND DISCRETIZING THE WAGE DISTRIBUTION
subroutine discretize_wages(w, phi, p_cost, w_y, w_bar, LFP_without)

    ! discretized wage distribution
    real*8, intent(out) :: w(6), phi(6)
    
    ! participation cost and wage of a nanny
    real*8, intent(in) :: p_cost, w_y

    ! moments to match
    real*8, intent(in) :: w_bar, LFP_without
    
    ! mean and std parameter of the log-normal distribution
    real*8 :: mu, sigma, mu_log
    
    ! thresholds for discretization
    real*8 :: thresh(6), phi_c(6)
    
    ! for rootfinding process
    real*8 :: p
    logical :: check
    integer :: in
    
    ! set communication variables
    com_w   = w_bar
    com_LFP = LFP_without
    com_p   = p_cost
    
    ! find the mean and std parameter that guarantees that w_bar and LFP are matched
    p = 1d0
    call fzero(p, moment_eq, check)
    if(check)then
        write(*,'(a)')'ERROR OCCURED WHILE TRYING TO MATCH MOMENTS'
    endif
    
    ! set parameters of the log normal distribution
    sigma  = p
    mu     = log(com_p) - sigma*normalCDF_Inv(1d0-LFP_without)
    mu_log = exp(mu + sigma**2/2d0)
    
    ! define thresholds
    thresh(1) = 0d0
    thresh(2) = p_cost
    thresh(3) = p_cost + 0.50*w_y
    thresh(4) = p_cost + 0.75*w_y
    thresh(5) = p_cost + 1.00*w_y
    thresh(6) = p_cost + 1.25*w_y
    
    
    ! now discretize the distribution
    w(1)     = thresh(2)
    phi(1)   = normalCDF(log(thresh(2)), mu, sigma**2)
    phi_c(1) = phi(1)
    
    do in = 2, 5
        w(in)     = mu_log*(normalCDF((log(thresh(in+1))-mu-sigma**2)/sigma)-normalCDF((log(thresh(in))-mu-sigma**2)/sigma)) &
                    /(normalCDF((log(thresh(in+1))-mu)/sigma)-normalCDF((log(thresh(in))-mu)/sigma))
        phi(in)   = normalCDF((log(thresh(in+1))-mu)/sigma)-normalCDF((log(thresh(in))-mu)/sigma)
        phi_c(in) = phi_c(in-1) + phi(in)
    enddo
    
    w(6)     = mu_log*(1d0-normalCDF((log(thresh(6))-mu-sigma**2)/sigma)) &
                    /(1d0-normalCDF((log(thresh(6))-mu)/sigma))
    phi(6)   = 1d0-normalCDF((log(thresh(6))-mu)/sigma)
    phi_c(6) = phi_c(5) + phi(6)
    
    ! subtract participation costs
    w = w - p_cost

end subroutine


! FUNCTION USED TO DETERMINE THE MEAN AND STD PARAMETER OF THE LOG-NORMAL DISTRIBUTION
function moment_eq(p)

    real*8, intent(in) :: p
    real*8 :: moment_eq
    real*8 :: mu, sigma, Phi_LFP       
    
    sigma = p    
    
    Phi_LFP = normalCDF_Inv(1d0 - com_LFP)
    moment_eq = exp(log(com_p) - sigma*(Phi_LFP - sigma/2d0))*(1d0-normalCDF(Phi_LFP - sigma))/com_LFP - com_w
    
end function
    
end module