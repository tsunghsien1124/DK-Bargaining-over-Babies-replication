!#####
! FOR WRITING MODEL OUTPUT
!#####
module output
    
use globals

implicit none

contains

! WRITES OUTPUT FILES (OR TARGETS)
subroutine write_output(filename, ie)
    
    character(LEN=*), intent(in) :: filename
    integer, intent(in) :: ie
    integer :: in

    open(234, file=filename)
    
    write(234,'(a/)')'OUTPUT FROM FERTILITY MODEL'
    
    write(234,'(a/)')'AGREEMENT AND FERTILITY OUTCOMES BY GROUP'
    
    ! write absolute tables
    do in = 0, 2
        
        if(in == -1)then
            write(234, '(a)')' TOTAL'
        else
            write(234, '(a, i3)')' NKIDS = ', in
        endif
        write(234, '(a)')' SHARES                                               RATES                                      '
        write(234, '(a)')' -------------------------------------------          -------------------------------------------'
        write(234, '(a)')' f/m |         NA |          A |      TOTAL           f/m |         NA |          A |      TOTAL'
        write(234, '(a)')' -------------------------------------------          -------------------------------------------'
        
        write(234, '(a,5(f10.5,a))')'  NA | ',fert_int(0,0,in,ie),     ' | ', fert_int(0,1,in,ie)     ,' | ', sum(fert_int(0,:,in,ie)), &
                          '            NA | ',fert_out(0,0,in,ie),     ' | ', fert_out(0,1,in,ie)     ,' | '
                                
        write(234, '(a,5(f10.5,a))')'   A | ',fert_int(1,0,in,ie),     ' | ', fert_int(1,1,in,ie)     ,' | ', sum(fert_int(1,:,in,ie)), &
                          '             A | ',fert_out(1,0,in,ie),     ' | ', fert_out(1,1,in,ie)     ,' | '
        
        write(234, '(a)')' -------------------------------------------          -------------------------------------------'
        
        write(234, '(a,3(f10.5,a))')' TOT | ', sum(fert_int(:,0,in,ie)),' | ', sum(fert_int(:,1,in,ie)),' | ', sum(fert_int(:,:,in,ie)), &
                          '           TOT |            |            |           '
        write(234, '(/)')
    enddo        
    
    write(234,'(a,f10.5/)')'TOTAL FERTILITY RATE:            ', fert_rat(ie)
    write(234,'(a,2f10.3/)')'AGE AT FIRST/LAST BIRTH                  :', fert_age_first(ie), fert_age_last(ie)
    write(234,'(a,4f10.3)') 'FEMALE LABOR FORCE PARTICIPATION WITHOUT :', LFP_without(:,ie)
    write(234,'(a,4f10.3/)')'FEMALE LABOR FORCE PARTICIPATION AGE <= 3:', LFP_with(:,ie)
    write(234,'(a        )')'DISTRIBUTION OVER in:        n=0       n=1       n=2       n=3'
    write(234,'(a,4f10.5/)')'                      ', tot_dist(:,ie)
    
    
    ! write conditional tables
    
    write(234, '(a)')' f = 0 / m = 0                                        f = 0 / m = 1                              '
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    write(234, '(a)')' f/m |         NA |          A |      TOTAL           f/m |         NA |          A |      TOTAL'
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    
    write(234, '(a,6(f10.2,a))')'  NA | ',fert_int_cond(0,0,0,0,ie),     ' | ', &
                                          fert_int_cond(0,0,0,1,ie)     ,' | ', &
                                          sum(fert_int_cond(0,0,0,:,ie)), &
                      '            NA | ',fert_int_cond(0,1,0,0,ie),     ' | ', &
                                          fert_int_cond(0,1,0,1,ie)     ,' | ', &
                                          sum(fert_int_cond(0,1,0,:,ie))
                            
    write(234, '(a,6(f10.2,a))')'   A | ',fert_int_cond(0,0,1,0,ie),     ' | ', &
                                          fert_int_cond(0,0,1,1,ie)     ,' | ', &
                                          sum(fert_int_cond(0,0,1,:,ie)), &
                      '             A | ',fert_int_cond(0,1,1,0,ie),     ' | ', &
                                          fert_int_cond(0,1,1,1,ie)     ,' | ', &
                                          sum(fert_int_cond(0,1,1,:,ie))
    
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    
    write(234, '(a)')' f = 1 / m = 0                                        f = 1 / m = 1                              '
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    write(234, '(a)')' f/m |         NA |          A |      TOTAL           f/m |         NA |          A |      TOTAL'
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    
    write(234, '(a,6(f10.2,a))')'  NA | ',fert_int_cond(1,0,0,0,ie),     ' | ', &
                                          fert_int_cond(1,0,0,1,ie)     ,' | ', &
                                          sum(fert_int_cond(1,0,0,:,ie)), &
                      '            NA | ',fert_int_cond(1,1,0,0,ie),     ' | ', &
                                          fert_int_cond(1,1,0,1,ie)     ,' | ', &
                                          sum(fert_int_cond(1,1,0,:,ie))
                            
    write(234, '(a,6(f10.2,a))')'   A | ',fert_int_cond(1,0,1,0,ie),     ' | ', &
                                          fert_int_cond(1,0,1,1,ie)     ,' | ', &
                                          sum(fert_int_cond(1,0,1,:,ie)), &
                      '             A | ',fert_int_cond(1,1,1,0,ie),     ' | ', &
                                          fert_int_cond(1,1,1,1,ie)     ,' | ', &
                                          sum(fert_int_cond(1,1,1,:,ie))
    
    write(234, '(a)')' -------------------------------------------          -------------------------------------------'
    
    write(234, '(/)')
    
    close(234)
    
end subroutine


! WRITES PARAMETERS TO A FILE
subroutine write_params(filename)

    character(LEN=*), intent(in) :: filename
        
    open(234, file=filename)
    
    write(234,'(a)')'PARATEMERS OF THE FERTILITY MODEL'
    
    write(234, '(/a)')'FERTILITY PREFERENCES'
    write(234, '(a,2f12.6)')'MEANS CHILD 1 HS     :', mu(0, f, hs), mu(0, m, hs)
    write(234, '(a,2f12.6)')'MEANS CHILD 2 HS     :', mu(1, f, hs), mu(1, m, hs)
    write(234, '(a,2f12.6)')'MEANS CHILD 3 HS     :', mu(2, f, hs), mu(2, m, hs)
    write(234, '(a,2f12.6)')'MEANS CHILD 1 CO     :', mu(0, f, co), mu(0, m, co)
    write(234, '(a,2f12.6)')'MEANS CHILD 2 CO     :', mu(1, f, co), mu(1, m, co)
    write(234, '(a,2f12.6)')'MEANS CHILD 3 CO     :', mu(2, f, co), mu(2, m, co)
    write(234, '(a,2f12.6)')'STD                  :', sqrt(sig(f)), sqrt(sig(m))
    write(234, '(a,3f12.6)')'CORR                 :', rho
    write(234, '(a,3f12.6)')'PERSISTENCE          :', pi    
    
    
    write(234, '(/a)')'CHILD RELATED'        
    write(234, '(a,2f12.6)')'COST DISTRIB. (f/m)  :', chi(f), chi(m)
    write(234, '(a,3f12.6)')'CHILD CARE COST      :', w_y_base, w_y_cc(1), w_y_cc(2)
    
    write(234, '(/a)')'PARAMETERS AND OTHER STUFF'
    write(234, '(a,2f12.6)')'MEAN WAGES (hs/co)          :', w_f_bar
    write(234, '(a,2f12.6)')'PARTICIPATION COST          :', p_cost
    write(234, '(a,2f12.6)')'DISTRIBUTION (hs/co)        :', dist_hc
    write(234, '(a,2f12.6)')'ECON. OF SCALE              :', alpha
    
    write(234, '(/a,f16.6)')'RSS: ',RSS_est
    
    write(234, *)
    write(234, *)
    write(234,'(a)')'! mean of child preferences'
    write(234, '(a,f25.8,a,f25.8,a)')'mu(0, f, hs) = ',mu(0, f, hs),'d0 ; mu(0, m, hs) = ',mu(0, m, hs),'d0'
    write(234, '(a,f25.8,a,f25.8,a)')'mu(1, f, hs) = ',mu(1, f, hs),'d0 ; mu(1, m, hs) = ',mu(1, m, hs),'d0'
    write(234, '(a,f25.8,a,f25.8,a)')'mu(2, f, hs) = ',mu(2, f, hs),'d0 ; mu(2, m, hs) = ',mu(2, m, hs),'d0'
    write(234, *)
    write(234, '(a,f25.8,a,f25.8,a)')'mu(0, f, co) = ',mu(0, f, co),'d0 ; mu(0, m, co) = ',mu(0, m, co),'d0'
    write(234, '(a,f25.8,a,f25.8,a)')'mu(1, f, co) = ',mu(1, f, co),'d0 ; mu(1, m, co) = ',mu(1, m, co),'d0'
    write(234, '(a,f25.8,a,f25.8,a)')'mu(2, f, co) = ',mu(2, f, co),'d0 ; mu(2, m, co) = ',mu(2, m, co),'d0'
    write(234, *)
    write(234,'(a)')'! variance of fertility preferences'
    write(234, '(a,f25.8,a)')'sig(f)   = (',sqrt(sig(f)),'d0)**2'
    write(234, '(a,f25.8,a)')'sig(m)   = (',sqrt(sig(m)),'d0)**2'
    write(234,*)
    write(234,'(a)')'! correlation'
    write(234, '(a,f25.8,a)')'rho      = ',rho,'d0'
    write(234,*)
    write(234,'(a)')'! persistence of child preferences'
    write(234, '(a,f25.8,a)')'pi       = ',pi,'d0'
    write(234,*)
    write(234,'(a)')'! utility cost of children'
    write(234, '(a,f25.8,a)')'phi_u    = ',phi_u,'d0'
    write(234,*)
    write(234,'(a)')'! ficticious child care wage'
    write(234, '(a,f25.8,a)')'w_y_base = ',w_y_base,'d0'
    write(234,*)
    write(234,'(a)')'! child care wage in cross country analysis'
    write(234, '(a,f25.8,a)')'w_y_cc(1)= ',w_y_cc(1),'d0'
    write(234, '(a,f25.8,a)')'w_y_cc(2)= ',w_y_cc(2),'d0'
    write(234,*)
    write(234,'(a)')'! participation cost'
    write(234, '(a,f25.8,a)')'p_cost   = ',p_cost,'d0'
    
    close(234)

end subroutine


! WRITES ALL ELEMENTS OF THE ESTIMATION PROCESS
subroutine write_estimation_results(filename)
    
    character(LEN=*), intent(in) :: filename
    real*8 :: cells(0:1, 0:1, hs:co)
    integer :: in, ie, ic

    open(234, file=filename)
    
    write(234,'(a/)')'ESTIMATION RESULTS'
    
    write(234,'(a/)')'AGREEMENT AND FERTILITY OUTCOMES BY GROUP'
    
    ! write absolute tables
    do in = 0, 2
        
        write(234, '(a, i3)')' NKIDS = ', in
        
        do ie = hs, co
            cells(0, 0, ie) = fert_int(0, 0, in, ie) - fert_int_targ(0, 0, in, ie)
            cells(0, 1, ie) = fert_int(0, 1, in, ie) - fert_int_targ(0, 1, in, ie)
            cells(1, 0, ie) = fert_int(1, 0, in, ie) - fert_int_targ(1, 0, in, ie)
            cells(1, 1, ie) = fert_int(1, 1, in, ie) - fert_int_targ(1, 1, in, ie)
        enddo
        
        write(234, '(a)')' SHARES HIGH SCHOOL                       SHARES COLLEGE                '
        write(234, '(a)')' ------------------------------           ------------------------------'
        write(234, '(a)')' f/m |         NA |          A            f/m |         NA |          A '
        write(234, '(a)')' ------------------------------           ------------------------------'
        
        write(234, '(a,4(f10.5,a))')'  NA | ',cells(0, 0, hs),     ' | ', cells(0, 1, hs), &
                          '            NA | ',cells(0, 0, co),     ' | ', cells(0, 1, co),' | '
                                
        write(234, '(a,5(f10.5,a))')'   A | ',cells(1, 0, hs),     ' | ', cells(1, 1, hs), &
                          '             A | ',cells(1, 0, co),     ' | ', cells(1, 1, co),' | '
        
        write(234, '(a)')' ------------------------------           ------------------------------'

        write(234, '(/)')
    enddo        
    
    
    ! persistence of intentions
    write(234, '(a)')' PERSISTENCE                   '
    write(234, '(a)')' ------------------------------'
    write(234, '(a)')' f/m |         NA |          A '
    write(234, '(a)')' ------------------------------'
    
    cells(0, 0, hs) = fert_int_cond(0, 0, 0, 0, al) - persist_targ(0, 0)
    cells(0, 1, hs) = fert_int_cond(0, 1, 0, 1, al) - persist_targ(0, 1)
    cells(1, 0, hs) = fert_int_cond(1, 0, 1, 0, al) - persist_targ(1, 0)
    cells(1, 1, hs) = fert_int_cond(1, 1, 1, 1, al) - persist_targ(1, 1)
    
    write(234, '(a,2(f10.2,a))')'  NA | ',cells(0, 0, hs),     ' | ', cells(0, 1, hs)
    write(234, '(a,2(f10.2,a))')'   A | ',cells(1, 0, hs),     ' | ', cells(1, 1, hs)
    
    write(234, '(a)')' ------------------------------'
    
    write(234, '(/)')
    
    ! female labor force participation
    write(234,'(a,4f10.3/)')'FEMALE LABOR FORCE PARTICIPATION:', LFP_with(-1, hs:co)-LFP_with_targ
    
    write(234, '(/)')
    
    ! cross country analysis
    write(234, '(2(a,f8.5))')' COUNTRY chi_m = ',chi_cc(1),'                 COUNTRY chi_m = ',chi_cc(2)
    write(234, '(2(a,f8.3))')' LFP_with      = ',LFP_with_cc(1)-LFP_cc(1),'                 LFP_with      = ',LFP_with_cc(2)-LFP_cc(2)
    write(234, '(a)')' ------------------------------           ------------------------------'
    write(234, '(a)')'  in |     FBLOCK |     MBLOCK             in |     FBLOCK |     MBLOCK '
    write(234, '(a)')' ------------------------------           ------------------------------'
        
    write(234, '(a,4(f10.5,a))')'   1 | ',fblock(1, 1)-fbshare_cc(1, 1),     ' | ', mblock(1, 1)-mbshare_cc(1, 1), &
                        '              1 | ',fblock(1, 2)-fbshare_cc(1, 2),     ' | ', mblock(1, 2)-mbshare_cc(1, 2)
                                
    write(234, '(a,4(f10.5,a))')'   2 | ',fblock(2, 1)-fbshare_cc(2, 1),     ' | ', mblock(2, 1)-mbshare_cc(2, 1), &
                        '              2 | ',fblock(2, 2)-fbshare_cc(2, 2),     ' | ', mblock(2, 2)-mbshare_cc(2, 2)
    write(234, '(a)')' ------------------------------           ------------------------------'
    
    close(234)
    
end subroutine


! CREATES TEX OUTPUT OF ALL MODEL DATA
subroutine tex_output(filename)

    implicit none
    character(LEN=*), intent(in) :: filename
    real*8 :: muf, mum, sigf, sigm, cov, corr

    open(354, file=filename)
    
    muf = sum(v_g(f, :, 1, hs))/dble(SS)
    mum = sum(v_g(m, :, 1, hs))/dble(SS)
    sigf = sum((v_g(f, :, 1, hs)-muf)**2)/dble(SS-1)
    sigm = sum((v_g(m, :, 1, hs)-mum)**2)/dble(SS-1)
    cov = sum((v_g(f, :, 1, hs)-muf)*(v_g(m, :, 1, hs)-mum))/dble(SS-1)
    corr = cov/sqrt(sigf*sigm)
    
    
    write(354,'(a)')'\begin{tabular}{cl@{\hspace{1cm}}SSlSSlSS}\toprule'
    write(354,'(a)')'          && \multicolumn{8}{c}{\em High school} \\\cmidrule{3-10}'
    write(354,'(a)')'          && \multicolumn{2}{c}{$n=0$} && \multicolumn{2}{c}{$n=1$} && \multicolumn{2}{c}{$n=2$} \\'
    write(354,'(a)')'          && {He no}&{He yes}&&{He no} &{He yes}&&{He no} &{He yes}\\\cmidrule{3-10}'
    write(354,'(a,6(f5.2,a))')' \em Data  & She no     & ', &
            fert_int_targ(0,0,0,hs),'  & ',fert_int_targ(0,1,0,hs),'  && ', &
            fert_int_targ(0,0,1,hs),'  & ',fert_int_targ(0,1,1,hs),'  && ', &
            fert_int_targ(0,0,2,hs),'  & ',fert_int_targ(0,1,2,hs),' \\'
    write(354,'(a,6(f5.2,a))')'           & She yes    & ', &
            fert_int_targ(1,0,0,hs),'  & ',fert_int_targ(1,1,0,hs),'  && ', &
            fert_int_targ(1,0,1,hs),'  & ',fert_int_targ(1,1,1,hs),'  && ', &
            fert_int_targ(1,0,2,hs),'  & ',fert_int_targ(1,1,2,hs),' \\\cmidrule{3-10}'    
    write(354,'(a,6(f5.2,a))')' \em Model & She no     & ', &
            fert_int(0,0,0,hs),'  & ',fert_int(0,1,0,hs),'  && ', &
            fert_int(0,0,1,hs),'  & ',fert_int(0,1,1,hs),'  && ', &
            fert_int(0,0,2,hs),'  & ',fert_int(0,1,2,hs),' \\'
    write(354,'(a,6(f5.2,a))')'           & She yes    & ', &
            fert_int(1,0,0,hs),'  & ',fert_int(1,1,0,hs),'  && ', &
            fert_int(1,0,1,hs),'  & ',fert_int(1,1,1,hs),'  && ', &
            fert_int(1,0,2,hs),'  & ',fert_int(1,1,2,hs),' \\\midrule'
    write(354,'(a)')'          && \multicolumn{8}{c}{\em College} \\\cmidrule{3-10}'
    write(354,'(a)')'          && \multicolumn{2}{c}{$n=0$} && \multicolumn{2}{c}{$n=1$} && \multicolumn{2}{c}{$n=2$} \\'
    write(354,'(a)')'          && {He no}&{He yes}&&{He no} &{He yes}&&{He no} &{He yes}\\\cmidrule{3-10}'
    write(354,'(a,6(f5.2,a))')' \em Data  & She no     & ', &
            fert_int_targ(0,0,0,co),'  & ',fert_int_targ(0,1,0,co),'  && ', &
            fert_int_targ(0,0,1,co),'  & ',fert_int_targ(0,1,1,co),'  && ', &
            fert_int_targ(0,0,2,co),'  & ',fert_int_targ(0,1,2,co),' \\'
    write(354,'(a,6(f5.2,a))')'           & She yes    & ', &
            fert_int_targ(1,0,0,co),'  & ',fert_int_targ(1,1,0,co),'  && ', &
            fert_int_targ(1,0,1,co),'  & ',fert_int_targ(1,1,1,co),'  && ', &
            fert_int_targ(1,0,2,co),'  & ',fert_int_targ(1,1,2,co),' \\\cmidrule{3-10}'    
    write(354,'(a,6(f5.2,a))')' \em Model & She no     & ', &
            fert_int(0,0,0,co),'  & ',fert_int(0,1,0,co),'  && ', &
            fert_int(0,0,1,co),'  & ',fert_int(0,1,1,co),'  && ', &
            fert_int(0,0,2,co),'  & ',fert_int(0,1,2,co),' \\'
    write(354,'(a,6(f5.2,a))')'           & She yes    & ', &
            fert_int(1,0,0,co),'  & ',fert_int(1,1,0,co),'  && ', &
            fert_int(1,0,1,co),'  & ',fert_int(1,1,1,co),'  && ', &
            fert_int(1,0,2,co),'  & ',fert_int(1,1,2,co),' \\\bottomrule'
    write(354,'(a)')'\end{tabular}'
    write(354, '(//)')
    
    write(354,'(a)')'\begin{tabular}{lcSScSS}\toprule'
    write(354,'(a)')'             &&\multicolumn{2}{c}{{\em Data}} &&  \multicolumn{2}{c}{{\em Model}} \\\cmidrule{3-7}'
    write(354,'(a)')'             && {He no} & {He yes} && {He no} & {He yes} \\\cmidrule{3-7}'
    write(354,'(a,4(f5.2,a))')'  She no     &&   ',persist_targ(0,0),' &    ',persist_targ(0,1),' &&   ', &
                            fert_int_cond(0,0,0,0,al),' &    ',fert_int_cond(0,1,0,1,al),' \\'
    write(354,'(a,4(f5.2,a))')'  She yes    &&   ',persist_targ(1,0),' &    ',persist_targ(1,1),' &&   ', &
                            fert_int_cond(1,0,1,0,al),' &    ',fert_int_cond(1,1,1,1,al),' \\\bottomrule'
    write(354,'(a)')'\end{tabular}'
    write(354, '(//)')
    
    write(354,'(a)')'\begin{tabular}{lcSScSS}\toprule'
    write(354,'(a)')'             && \multicolumn{2}{c}{\em Data} && \multicolumn{2}{c}{\em Model}  \\\cmidrule{3-4}\cmidrule{6-7}'
    write(354,'(a)')'             && \multicolumn{2}{c}{Child under 3:} &&   \multicolumn{2}{c}{Child under 3:}\\[-0.75ex]'
    write(354,'(a)')'             && {No}    & {Yes}    && {No}   & {Yes}  \\\cmidrule{3-4}\cmidrule{6-7}'
    write(354,'(a,4(f5.2,a))')'  High school &&   ',LFP_without_targ(hs),' &    ',LFP_with_targ(hs),' &&   ', &
                            LFP_without(-1,hs),' &    ',LFP_with(-1,hs),' \\'
    write(354,'(a,4(f5.2,a))')'  College     &&   ',LFP_without_targ(co),' &    ',LFP_with_targ(co),' &&   ', &
                            LFP_without(-1,co),' &    ',LFP_with(-1,co),' \\\bottomrule'
    write(354,'(a)')'\end{tabular}'
    write(354, '(//)')
    
    write(354, '(a)')'\begin{tabular}{lcrr}\toprule'
    write(354, '(a)')'  {\em Description}           & {\em Parameter} & \multicolumn{2}{c}{\em Value}\\\midrule'
    write(354, '(a,2(f8.4,a))')'  Mean women first child   & $\mu_{f,1}$     & ',mu(0, f, hs),' & ',mu(0, f, co),'  \\'
    write(354, '(a,2(f8.4,a))')'  Mean women second child  & $\mu_{f,2}$     & ',mu(1, f, hs),' & ',mu(1, f, co),'  \\'
    write(354, '(a,2(f8.4,a))')'  Mean women third child   & $\mu_{f,3}$     & ',mu(2, f, hs),' & ',mu(2, f, co),'  \\'
    write(354, '(a,f8.4,a)')'  Std. dev. women             & $\sigma_f $     & \multicolumn{2}{c}{',sqrt(sig(f)),'}  \\\midrule'
    write(354, '(a,2(f8.4,a))')'  Mean men first child     & $\mu_{m,1}$     & ',mu(0, m, hs),' & ',mu(0, m, co),'  \\'
    write(354, '(a,2(f8.4,a))')'  Mean men second child    & $\mu_{m,2}$     & ',mu(1, m, hs),' & ',mu(1, m, co),'  \\'
    write(354, '(a,2(f8.4,a))')'  Mean men third child     & $\mu_{m,3}$     & ',mu(2, m, hs),' & ',mu(2, m, co),'  \\'
    write(354, '(a,f8.4,a)')'  Std. dev. men               & $\sigma_m $     & \multicolumn{2}{c}{',sqrt(sig(m)),'}  \\\midrule'
    write(354, '(a,f8.4,a)')'  Correlation                 & $\rho$          & \multicolumn{2}{c}{',corr,'}  \\'
    write(354, '(a,f8.4,a)')'  Persistence                 & $\pi$           & \multicolumn{2}{c}{',pi,'}  \\\midrule'
    write(354, '(a,f8.4,a)')'  Utility cost                & $\phi_u$        & \multicolumn{2}{c}{',phi_u,'}  \\'
    write(354, '(a,f8.4,a)')'  Child care cost             & $w_y$           & \multicolumn{2}{c}{',w_y,'}  \\'
    write(354, '(a,f8.4,a)')'  Participation cost          & $p_c$           & \multicolumn{2}{c}{',p_cost,'}  \\\bottomrule'
    write(354, '(a)')'\end{tabular}'       
    write(354,'(//)')
    
    write(354,'(a)')'\begin{tabular}{lcS}\toprule'
    write(354, '(a,f6.4,a)')'    Total fertility rate                            &   ',fert_rat(al),'  \\\midrule'
    write(354, '(a,f6.4,a)')'    Fraction of couples without children            &   ',tot_dist(0,al),'  \\'
    write(354, '(a,f6.4,a)')'    Fraction of couples with one child              &   ',tot_dist(1,al),'  \\'
    write(354, '(a,f6.4,a)')'    Fraction of couples with two children           &   ',tot_dist(2,al),'  \\'
    write(354, '(a,f6.4,a)')'    Fraction of couples with more than two children &   ',tot_dist(3,al),'  \\\bottomrule'
    write(354,'(a)')'\end{tabular}'
    
    close(354)

end subroutine
    
    
end module
