subroutine iau_main_M05(transflag, JD_TT, leapsecond, DUT1, xp, yp, M_Rotaion)

    use M05
    implicit none

!----declare variables----------
    integer(ik),  intent(in   )         :: transflag
    integer(ik),  intent(in   )         :: leapsecond
    real(rk),     intent(in   )         :: JD_TT
    real(rk),     intent(in   )         :: DUT1
    real(rk),     intent(in   )         :: xp, yp
    real(rk),     intent(  out)         :: M_Rotaion(3, 3)   

    !---------------------------
    real(rk)                            :: date1, date2
    DOUBLE PRECISION                    :: DPSI, DEPS, EPSA   !章动角，章动角修正值
    DOUBLE PRECISION                    :: RPOM(3,3), RBPN(3,3), RT2C(3,3)            !极移矩阵，岁差章动矩阵，ITRS到GCRS的转换矩阵
    DOUBLE PRECISION                    :: RC2T(3,3) !GCRS到ITRS的转换矩阵
    DOUBLE PRECISION                    :: RB(3,3), RP(3,3), RBP(3,3),RN(3,3)!
    DOUBLE PRECISION                    :: iau_GST06,iau_SP00 !计算GAST\sp的函数
    DOUBLE PRECISION                    :: GST                !格林尼治真恒星时
    DOUBLE PRECISION                    :: sp                 !极移矩阵中的一个量
    DOUBLE PRECISION                    :: DJ1, DJ2           !UT1儒略日的两部分
!-------------------------------

    
    date2 = MOD(JD_TT - 0.5, 1.0)
    date1 = JD_TT - date2

    call iau_NUT06A ( DATE1, DATE2, DPSI, DEPS )
    call iau_PN06 ( DATE1, DATE2, DPSI, DEPS,EPSA, RB, RP, RBP, RN, RBPN )
    
    DJ1=DATE1
    DJ2=DATE2-(LEAPSECOND+32.184d0-DUT1)/86400D0
    GST=iau_GST06 ( DJ1, DJ2, date1, date2, RBPN )
    call iau_RZ ( GST, RBPN )
    
    sp=iau_SP00 ( DATE1, DATE2 )
    call iau_POM00 ( XP, YP, SP, RPOM )
        
    call iau_RXR ( RPOM, RBPN, RC2T )
    
    if(transflag == 1) then
        M_Rotaion = RC2T
    else
        call iau_TR(RC2T,RT2C)
        M_Rotaion = RT2C
    end if




end subroutine iau_main_M05