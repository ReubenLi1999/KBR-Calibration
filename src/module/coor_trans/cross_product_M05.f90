subroutine cross_product_M05(a, b, c)
    use M05 
    implicit none 
    real(rk)    ::a(3), b(3), c(3)

    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)

end subroutine