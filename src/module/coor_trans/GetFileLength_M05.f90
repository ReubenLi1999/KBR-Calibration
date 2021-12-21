subroutine GetFileLength_M05(FileIn, headerlines, Number_Data)

    implicit none
    character(len = 200)    ::FileIn
    integer(kind = 4)       ::headerlines
    integer(kind = 4)       ::Number_Data
    integer(kind = 4)       ::ios
    integer(kind = 4)       ::i

    
    Number_Data = 0
    ios = 0
    open(10, file = FileIn, status = 'old')
    do i = 1, headerlines
        read(10, *, iostat = ios)
    end do
    do while(ios==0)
        read(10, *, iostat = ios)
        if(ios /= 0) exit
        Number_Data = Number_Data + 1
    end do

    close(10)

end subroutine GetFileLength_M05