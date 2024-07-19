module module1
implicit none
integer, parameter :: im = 10
integer, parameter :: jm = 10
real(8), parameter :: lx = 1.d0
real(8), parameter :: ly = 1.d0
real(8), parameter :: dx = lx / (im-1)
real(8), parameter :: dy = ly / (jm-1)
end module module1
!==============================================================================
program main
    use module1
    implicit none
    integer :: i, j
    real(8), dimension(im,jm) :: x, y
    call grid(x,y)
    call write_result(x,y)
end program main
!==============================================================================
subroutine grid(x,y)
    use module1
    implicit none
    integer :: i, j
    real(8), dimension(im,jm) :: x, y

    x(1,:) = 0.d0
    do j=1, jm
        do i=1, im-1
            x(i+1,j) = x(i,j) + dx
        end do
    end do

    y(:,1) = 0.d0
    do i=1, im
        do j=1, jm-1
            y(i,j+1) = y(i,j) + dy
        end do
    end do
end subroutine
!------------------------------------------------------------------------------
subroutine write_result(x,y)
    use module1
    implicit none
    integer :: i, j
    real(8), dimension(im,jm) :: x, y
    open(10,file="grid.txt")
    open(11,file="grid.plt")
    write(10,*) 'variables=x,y'
    write(10,*) 'zone'
    write(10,*) 'f=point'
    write(10,*) 'i=', im
    write(10,*) 'j=', jm

    do i=1, im
        do j=1, jm
            write(10,*) x(i,j), y(i,j)
        end do
    end do
!------------------------------------------------------------------------------
    write(11,*) 'variables=x,y'
    write(11,*) 'zone'
    write(11,*) 'f=point'
    write(11,*) 'i=',im
    write(11,*) 'j=',jm
    do i=1, im
        do j=1, jm
            write(11,*) x(i,j), y(i,j)
        end do
    end do
end subroutine
!==============================================================================