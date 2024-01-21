program MonteCarloPi
    implicit none
    integer, parameter :: seed = 777, numPoints = 1000000
    real :: x, y, pi_estimate
    integer :: i, points_inside
    real, dimension(2) :: random_numbers

    ! Initialize variables
    points_inside = 0
    call random_seed(put=seed)

    ! Monte Carlo Simulation
    do i = 1, numPoints
        call random_number(random_numbers)
        x = random_numbers(1)
        y = random_numbers(2)
        if (x**2 + y**2 <= 1.0) then
            points_inside = points_inside + 1
        endif
    end do

    ! Estimate Pi
    pi_estimate = 4.0 * points_inside / numPoints
    print *, 'Estimated value of Pi is ', pi_estimate

end program MonteCarloPi
