module mod_global

   use mod_cte, only: kd
   use mod_ios, only: write_out

   implicit none

   contains

      elemental function pbc( r, L )
      ! Periodic Boundary Conditions applied to r for a box side of size L
         implicit none
         real(kd), intent(in) :: r, L
         real(kd)             :: pbc

         pbc = r - L * anint( r / L )
       ! pbc = r - L * nint( r / L )

      end function pbc

      subroutine start_tracking_time( start_cpu_time, start_wall_time )
         !
         real(kd), intent(out) :: start_cpu_time
         integer, intent(out)  :: start_wall_time

         call cpu_time(start_cpu_time)
         call system_clock(start_wall_time)

      end subroutine start_tracking_time

      subroutine end_tracking_time( start_cpu_time, start_wall_time, filename )
         !
         real(kd), intent(in)     :: start_cpu_time
         integer, intent(in)      :: start_wall_time
         character(*), intent(in) :: filename
         real(kd)                 :: end_cpu_time, wall_time
         integer                  :: end_wall_time, count_rate, count_max

         ! Calculation time.
         call cpu_time(end_cpu_time)
         call system_clock(end_wall_time, count_rate, count_max)
         if (end_wall_time < start_wall_time) then
            end_wall_time = end_wall_time + count_max
         end if
         wall_time = real(end_wall_time-start_wall_time,kd)/real(count_rate,kd)

         call write_out( " ", filename )
         call write_out( "Wall time:", wall_time, filename ) 
         call write_out( "CPU time:", end_cpu_time-start_cpu_time, filename )

      end subroutine end_tracking_time

end module mod_global
