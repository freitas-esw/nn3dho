module mod_rng

   use mod_cte, only: pi, two, kd
   use mod_ios, only: write_out

   implicit none

   private

#include "sprng_f.h"

   integer                :: rng_type = 3     ! cmrg generator 
   integer                :: seed = 985456376 ! example seed
   integer                :: tot_stream = 0
   SPRNG_POINTER, pointer :: stream(:)

   public :: initialize_rng, finalize_rng, make_new_seed
   public :: save_seed, set_seed, get_seed
   public :: save_rng_type, set_rng_type, get_rng_type
   public :: randu, randg

   interface randu
      module procedure randu_scalar_stream
      module procedure randu_scalar, randu_array
      module procedure randu_matrix
   end interface randu

   interface randg
      module procedure randg_scalar_stream
      module procedure randg_scalar, randg_array
      module procedure randg_matrix
   end interface randg

   integer, dimension(4), save :: mm=(/ 502,1521,4071,2107/), ll=(/0,0,0,1/)

   contains

      subroutine initialize_rng( num_stream )
         integer, intent(in) :: num_stream
         integer             :: err, i

         if ( num_stream < 1 ) stop "Error: number of streams must be positive"
        
         tot_stream = num_stream 
         allocate( stream( tot_stream ), stat=err )

         do i = 1, tot_stream
            stream(i) = init_sprng( rng_type, i-1, tot_stream, seed, SPRNG_DEFAULT )
         enddo

      end subroutine initialize_rng

      subroutine finalize_rng()
         integer :: i, aux
         do i = 1, tot_stream
            aux = free_sprng( stream(i) )
         enddo
      end subroutine finalize_rng

      function make_new_seed() result( get_seed )
         integer get_seed
         seed = make_sprng_seed()
         get_seed = seed
      end function make_new_seed

      subroutine save_seed( filename )
         character(*) :: filename

         !call write_out( " ", filename )
         call write_out( "SPRNG seed:", seed, filename )
         call write_out( "Number of streams:", tot_stream, filename )
         ! case ( rng_type )

      end subroutine save_seed

      subroutine set_seed( seed_in )
         integer :: seed_in
         seed = seed_in
      end subroutine set_seed

      function get_seed()
         integer :: get_seed
         get_seed = seed
      end function get_seed

      subroutine save_rng_type( filename )
         character(*) :: filename
         !call write_out( " ", filename )
         call write_out( "SPRNG random generator type:", rng_type, filename )
      end subroutine save_rng_type

      subroutine set_rng_type( rng_type_in )
         integer :: rng_type_in
         rng_type = rng_type_in
      end subroutine set_rng_type

      function get_rng_type()
         integer :: get_rng_type
         get_rng_type = rng_type
      end function get_rng_type

!     subroutine print_info_stream( stream_num )
!        integer :: stream_num
!        integer :: aux
!        aux = print_sprng( stream(stream_num) )
!     end subroutine print_info_stream

      function randu_scalar()
         real(8) :: randu_scalar

         randu_scalar = sprng( stream(1) ) 

      end function randu_scalar
      
      function randu_scalar_stream( stream_num )
         integer :: stream_num
         real(8) :: randu_scalar_stream

         randu_scalar_stream = sprng( stream( stream_num ) ) 

      end function randu_scalar_stream
      
      function randu_array( stream_num, size )
         integer :: stream_num, size, i
         real(8) :: randu_array(size)

         do i = 1, size
            randu_array(i) = sprng( stream( stream_num ) ) 
         enddo

      end function randu_array
      
      function randu_matrix( stream_num, lin, col )
         integer :: stream_num, lin, col, i, j
         real(8) :: randu_matrix(lin,col)

         do i = 1, lin
         do j = 1, col
            randu_matrix(i,j) = sprng( stream( stream_num ) ) 
         enddo
         enddo

      end function randu_matrix

      function randg_scalar()
         real(8) :: randg_scalar, aux

         aux = two * pi * sprng( stream(1) ) 
         randg_scalar = sqrt( -two*log(sprng(stream(1))) )*sin( aux )

      end function randg_scalar
      
      function randg_scalar_stream( stream_num )
         integer :: stream_num
         real(8) :: randg_scalar_stream, aux

         aux = two * pi * sprng( stream(stream_num) ) 
         randg_scalar_stream = &
                 sqrt( -two*log(sprng(stream(stream_num))) )*sin( aux )

      end function randg_scalar_stream
      
      function randg_array( stream_num, size )
         integer :: stream_num, size, i
         real(8) :: randg_array(size), aux1, aux2

         do i = 1, size-1, 2
            aux1 = two * pi * sprng( stream(stream_num) ) 
            aux2 = sqrt( -two*log(sprng(stream(stream_num))) )
            randg_array(i)   = aux2 * sin( aux1 ) 
            randg_array(i+1) = aux2 * cos( aux1 ) 
         enddo

         if ( mod( size, 2 ) == 1 ) then
            aux1 = two * pi * sprng( stream(stream_num) ) 
            aux2 = sqrt( -two*log(sprng(stream(stream_num))) )
            randg_array(size)   = aux2 * sin( aux1 ) 
         end if

      end function randg_array
      
      function randg_matrix( stream_num, lin, col )
         integer :: stream_num, lin, col, i, j
         real(8) :: randg_matrix(lin,col)

         do i = 1, lin
         do j = 1, col
            randg_matrix(i,j) = randg_scalar_stream( stream_num ) 
         enddo
         enddo

      end function randg_matrix

      real(kd) function rannyu()
        real(kd), parameter :: ooto12=1.0_kd/4096.0_kd
        integer, parameter  :: itwo12=4096
        integer :: i1, i2, i3, i4
        i1=ll(1)*mm(4)+ll(2)*mm(3)+ll(3)*mm(2)+ll(4)*mm(1)
        i2=ll(2)*mm(4)+ll(3)*mm(3)+ll(4)*mm(2)
        i3=ll(3)*mm(4)+ll(4)*mm(3)
        i4=ll(4)*mm(4)
        ll(4)=mod(i4,itwo12)
        i3=i3+i4/itwo12
        ll(3)=mod(i3,itwo12)
        i2=i2+i3/itwo12
        ll(2)=mod(i2,itwo12)
        ll(1)=mod(i1+i2/itwo12,itwo12)
        rannyu=ooto12*(float(ll(1)) + ooto12*(float(ll(2)) + ooto12*(float(ll(3)) + ooto12*(float(ll(4))))))
      end function rannyu

end module mod_rng
