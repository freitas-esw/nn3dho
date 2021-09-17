module mod_ios

   use mod_cte

   implicit none

   private
   public :: write_stop, write_out, check_open

   interface write_out
      module procedure write_free, &
              & wout_si, wout_di, &
              & wout_sp, wout_dp, &
              & wout_si_fmt, wout_di_fmt, &
              & wout_sp_fmt, wout_dp_fmt, &
              & wout_si_matrix, wout_sp_matrix, &
              & wout_dp_matrix, wout_zp_matrix, &
              & wout_si_array, wout_sp_array, &
              & wout_dp_array, wout_zp_array, &
              & wout_cp_matrix, wout_cp_array, &
              & wout_si_matrix_fmt, wout_si_array_fmt, &
              & wout_sp_matrix_fmt, wout_sp_array_fmt, &
              & wout_dp_matrix_fmt, wout_dp_array_fmt, &
              & wout_cp_matrix_fmt, wout_cp_array_fmt, &
              & wout_zp_matrix_fmt, wout_zp_array_fmt
   end interface

!   interface wout
!      module procedure &
!   end interface wout

   contains 

      subroutine check_open( file_name, file_num, err )
         ! Subroutine to check if the file file_name is already open and get its 
         ! unit number OR to open a file named file_name (create the file if it doesn't exist)
         implicit none
         character(*), intent(in) :: file_name
         integer, intent(out)     :: file_num, err
         logical                  :: isOpen, isThere

         err=0
         inquire( file=file_name, opened=isOpen, exist=isThere, number=file_num )
         if ( isOpen ) goto 10

         file_num = 1
         do
            inquire( unit=file_num, opened=isOpen )
            if ( .not. isOpen ) exit
            file_num = file_num + 1
         enddo

         if ( isThere ) then
            open( unit=file_num, file=file_name, action='write', status='old', iostat=err )
         else
            open( unit=file_num, file=file_name, action='write', status='new', iostat=err )
         endif

         10 return

      end subroutine check_open

      subroutine write_stop( routine, issue )
         implicit none
         character(*), intent(in) :: routine, issue
         integer                  :: file_num, err

         call check_open( "error.out", file_num, err )
         if ( err /= 0 ) stop 'Abrupt error in write_stop'
         write( unit=file_num, fmt=* ) 'Error: ', routine
         write( unit=file_num, fmt=* ) 'Issue: ', issue
         write( unit=file_num, fmt=* ) 'The program is going to exit'
         stop

      end subroutine write_stop

      subroutine write_free( sentence, filename )
         implicit none
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename 
         integer                  :: file_num, err

         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'write_free', 'open -- err /= 0' )
         write( unit=file_num, fmt=*, iostat=err ) sentence
         if ( err /= 0 ) call write_stop( 'write_free', 'write -- err /= 0' )

      end subroutine write_free

      subroutine wout_si( sentence, int, filename )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename
         integer(si), intent(in)  :: int 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wou_si', 'open -- err /= 0' )
         write( unit=file_num, fmt=*, iostat=err ) sentence, int
         if ( err /= 0 ) call write_stop( 'wout_si', 'write -- err /= 0' )
      end subroutine wout_si

      subroutine wout_si_fmt( sentence, int, filename, fmt )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename, fmt
         integer(si), intent(in)  :: int 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_si_fmt', 'open -- err /= 0' )
         write( unit=file_num, fmt='(A,'//fmt//')', iostat=err ) sentence, int
         if ( err /= 0 ) call write_stop( 'wout_si_fmt', 'write -- err /= 0' )
      end subroutine wout_si_fmt

      subroutine wout_di( sentence, int, filename )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename 
         integer(di), intent(in)  :: int 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_di', 'open -- err /= 0' )
         write( unit=file_num, fmt=*, iostat=err ) sentence, int
         if ( err /= 0 ) call write_stop( 'wout_di', 'write -- err /= 0' )
      end subroutine wout_di

      subroutine wout_di_fmt( sentence, int, filename, fmt )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename, fmt 
         integer(di), intent(in)  :: int 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_di_fmt', 'open -- err /= 0' )
         write( unit=file_num, fmt='(A,'//fmt//')', iostat=err ) sentence, int
         if ( err /= 0 ) call write_stop( 'wout_di_fmt', 'write -- err /= 0' )
      end subroutine wout_di_fmt

      subroutine wout_sp( sentence, real, filename )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename 
         real(sp), intent(in)     :: real 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_sp', 'open -- err /= 0' )
         write( unit=file_num, fmt=*, iostat=err ) sentence, real
         if ( err /= 0 ) call write_stop( 'wout_sp', 'write -- err /= 0' )
      end subroutine wout_sp

      subroutine wout_sp_fmt( sentence, real, filename, fmt )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename, fmt 
         real(sp), intent(in)     :: real 
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_sp_fmt', 'open -- err /= 0' )
         write( unit=file_num, fmt='(A,'//fmt//')', iostat=err ) sentence, real
         if ( err /= 0 ) call write_stop( 'wout_sp_fmt', 'write -- err /= 0' )
      end subroutine wout_sp_fmt

      subroutine wout_dp( sentence, real, filename )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename 
         real(dp), intent(in)     :: real
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_dp', 'open -- err /= 0' )
         write( unit=file_num, fmt=*, iostat=err ) sentence, real
         if ( err /= 0 ) call write_stop( 'wout_dp', 'write -- err /= 0' )
      end subroutine wout_dp

      subroutine wout_dp_fmt( sentence, real, filename, fmt )
         character(*), intent(in) :: sentence 
         character(*), intent(in) :: filename, fmt 
         real(dp), intent(in)     :: real
         integer                  :: file_num, err
         call check_open( filename, file_num, err )
         if ( err /= 0 ) call write_stop( 'wout_dp_fmt', 'open -- err /= 0' )
         write( unit=file_num, fmt='(A,'//fmt//')', iostat=err ) sentence, real
         if ( err /= 0 ) call write_stop( 'wout_dp_fmt', 'write -- err /= 0' )
      end subroutine wout_dp_fmt
      
      subroutine wout_si_matrix( matrix, lin, col, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         integer(si), intent(in)   :: matrix(lin,col)
         integer(si), intent(in)   :: lin, col
         integer                   :: err, file_num, i, j
   
         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_si_matrix", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt_si, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_si_matrix

      subroutine wout_si_matrix_fmt( matrix, lin, col, file_name, fmt ) 
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         integer(si), intent(in)   :: matrix(lin,col)
         integer(si), intent(in)   :: lin, col
         integer                   :: err, file_num, i, j
   
         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_si_matrix_fmt", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_si_matrix_fmt

      subroutine wout_sp_matrix( matrix, lin, col, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         real(sp), intent(in)      :: matrix(:,:)
         integer, intent(in)       :: lin, col
         integer                   :: err, file_num, i, j

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_sp_matrix", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt_sp, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_sp_matrix

      subroutine wout_sp_matrix_fmt( matrix, lin, col, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         real(sp), intent(in)      :: matrix(:,:)
         integer, intent(in)       :: lin, col
         integer                   :: err, file_num, i, j

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_sp_matrix_fmt", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_sp_matrix_fmt

      subroutine wout_dp_matrix( matrix, lin, col, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         real(dp), intent(in)     :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j

         call check_open( file_name, file_num, err )

         if ( err /= 0 ) call write_stop( "wout_dp_matrix", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt_dp, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_dp_matrix

      subroutine wout_dp_matrix_fmt( matrix, lin, col, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         real(dp), intent(in)     :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j

         call check_open( file_name, file_num, err )

         if ( err /= 0 ) call write_stop( "wout_dp_matrix_fmt", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( file_num, fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_dp_matrix_fmt

      subroutine wout_cp_matrix( matrix, lin, col, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         complex(sp), intent(in)  :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j
         character(sc)            :: trim_str

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_matrix", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( trim_str, fmt_cp ) matrix(i,j)
               write( file_num, '(a)', advance='no' ) trim( trim_str )
               !write( file_num, out_fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_cp_matrix

      subroutine wout_cp_matrix_fmt( matrix, lin, col, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         complex(sp), intent(in)  :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j
         character(sc)            :: trim_str

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_matrix_fmt", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( trim_str, fmt ) matrix(i,j)
               write( file_num, '(a)', advance='no' ) trim( trim_str )
               !write( file_num, out_fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_cp_matrix_fmt

      subroutine wout_zp_matrix( matrix, lin, col, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         complex(dp), intent(in)  :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j
         character(sc)            :: trim_str

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_matrix", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( trim_str, fmt_zp ) matrix(i,j)
               write( file_num, '(a)', advance='no' ) trim( trim_str )
               !write( file_num, out_fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_zp_matrix

      subroutine wout_zp_matrix_fmt( matrix, lin, col, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         complex(dp), intent(in)  :: matrix(lin,col)
         integer, intent(in)      :: lin, col
         integer                  :: err, file_num, i, j
         character(sc)            :: trim_str

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_matrix_fmt", "error in check_open" )

         do i = 1, lin
            do j = 1, col
               write( trim_str, fmt ) matrix(i,j)
               write( file_num, '(a)', advance='no' ) trim( trim_str )
               !write( file_num, out_fmt, advance='no' ) matrix(i,j)
            enddo
            write( file_num, '(a)' ) ''
         enddo

      end subroutine wout_zp_matrix_fmt

      subroutine wout_si_array( array, len, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         integer(si), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_si_array", "error in check_open" )

         do i = 1, len
            write( file_num, fmt_si ) array(i)
         enddo

      end subroutine wout_si_array

      subroutine wout_si_array_fmt( array, len, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         integer(si), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_si_array_fmt", "error in check_open" )

         do i = 1, len
            write( file_num, fmt ) array(i)
         enddo

      end subroutine wout_si_array_fmt

      subroutine wout_sp_array( array, len, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         real(sp), intent(in)     :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_sp_array", "error in check_open" )

         do i = 1, len
            write( file_num, fmt_sp ) array(i)
         enddo

      end subroutine wout_sp_array

      subroutine wout_sp_array_fmt( array, len, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         real(sp), intent(in)     :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_sp_array_fmt", "error in check_open" )

         do i = 1, len
            write( file_num, fmt ) array(i)
         enddo

      end subroutine wout_sp_array_fmt

      subroutine wout_dp_array( array, len, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         real(dp), intent(in)     :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_dp_array", "error in check_open" )

         do i = 1, len
            write( file_num, fmt_dp ) array(i)
         enddo

      end subroutine wout_dp_array

      subroutine wout_dp_array_fmt( array, len, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         real(dp), intent(in)     :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_dp_array_fmt", "error in check_open" )

         do i = 1, len
            write( file_num, fmt ) array(i)
         enddo

      end subroutine wout_dp_array_fmt

      subroutine wout_zp_array( array, len, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         complex(dp), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_array", "error in check_open" )

         do i = 1, len
            write( file_num, fmt_zp ) array(i)
         enddo

      end subroutine wout_zp_array

      subroutine wout_zp_array_fmt( array, len, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         complex(dp), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_array_fmt", "error in check_open" )

         do i = 1, len
            write( file_num, fmt ) array(i)
         enddo

      end subroutine wout_zp_array_fmt

      subroutine wout_cp_array( array, len, file_name )
         implicit none
         character(*), intent(in) :: file_name 
         complex(sp), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_array", "error in check_open" )

         do i = 1, len
            write( file_num, fmt_cp ) array(i)
         enddo

      end subroutine wout_cp_array

      subroutine wout_cp_array_fmt( array, len, file_name, fmt )
         implicit none
         character(*), intent(in) :: file_name 
         character(*), intent(in) :: fmt
         complex(sp), intent(in)  :: array(len)
         integer, intent(in)      :: len
         integer                  :: err, file_num, i

         call check_open( file_name, file_num, err )
         if ( err /= 0 ) call write_stop( "wout_zp_array_fmt", "error in check_open" )

         do i = 1, len
            write( file_num, fmt ) array(i)
         enddo

      end subroutine wout_cp_array_fmt

end module mod_ios
