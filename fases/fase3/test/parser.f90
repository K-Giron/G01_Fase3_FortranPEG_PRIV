
!auto-generated
module parser
   implicit none
   character(len=:), allocatable, private :: input
   integer, private :: savePoint, lexemeStart, cursor

   interface toStr
       module procedure intToStr
       module procedure strToStr
   end interface
   
   
   

   contains
   
   subroutine decirHola
        print *, "Hola desde accion"
    end subroutine decirHola


   function parse(str) result(res)
       character(len=:), allocatable :: str
       integer :: res

       input = str
       cursor = 1

       res = peg_suma()
   end function parse

   
   function peg_suma() result (res)
       integer :: res
       integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               expr_0_0 = peg_num()

               lexemeStart = cursor
               if(.not. acceptString('+')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_num()
               if (.not. acceptEOF()) cycle
               
               res = peg_suma_f0(expr_0_0, expr_0_2)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_suma


   function peg_num() result (res)
       integer :: res
       character(len=:), allocatable :: expr_0_0
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if (.not. (acceptRange('0', '9'))) cycle
               do while (.not. cursor > len(input))
                   if (.not. (acceptRange('0', '9'))) exit
               end do
               expr_0_0 = consumeInput()
           
               
               
               res = peg_num_f0(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_num


   
   function peg_suma_f0(n1, n2) result(res)
       integer :: n1
integer :: n2
       integer :: res
       

        call decirHola()

        res = n1 + n2;
    
   end function peg_suma_f0
   

   function peg_num_f0(num) result(res)
       character(len=:), allocatable :: num
       integer :: res
       
        integer :: tmp

        call decirHola()

        read(num, *) tmp
        res = tmp
    
   end function peg_num_f0
   

   function acceptString(str) result(accept)
       character(len=*) :: str
       logical :: accept
       integer :: offset

       offset = len(str) - 1
       if (str /= input(cursor:cursor + offset)) then
           accept = .false.
           return
       end if
       cursor = cursor + len(str)
       accept = .true.
   end function acceptString

   function acceptRange(bottom, top) result(accept)
       character(len=1) :: bottom, top
       logical :: accept

       if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptRange

   function acceptSet(set) result(accept)
       character(len=1), dimension(:) :: set
       logical :: accept

       if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptSet

   function acceptPeriod() result(accept)
       logical :: accept

       if (cursor > len(input)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptPeriod

   function acceptEOF() result(accept)
       logical :: accept

       if(.not. cursor > len(input)) then
           accept = .false.
           return
       end if
       accept = .true.
   end function acceptEOF

   function consumeInput() result(substr)
       character(len=:), allocatable :: substr

       substr = input(lexemeStart:cursor - 1)
   end function consumeInput

   subroutine pegError()
       print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

       !call exit(1)
   end subroutine pegError

   function intToStr(int) result(cast)
       integer :: int
       character(len=31) :: tmp
       character(len=:), allocatable :: cast

       write(tmp, '(I0)') int
       cast = trim(adjustl(tmp))
   end function intToStr

   function strToStr(str) result(cast)
       character(len=:), allocatable :: str
       character(len=:), allocatable :: cast

       cast = str
   end function strToStr
end module parser
