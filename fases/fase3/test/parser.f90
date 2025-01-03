
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
   
   

   function parse(str) result(res)
       character(len=:), allocatable :: str
       character(len=:), allocatable :: res

       input = str
       cursor = 1

       res = peg_sum()
   end function parse

   
   function peg_sum() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_2_0
character(len=:), allocatable :: expr_3_0
       integer :: i

       savePoint = cursor
       
       do i = 0, 4
           select case(i)
           
           case(0)
               cursor = savePoint
               
               expr_0_0 = peg_numeros()
expr_0_1 = peg_minuscula()
               if (.not. acceptEOF()) cycle
               
               res = toStr(expr_0_0)//toStr(expr_0_1)


               exit
           
           case(1)
               cursor = savePoint
               
               expr_1_0 = peg_mayuscula()
expr_1_1 = peg_minuscula()
               if (.not. acceptEOF()) cycle
               
               res = toStr(expr_1_0)//toStr(expr_1_1)


               exit
           
           case(2)
               cursor = savePoint
               
               expr_2_0 = peg_minuscula()
               if (.not. acceptEOF()) cycle
               
               res = toStr(expr_2_0)


               exit
           
           case(3)
               cursor = savePoint
               
               expr_3_0 = peg_mayuscula()
               if (.not. acceptEOF()) cycle
               
               res = toStr(expr_3_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_sum


   function peg_numeros() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if(.not. acceptString('hola')) cycle
               expr_0_0 = consumeInput()
       
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_numeros


   function peg_minuscula() result (res)
       character(len=:), allocatable :: res
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
           
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_minuscula


   function peg_mayuscula() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if(.not. acceptString('welcome')) cycle
               expr_0_0 = consumeInput()
       
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_mayuscula


   

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
