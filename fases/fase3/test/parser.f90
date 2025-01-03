
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
   
   function replace_special_characters(input_string) result(output_string)
    implicit none
    character(len=:), allocatable, intent(in) :: input_string
    character(len=:), allocatable :: output_string
    character(len=1) :: current_char
    integer :: i, char_ascii, length
    character(len=:), allocatable :: temp_string

    temp_string = ""
    length = len(input_string)

    do i = 1, length
        current_char = input_string(i:i)
        char_ascii = ichar(current_char)

        ! Verificar si el carácter actual es un carácter especial
        select case (char_ascii)
        case (9, 10, 13,32) ! Tabulación, nueva línea, retorno de carro
            ! No incluir este carácter en la salida
        case default
            temp_string = temp_string // current_char
        end select
    end do

    allocate(character(len=len(temp_string)) :: output_string)
    output_string = temp_string
end function replace_special_characters
   
   

   function parse(str) result(res)
       character(len=:), allocatable :: str
       character(len=:), allocatable :: res

       input = str
       cursor = 1

       !limpiar caracteres especiales
       input = replace_special_characters(input)

       res = peg_regla()
   end function parse

   
   function peg_regla() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
       character(len=:), allocatable :: Ayuda
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               expr_0_0 = peg_palabra()

expr_0_1 = peg__()

               lexemeStart = cursor
               if(.not. acceptString('valido')) cycle
               expr_0_2 = consumeInput()
       
               if (.not. acceptEOF()) cycle
               
               res = toStr(expr_0_0)//toStr(expr_0_1)//toStr(expr_0_2)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_regla


   function peg_palabra() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
       character(len=:), allocatable :: Ayuda
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if(.not. acceptString('test')) cycle
               expr_0_0 = consumeInput()
       
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_palabra


   function peg_prohibido() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
       character(len=:), allocatable :: Ayuda
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if(.not. acceptString('bad')) cycle
               expr_0_0 = consumeInput()
       
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg_prohibido


   function peg__() result (res)
       character(len=:), allocatable :: res
       character(len=:), allocatable :: expr_0_0
       character(len=:), allocatable :: Ayuda
       integer :: i

       savePoint = cursor
       
       do i = 0, 1
           select case(i)
           
           case(0)
               cursor = savePoint
               
               
               lexemeStart = cursor
               if (.not. (acceptSet([char(32),char(9),char(10),char(13)]))) cycle
               do while (.not. cursor > len(input))
                   if (.not. (acceptSet([char(32),char(9),char(10),char(13)]))) exit
               end do
               expr_0_0 = consumeInput()
           
               
               
               res = toStr(expr_0_0)


               exit
           
           case default
               call pegError()
           end select
       end do

   end function peg__


   

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
       print '(A,I0,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

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
