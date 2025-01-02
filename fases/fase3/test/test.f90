program test
    use parser
    implicit none

    character(len=100) :: filename
    character(len=:), allocatable :: input
    integer :: resultado
    integer :: len
    logical :: exists

    ! Verificar que nos pasen por lo menos un argumento
    if (command_argument_count() == 0) then
        print *, "error: no input file"
        stop
    end if

    ! Leer el primer argumento de línea de comandos
    call get_command_argument(1, filename)

    ! Verificar que el archivo existe y obtener su tamaño
    inquire(file=filename, exist=exists, size=len)
    if (exists) then
        open(unit=1, file=filename, status='old', action='read', &
             access='stream', form='unformatted')

        ! Reservar memoria para leer todo el contenido
        allocate(character(len=len) :: input)
        read (1) input

        ! Aquí parse(...) devuelve un entero, así que 'resultado' es INTEGER
        resultado = parse(input)

        print *, "Resultado de parse:", resultado

        close(1)
    else
        print *, "error: file is not present"
        stop
    end if

end program test