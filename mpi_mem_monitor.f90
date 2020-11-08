      module mpi_mem_monitor
      use mpi
      implicit none
      integer,parameter :: max_process = 1048576 ! must be grater than max_node*max_core
      integer,parameter :: io_unit = 76
      integer :: breakpoint_num = 0
      integer(8),dimension(max_process) :: mem_usages !mem_usages(i): memory usage of a process(i)
      !integer,dimension(max_process) :: host_ids !process(i) is running on jazz(host_ids(i))
      contains

      subroutine dump_mem_usages(message, message_unit)
        integer :: rank, i
        integer,optional :: message_unit
        character(32) :: filename
        character(*),optional :: message
        integer(8),dimension(8) :: dt
        integer(8) :: time_in_ms
        character(10) :: date, time, zone
        call set_mem_usages()

        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, i)
        if (rank .eq. 0) then
          if (breakpoint_num==0) call system('mkdir -p mem_usage')
          call date_and_time(date, time, zone, dt)
          dt(1)=dt(1)-2000
          if(present(message_unit) .and. (len_trim(message) .gt. 0))then
            write(message_unit,'(a,3i2.2,"_",3i2.2," mem_usage_",i0)') message,dt(1),dt(2),dt(3),dt(5),dt(6),dt(7),breakpoint_num
          endif
          time_in_ms=((dt(1)-19)*31536000+(dt(2)-1)*2592000+(dt(3)-1)*86400+dt(5)*3600+dt(6)*60+dt(7))*1000+dt(8)

          write(filename, '("mem_usage/",i0)') breakpoint_num
          open(io_unit, file=filename, status='replace')
          write(io_unit,'("#",i0)') time_in_ms
          !write(io_unit, '(a)') "#proc  host   mem_usage(kB)"
          write(io_unit, '(a)') "#proc  mem_usage(kB)"
          do i=1,max_process
            if (mem_usages(i) .eq. 0) cycle
            !write(io_unit,'(i5,i6,i16)') i, host_ids(i), mem_usages(i)
            write(io_unit,'(i5,i16)') i, mem_usages(i)
          end do
          close(io_unit)

          do i=1,max_process
            mem_usages(i)=0
          end do
          breakpoint_num=breakpoint_num+1
        endif
        return
      end subroutine dump_mem_usages

      subroutine set_mem_usages()
        use ifport !if on intel compiler

        character(len=200):: filename=' '
        character(len=200):: line
        character(len=200):: pid_char=' '
        character(len=64) :: host_name
        character(:),allocatable :: known_host_names
        integer :: i
        integer :: pid, host_id
        integer :: ncpu, rank
        integer :: retval
        integer(8) :: mem_usage
        logical :: ifxst
        integer,allocatable :: ista(:)
        allocate(ista(MPI_STATUS_SIZE))


        !--- get host ID
        !known_host_names=""
        !retval = hostnm(host_name) ! ex. 'jazz01'
        !if (scan(known_host_names, host_name).eq.0) then ! if is not known
            !known_host_names=known_host_names//host_name//achar(z'0a')
        !host_id = scan(known_host_names, host_name)

        !--- get process ID
        pid=getpid()
        write(pid_char,'(I8)') pid
        filename='/proc/'//trim(adjustl(pid_char))//'/status'

        !--- read system file
        inquire (file=filename,exist=ifxst)
        if (.not.ifxst) then
          write (*,*) 'err set_mem_usages(): system file does not exist'
          return
        endif

        open(unit=io_unit, file=filename, action='read')
        do
          read (io_unit,'(a)',end=120) line
          if (line(1:6).eq.'VmRSS:') then
             read (line(7:),*) mem_usage
             exit
          endif
        enddo
        120 continue
        close(io_unit)

        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, retval)
        if (rank .eq. 0) then
          mem_usages(1)=mem_usage
          !host_ids(1)=host_id
          call MPI_COMM_SIZE(MPI_COMM_WORLD, ncpu, retval)
          do i=1,ncpu-1
            call MPI_Recv( &
              mem_usage, 1, MPI_INTEGER8, &
              i, i, MPI_COMM_WORLD, ista, retval &
            )
            !call MPI_Recv( &
              !host_id, 1, MPI_INTEGER, &
              !i, i, MPI_COMM_WORLD, ista, retval &
            !)
            mem_usages(i+1)=mem_usage
            !host_ids(i+1)=host_id
          end do
        else
          call MPI_Send( &
            mem_usage, 1, MPI_INTEGER8, &
            0, rank, MPI_COMM_WORLD, retval &
          )
          !call MPI_Send( &
            !host_id, 1, MPI_INTEGER, &
            !0, rank, MPI_COMM_WORLD, retval &
          !)
        end if

        return
      end subroutine set_mem_usages

      end module mpi_mem_monitor
