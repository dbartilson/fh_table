program hash_table_test
   use hash_table_module
   implicit none

   !==========================================================
   integer, parameter :: slen = 10
   integer            :: i, j, k, flag, val, strn(slen), ierror, &
                         base, r, a, b, n
   real               :: t1, t2, lf
   integer, dimension(27) :: asciis
   integer, dimension(:), allocatable :: keys_i, vals_i
   character(slen), dimension(:), allocatable :: keys_s
   character(slen) :: str
   character(6)         :: name

   type(ht_int) :: htable_int
   type(ht_str) :: htable_str
   !==========================================================

   lf = 0.70 ! Load factor

   !==========================================================

   write(*,"(/,'Integer Functionality Test'/,26('='))")
   n = 1000
   allocate(keys_i(n),vals_i(n),STAT=ierror)
   if(ierror /= 0) write(*,*) "Memory allocation error: keys, vals"

   ! Set up key/val pairs
   do i = 1,n
      keys_i(i) = n-i+1 ! keys are reversed order
      vals_i(i) = i
   end do

   do k = 1,7
      select case(k)
      case(1); name = 'djb2  '
      case(2); name = 'djb2a '
      case(3); name = 'sdbm  '
      case(4); name = 'fnv1  '
      case(5); name = 'fnv1a '
      case(6); name = 'mmh2  '
      case(7); name = 'mmh3  '
      end select

      write(*,*) "Testing algorithm: ",name

      call htable_int%init(n,flag,alg=name,load_factor=lf)
      if(flag /= 0) write(*,*) "Error in init: ",name
      ! If odd numbered, use 'simple' key/val removal scheme instead of 'shift'
      if(mod(k,2) == 1) htable_int%remove => trem_simple_int

      ! Set key/val pairs
      do i = 1,n
         call htable_int%insert(keys_i(i),vals_i(i),flag)
         if(flag /= 0) write(*,*) "Error in set1: ",name
      end do
      ! Get key/val pairs
      do i = 1,n
         call htable_int%get(keys_i(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) write(*,*) "Error in get1: ",name, i, keys_i(i)
      end do
      ! Remove first quarter of keys
      do i = 1,n/4
         call htable_int%remove(keys_i(i),flag)
         if(flag /= 0) write(*,*) "Error in remove: ",name
      end do
      ! Add new set of key/val pairs with negated keys
      do i = 1,n/4
         call htable_int%insert(-keys_i(i),vals_i(i),flag)
         if(flag /= 0) write(*,*) "Error in set2: ",name
      end do
      ! Test get
      do i = 1,n/4
         call htable_int%get(-keys_i(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) write(*,*) "Error in get2: ",name
      end do
      ! Test overwriting (flag will be == 1)
      do i = 1,n/4
         call htable_int%set(-keys_i(i),-vals_i(i),flag)
         if(flag /= 0) write(*,*) "Error in set3: ",name
      end do
      do i = 1,n/4
         call htable_int%get(-keys_i(i),val,flag)
         if(flag /= 0 .or. val /= -vals_i(i)) write(*,*) "Error in get3: ",name
      end do
      call htable_int%destruct(flag)
      if(flag /= 0) write(*,*) "Error in destruct: ",name
   end do
   deallocate(keys_i,vals_i)

   !==========================================================

   n = 10000000

   write(*,"(/,'Integer Speed Test: ',i8,' 8-byte integers'/,44('='))") n

   call htable_int%init(n,flag,alg='mmh2 ',load_factor=lf)

   call cpu_time(t1)
   do i = 1,n
      call htable_int%insert(i,i,flag)
   end do
   call cpu_time(t2)
   write(*,"(' HT Insert: ',f5.2,'s')") t2-t1

   call cpu_time(t1)
   call htable_int%resize(int(n/lf),flag)
   call cpu_time(t2)
   write(*,"(' HT Resize: ',f5.2,'s')") t2-t1

   call cpu_time(t1)
   do i = 1,n
      call htable_int%get(i,val,flag)
      if(i /= val) write(*,"('Val not found: ',i8,i8)") i, val
   end do
   call cpu_time(t2)
   write(*,"(' HT Get:    ',f5.2,'s')") t2-t1

   call htable_int%destruct(flag)

   !==========================================================

   write(*,"(//,'String Functionality Test'/,25('='))")
   n = 1000

   ! Use base conversion to make n strings which utilize character indices
   ! 1 through 'base' from among ASCII 32 & 97-122 (space + lower case letters)
   ! e.g. base 5 would use space + a,b,c,d to make strings '   ' through 'ddd'
   allocate(keys_s(n),vals_i(n),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   asciis(1) = 32
   asciis(2:27) = [(i,i=97,122)]

   base = 7

   do i = 1,n
      b = i
      do j = 1,slen                ! Do j=1,slen Euclidean divisions
         a = b/base                ! integer division, get seed for next digit
         strn(j)  = b - a*base +1  ! Fortran is indexed to 1, modulo is indexed to 0
         b = a                     ! set b = a to start on next digit
      end do

      do j = 1,slen        ! Convert from index to ASCII code to character
         str(j:j) = achar(asciis(strn(j)))
      end do
      keys_s(i) = str
      vals_i(i) = i
   end do

   do k = 1,7
      select case(k)
      case(1); name = 'djb2  '
      case(2); name = 'djb2a '
      case(3); name = 'sdbm  '
      case(4); name = 'fnv1  '
      case(5); name = 'fnv1a '
      case(6); name = 'mmh2  '
      case(7); name = 'mmh3  '
      end select

      write(*,*) "Testing algorithm: ",name

      call htable_str%init(n,flag,alg=name,load_factor=lf)
      if(flag /= 0) write(*,*) "Error in init: ",name
      ! If odd numbered, use 'simple' key/val removal scheme instead of 'shift'
      if(mod(k,2) == 1) htable_str%remove => trem_simple_str

      ! Set key/val pairs
      do i = 1,n
         call htable_str%insert(keys_s(i),vals_i(i),flag)
         if(flag /= 0) write(*,*) "Error in insert: ",name
      end do
      ! Get key/val pairs
      do i = 1,n
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) write(*,*) "Error in get1: ",name
      end do
      ! Remove first quarter of keys
      do i = 1,n/4
         call htable_str%remove(keys_s(i),flag)
         if(flag /= 0) write(*,*) "Error in remove: ",name
      end do
      ! Get key/val pairs
      do i = (n/4+1),n
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) write(*,*) "Error in get2: ",name
      end do
      ! Test overwriting (flag will be == 1)
      do i = (n/4+1),n/2
         call htable_str%set(keys_s(i),-vals_i(i),flag)
         if(flag /= 0) write(*,*) "Error in set: ",name, flag, vals_i(i),keys_s(i)
      end do
      do i = (n/4+1),n/2
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= -vals_i(i)) write(*,*) "Error in get3: ",name
      end do
      call htable_str%destruct(flag)
      if(flag /= 0) write(*,*) "Error in destruct: ",name
   end do
   deallocate(keys_s,vals_i)

   !==========================================================

   n = 1000000

   allocate(keys_s(n),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   asciis(1) = 32
   asciis(2:27) = [(i,i=97,122)]

   base = 5

   do i = 1,n
      b = i
      do j = 1,slen        ! Do j=1,slen Euclidean divisions
         a = b/base        ! integer division, get seed for next digit
         r = b - a*base    ! get remainder for current digit
         b = a             ! set b = a to start on next digit
         strn(j) = r + 1   ! Fortran is indexed to 1, modulo is indexed to 0
      end do

      do j = 1,slen        ! Convert from index to ASCII code to character
         str(j:j) = achar(asciis(strn(j)))
      end do
      keys_s(i) = str
   end do

   write(*,"(/,'String Speed Test: ',i8,' ',i2,'-byte strings'/,43('='))") n,slen

   call htable_str%init(n,flag,alg='mmh2 ',load_factor=lf)

   call cpu_time(t1)
   do i = 1,n
      call htable_str%insert(keys_s(i),i,flag)
      if(flag /= 0) write(*,*) 'Error writing key: ', keys_s(i)
   end do
   call cpu_time(t2)
   write(*,"(' HT Insert: ',f5.2,'s')") t2-t1

   call cpu_time(t1)
   call htable_str%resize(int(n/lf),flag)
   call cpu_time(t2)
   write(*,"(' HT Resize: ',f5.2,'s')") t2-t1

   call cpu_time(t1)
   do i = 1,n
      call htable_str%get(keys_s(i),val,flag)
      if(i /= val) write(*,*) 'Error getting val: ', i, val
   end do
   call cpu_time(t2)
   write(*,"(' HT Get:    ',f5.2,'s')") t2-t1

   call htable_str%destruct(flag)

   write(*,"(//,'Done...')")
   read(*,"(a)") str

end program

