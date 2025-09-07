program functionality_test
   ! This program performs functionality tests for the integer
   ! and string hash functions in hash_functions.f90 module and
   ! the basic hash table operations in hash_table_module.f90

   !============================================================================
   use hash_functions
   use hash_table_module
   implicit none

   integer, parameter :: slen = 10 ! number of string bytes
   character(6)   :: name
   character(slen):: str
   integer        :: ierror, key, value, flag, ret = 0
   integer        :: i, j, k, strn(slen), base, a, b, val, n
   real           :: lf

   integer, dimension(27) :: asciis
   integer, dimension(:), allocatable :: keys_i, vals_i
   character(slen), dimension(:), allocatable :: keys_s

   procedure(sdbm_64_int),    pointer :: fhash_i => null()
   procedure(sdbm_64_str),    pointer :: fhash_s => null()
   type(ht_int) :: htable_int
   type(ht_str) :: htable_str

   !============================================================================
   ! Hash function regression
   !============================================================================

   write(*,"('Hash functionality test')") 
   
   do k = 1,7
      select case(k)
      case(1); fhash_i => djb2_64_int;    name = 'djb2  '; value = 7579091932183233
      case(2); fhash_i => djb2a_64_int;   name = 'djb2a '; value = 7564629544945605
      case(3); fhash_i => sdbm_64_int;    name = 'sdbm  '; value = -6611300386825593088
      case(4); fhash_i => fnv1_64_int;    name = 'fnv1  '; value = -5143479320121640220
      case(5); fhash_i => fnv1a_64_int;   name = 'fnv1a '; value = 7278183591693927820
      case(6); fhash_i => mmh2_64_int;    name = 'mmh2  '; value = -6233134304786354574
      case(7); fhash_i => mmh3_64_int;    name = 'mmh3  '; value = 7410865052703103038
      end select

      key = fhash_i(2147483647)
      if(key == value) then
         write(*,"(a6,'  passed')") name
      else 
         write(*,"(a6,'  failed (key = ',i0,')')") name, key
         ret = ret + 1
      end if
   end do

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
         if(flag /= 0) then 
            write(*,*) "Error in set1: ",name
            ret = ret + 1
         endif
      end do
      ! Get key/val pairs
      do i = 1,n
         call htable_int%get(keys_i(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) then
            write(*,*) "Error in get1: ",name, i, keys_i(i)
            ret = ret + 1
         endif
      end do
      ! Remove first quarter of keys
      do i = 1,n/4
         call htable_int%remove(keys_i(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in remove: ",name
            ret = ret + 1
         endif
      end do
      ! Add new set of key/val pairs with negated keys
      do i = 1,n/4
         call htable_int%insert(-keys_i(i),vals_i(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in set2: ",name
            ret = ret + 1
         endif
      end do
      ! Test get
      do i = 1,n/4
         call htable_int%get(-keys_i(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) then
            write(*,*) "Error in get2: ",name
            ret = ret + 1
         endif
      end do
      ! Test overwriting (flag will be == 1)
      do i = 1,n/4
         call htable_int%set(-keys_i(i),-vals_i(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in set3: ",name
            ret = ret + 1
         endif
      end do
      do i = 1,n/4
         call htable_int%get(-keys_i(i),val,flag)
         if(flag /= 0 .or. val /= -vals_i(i)) then
            write(*,*) "Error in get3: ",name
            ret = ret + 1
         endif
      end do
      call htable_int%destruct(flag)
      if(flag /= 0) then
        write(*,*) "Error in destruct: ",name
        ret = ret + 1
      endif
   end do
   deallocate(keys_i,vals_i)

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
      if(flag /= 0) then
        write(*,*) "Error in init: ",name
        ret = ret + 1
      endif
      ! If odd numbered, use 'simple' key/val removal scheme instead of 'shift'
      if(mod(k,2) == 1) htable_str%remove => trem_simple_str

      ! Set key/val pairs
      do i = 1,n
         call htable_str%insert(keys_s(i),vals_i(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in insert: ",name
            ret = ret + 1
         endif
      end do
      ! Get key/val pairs
      do i = 1,n
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) then
            write(*,*) "Error in get1: ",name
            ret = ret + 1
         endif
      end do
      ! Remove first quarter of keys
      do i = 1,n/4
         call htable_str%remove(keys_s(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in remove: ",name
            ret = ret + 1
         endif
      end do
      ! Get key/val pairs
      do i = (n/4+1),n
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= vals_i(i)) then
            write(*,*) "Error in get2: ",name
            ret = ret + 1
         endif
      end do
      ! Test overwriting (flag will be == 1)
      do i = (n/4+1),n/2
         call htable_str%set(keys_s(i),-vals_i(i),flag)
         if(flag /= 0) then
            write(*,*) "Error in set: ",name, flag, vals_i(i),keys_s(i)
            ret = ret + 1
         endif
      end do
      do i = (n/4+1),n/2
         call htable_str%get(keys_s(i),val,flag)
         if(flag /= 0 .or. val /= -vals_i(i)) then
            write(*,*) "Error in get3: ",name
            ret = ret + 1
         endif
      end do
      call htable_str%destruct(flag)
      if(flag /= 0) then
        write(*,*) "Error in destruct: ",name
        ret = ret + 1
      endif
   end do
   deallocate(keys_s,vals_i)

   !==========================================================

   if (ret /= 0) stop(ret)
   write(*,"(//,'Done...')")
   !read(*,"(a)") str

end program
