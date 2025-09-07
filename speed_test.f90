program speed_test
   ! This program performs speed tests and collision tests for the integer
   ! and string hash functions in hash_functions.f90 module and 
   ! basic hash table operations in hash_table_module.f90

   !============================================================================
   use hash_functions
   use hash_table_module
   implicit none

   integer, parameter :: slen = 10 ! number of string bytes
   real           :: t1, t2, tsum, lf
   character(6)   :: name
   character(slen):: str
   integer        :: ierror, key, n, flag, val, r
   integer        :: i, j, ncol, k, strn(slen), base, a, b
   integer        :: tbl_size, nhash

   integer, dimension(27) :: asciis
   integer, dimension(:), allocatable :: keys, hash
   character(slen), dimension(:), allocatable :: keys_s

   procedure(sdbm_64_int),    pointer :: fhash_i => null()
   procedure(sdbm_64_str),    pointer :: fhash_s => null()
   type(ht_int) :: htable_int
   type(ht_str) :: htable_str

   !============================================================================
   ! Hash speed test: Integers
   !============================================================================

   nhash    = 50000000
   tbl_size = int(nhash/0.7)+1

   allocate(keys(nhash),hash(nhash),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   write(*,"(/)")
   write(*,"('Hash speed test: ',i0,' integers')") nhash

   do k = 1,7
      select case(k)
      case(1); fhash_i => djb2_64_int;    name = 'djb2  '
      case(2); fhash_i => djb2a_64_int;   name = 'djb2a '
      case(3); fhash_i => sdbm_64_int;    name = 'sdbm  '
      case(4); fhash_i => fnv1_64_int;    name = 'fnv1  '
      case(5); fhash_i => fnv1a_64_int;   name = 'fnv1a '
      case(6); fhash_i => mmh2_64_int;    name = 'mmh2  '
      case(7); fhash_i => mmh3_64_int;    name = 'mmh3  '
      end select

      call cpu_time(t1)
      do i = 1,nhash
         key = fhash_i(i)
         hash(i) = modulo(key,tbl_size)+1
      end do
      call cpu_time(t2)
      tsum = t2-t1

      write(*,"(a6,'  kiloHashes per second: ',i8)") name,int(nhash/(1000*tsum))
   end do

   !============================================================================
   ! Hash collision test: Integers
   !============================================================================

   nhash = 50000
   tbl_size = int(nhash/0.7)+1

   write(*,"(/)")
   write(*,"('Hash collision test: ',i0,' integers modulo ',i0)") nhash, tbl_size
   do k = 1,7
      select case(k)
      case(1); fhash_i => djb2_64_int;    name = 'djb2  '
      case(2); fhash_i => djb2a_64_int;   name = 'djb2a '
      case(3); fhash_i => sdbm_64_int;    name = 'sdbm  '
      case(4); fhash_i => fnv1_64_int;    name = 'fnv1  '
      case(5); fhash_i => fnv1a_64_int;   name = 'fnv1a '
      case(6); fhash_i => mmh2_64_int;    name = 'mmh2  '
      case(7); fhash_i => mmh3_64_int;    name = 'mmh3  '
      end select

      do i = 1,nhash
         key = fhash_i(i)
         hash(i) = modulo(key,tbl_size)+1 ! Pretend as if it is fitting into a table
      end do

      ncol = 0
      do i = 1,nhash-1
         if(hash(i) == 0) cycle
         do j = i+1,nhash
            if(hash(j)==0) then
               cycle
            else if(hash(i)==hash(j)) then
               ncol = ncol + 1
               hash(j) = 0
            end if
         end do
      end do
      write(*,"(a6,'  Number of collisions: ',i8)") name,ncol
   end do

   !============================================================================
   ! Hash speed test: Strings
   !============================================================================

   nhash    = 50000000
   tbl_size = int(nhash/0.7)+1

   write(*,"(/)")
   write(*,"('Hash speed test: ',i0,' ',i0,'-byte strings')") nhash, slen

   deallocate(keys,hash)
   allocate(keys_s(nhash),hash(nhash),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   ! Use base conversion to make nhash strings which utilize character indices
   ! 1 through 'base' from among ASCII 32 & 97-122 (space + lower case letters)
   ! e.g. base 5 would use space + a,b,c,d to make strings '   ' through 'ddd'
   asciis(1) = 32
   asciis(2:27) = [(i,i=97,122)]

   base = 7 ! number of ascii characters to use/base to convert to

   do i = 1,nhash
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

   end do

   do k = 1,7
      select case(k)
      case(1); fhash_s => djb2_64_str;    name = 'djb2  '
      case(2); fhash_s => djb2a_64_str;   name = 'djb2a '
      case(3); fhash_s => sdbm_64_str;    name = 'sdbm  '
      case(4); fhash_s => fnv1_64_str;    name = 'fnv1  '
      case(5); fhash_s => fnv1a_64_str;   name = 'fnv1a '
      case(6); fhash_s => mmh2_64_str;    name = 'mmh2  '
      case(7); fhash_s => mmh3_64_str;    name = 'mmh3  '
      end select

      call cpu_time(t1)
      do i = 1,nhash
         key = fhash_s(keys_s(i))
         hash(i) = modulo(key,tbl_size)+1
      end do
      call cpu_time(t2)
      tsum = t2-t1

      write(*,"(a6,'  kiloHashes per second: ',i8)") name,int(nhash/(1000*tsum))
   end do

   !============================================================================
   ! Hash collision test: Strings
   !============================================================================

   nhash = 50000
   tbl_size = int(nhash/0.7)+1

   write(*,"(/)")
   write(*,"('Hash collision test: ',i0,' ',i0,'-byte strings modulo ',i0)") nhash, slen, tbl_size
   do k = 1,7
      select case(k)
      case(1); fhash_s => djb2_64_str;    name = 'djb2  '
      case(2); fhash_s => djb2a_64_str;   name = 'djb2a '
      case(3); fhash_s => sdbm_64_str;    name = 'sdbm  '
      case(4); fhash_s => fnv1_64_str;    name = 'fnv1  '
      case(5); fhash_s => fnv1a_64_str;   name = 'fnv1a '
      case(6); fhash_s => mmh2_64_str;    name = 'mmh2  '
      case(7); fhash_s => mmh3_64_str;    name = 'mmh3  '
      end select

      do i = 1,nhash
         key = fhash_s(keys_s(i))
         hash(i) = modulo(key,tbl_size)+1
      end do

      ncol = 0
      do i = 1,nhash-1
         if(hash(i) == 0) cycle
         do j = i+1,nhash
            if(hash(j)==0) then
               cycle
            else if(hash(i)==hash(j)) then
               ncol = ncol + 1
               hash(j) = 0
            end if
         end do
      end do
      write(*,"(a6,'  Number of collisions: ',i8)") name,ncol
   end do
   deallocate(keys_s)

   !==========================================================

   n = 10000000

   write(*,"(/,'Hash Table Integer Speed Test: ',i0,' 8-byte integers'/,44('='))") n

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

   write(*,"(/,'String Speed Test: ',i0,' ',i2,'-byte strings'/,43('='))") n,slen

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

   !==========================================================

   write(*,"(//,'Done...')")
   !read(*,"(a)") str

end program
