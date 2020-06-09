program hash_function_test

   ! This program performs speed tests and collision tests for the integer
   ! and string hash functions in hash_functions.f90 module

   !============================================================================
   use hash_functions

   real           :: t1, t2, tsum, r1
   character(6)   :: name
   character(8)   :: str
   integer        :: ierror, key
   integer        :: i, j, ncol, k, strn(8)
   integer        :: tbl_size, nhash

   integer, dimension(27) :: asciis
   integer, dimension(:), allocatable :: keys, hash
   character(8), dimension(:), allocatable :: keys_s

   procedure(sdbm_64_int),    pointer :: fhash_i => null()
   procedure(sdbm_64_str),    pointer :: fhash_s => null()

   !============================================================================
   ! Hash speed test: Integers
   !============================================================================

   nhash    = 5000000
   tbl_size = int(nhash/0.7+1)

   allocate(keys(nhash),hash(nhash),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   write(*,"('Hash speed test: ',i12,' integers')") nhash

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
   tbl_size = int(nhash/0.7+1)

   write(*,"(/)")
   write(*,"('Hash collision test: ',i12,' integers modulo ',i12)") nhash, tbl_size
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
         key = fhash_i(i*10)
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

   nhash    = 5000000
   tbl_size = int(nhash/0.7+1)

   write(*,"(/)")
   write(*,"('Hash speed test: ',i12,' 8-byte strings')") nhash

   deallocate(keys,hash)
   allocate(keys_s(nhash),hash(nhash),STAT=ierror)
   if(ierror /= 0) write(*,"('Mem alloc error')")

   asciis(1) = 32
   asciis(2:27) = [(i,i=97,122)]

   do i = 1,nhash
      do j = 1,8
         call random_number(r1)
         strn(j) = int(r1*27)+1
      end do

      do j = 1,8
         str(j:j) = achar(asciis(strn(j)))
      end do
      keys_s(i) = str
!      write(*,"(a)") str
!      write(*,"(8(i4))") n
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
   tbl_size = int(nhash/0.7+1)

   write(*,"(/)")
   write(*,"('Hash collision test: ',i12,' 8-byte strings modulo ',i12)") nhash, tbl_size
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

   write(*,"(//,'Done...')")
   read(*,"(a)") str

end program
