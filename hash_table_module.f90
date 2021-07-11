!==========================================================
module hash_table_module
!==========================================================

! Flags
!  -12   : improper load factor
!  -11   : improper hash function name
!   -4   : failed to insert item (table full)
!   -3   : new dimension < number of old elements
!   -1   : lookup failed for item (key not in table)
!    0   : operation successfull
!    1   : failed to insert item (key already exists)


use hash_functions

private :: tlookup_int,   tlookup_str, &
           tinit_int,     tinit_str, &
           tdestruct_int, tdestruct_str, &
           tresize_int,   tresize_str, &
           tinsert_int,   tinsert_str, &
           tset_int,      tset_str, &
           tget_int,      tget_str, &
           key_string

!==========================================================
type ht_int
   integer, dimension(:), allocatable :: key, val
   integer :: dim=0, num=0, max_dist=1, max_num
   integer :: deleted   = Z'8000000'
   integer :: empty     = Z'7FFFFFF'
   real    :: max_load_factor = 0.7

   procedure(mmh2_64_int), nopass, pointer :: fhash  => mmh2_64_int
   procedure(trem_shift_int),      pointer :: remove => trem_shift_int
   contains
   procedure, private         :: lookup   => tlookup_int
   procedure, non_overridable :: init     => tinit_int
   procedure, non_overridable :: destruct => tdestruct_int
   procedure, non_overridable :: resize   => tresize_int
   procedure, non_overridable :: insert   => tinsert_int
   procedure, non_overridable :: set      => tset_int
   procedure, non_overridable :: get      => tget_int
end type
!==========================================================

!==========================================================
type key_string
   character(len=:), allocatable :: string
end type
!==========================================================

!==========================================================
type ht_str
   type(key_string), dimension(:), allocatable :: key
   integer,          dimension(:), allocatable :: val
   integer      :: dim=0, num=0, max_dist=1, max_num
   character(1) :: deleted = achar(2)
   character(1) :: empty   = achar(1)
   real         :: max_load_factor = 0.7

   procedure(mmh2_64_str), nopass, pointer :: fhash  => mmh2_64_str
   procedure(trem_shift_str),      pointer :: remove => trem_shift_str
   contains
   procedure, private         :: lookup   => tlookup_str
   procedure, non_overridable :: init     => tinit_str
   procedure, non_overridable :: destruct => tdestruct_str
   procedure, non_overridable :: resize   => tresize_str
   procedure, non_overridable :: insert   => tinsert_str
   procedure, non_overridable :: set      => tset_str
   procedure, non_overridable :: get      => tget_str
end type
!==========================================================

!==========================================================
contains
!==========================================================

!==========================================================
subroutine tinit_int(ht,dim,flag,alg,load_factor)
!==========================================================
   class(ht_int), intent(inout)        :: ht
   integer,       intent(in)           :: dim
   integer,       intent(out)          :: flag
   character(5),  intent(in), optional :: alg
   real,          intent(in), optional :: load_factor

   ht%dim   = dim
   ht%num   = 0
   ht%max_dist = 1

   allocate(ht%key(dim),ht%val(dim),STAT=flag)
   if(flag /= 0) return

   ht%key   = ht%empty
   ht%val   = 0

   if(present(alg)) then
      select case(alg)
      case('djb2 '); ht%fhash => djb2_64_int
      case('djb2a'); ht%fhash => djb2a_64_int
      case('sdbm '); ht%fhash => sdbm_64_int
      case('fnv1 '); ht%fhash => fnv1_64_int
      case('fnv1a'); ht%fhash => fnv1a_64_int
      case('mmh2 '); ht%fhash => mmh2_64_int
      case('mmh3 '); ht%fhash => mmh3_64_int
      case default; flag = -11; return ! Improper hash function
      end select
   end if

   if(present(load_factor)) then
      if(load_factor > 1.0 .or. load_factor <= 0.0) then
         flag = -12
         return
      else
         ht%max_load_factor = load_factor
      end if
   end if

   ht%max_num = int(ht%max_load_factor*ht%dim)

end subroutine
!==========================================================

!==========================================================
subroutine tinit_str(ht,dim,flag,alg,load_factor)
!==========================================================
   class(ht_str), intent(inout)        :: ht
   integer,       intent(in)           :: dim
   integer,       intent(out)          :: flag
   character(5),  intent(in), optional :: alg
   real,          intent(in), optional :: load_factor
   integer                             :: i

   ht%dim   = dim
   ht%num   = 0
   ht%max_dist = 1

   allocate(ht%key(dim),ht%val(dim),STAT=flag)
   if(flag /= 0) return

   do i = 1,dim
      ht%key(i)%string   = ht%empty
   end do
   ht%val   = 0

   if(present(alg)) then
      select case(alg)
      case('djb2 '); ht%fhash => djb2_64_str
      case('djb2a'); ht%fhash => djb2a_64_str
      case('sdbm '); ht%fhash => sdbm_64_str
      case('fnv1 '); ht%fhash => fnv1_64_str
      case('fnv1a'); ht%fhash => fnv1a_64_str
      case('mmh2 '); ht%fhash => mmh2_64_str
      case('mmh3 '); ht%fhash => mmh3_64_str
      case default; flag = -11; return ! Improper hash function
      end select
   end if

   if(present(load_factor)) then
      if(load_factor > 1.0 .or. load_factor <= 0.0) then
         flag = -12 ! Improper load factor
         return
      else
         ht%max_load_factor = load_factor
      end if
   end if

   ht%max_num = int(ht%max_load_factor*ht%dim)

end subroutine
!==========================================================

!==========================================================
subroutine tdestruct_int(ht,flag)
!==========================================================
   class(ht_int), intent(inout)  :: ht
   integer,       intent(out)    :: flag

   deallocate(ht%key,ht%val,STAT=flag)
   ht%dim      = 0
   ht%num      = 0
   ht%max_dist = 1
   ht%max_num  = 0

end subroutine
!==========================================================

!==========================================================
subroutine tdestruct_str(ht,flag)
!==========================================================
   class(ht_str), intent(inout)  :: ht
   integer,       intent(out)    :: flag

   deallocate(ht%key,ht%val,STAT=flag)
   ht%dim      = 0
   ht%num      = 0
   ht%max_dist = 1
   ht%max_num  = 0

end subroutine
!==========================================================

!==========================================================
subroutine tresize_int(ht,dim,flag)
!==========================================================
   class(ht_int), intent(inout)       :: ht
   integer,       intent(in)          :: dim
   integer,       intent(out)         :: flag
   integer                            :: i, n_old
   integer, dimension(:), allocatable :: key_temp, val_temp

   flag = -3
   n_old = ht%dim
   if(dim < ht%num) return ! new size inadequate, return with flag = -3

   call move_alloc(ht%key,key_temp)
   call move_alloc(ht%val,val_temp)
   call ht%init(dim,flag)
   if(flag /= 0) return ! with flag from init

   if(n_old > 0) then
      do i = 1,n_old
         if(.not. any(key_temp(i) == [ht%empty,ht%deleted])) then
            call ht%insert(key_temp(i),val_temp(i),flag)
         end if
      end do
   end if

   deallocate(key_temp,val_temp)

end subroutine
!==========================================================

!==========================================================
subroutine tresize_str(ht,dim,flag)
!==========================================================
   class(ht_str), intent(inout)                :: ht
   integer,       intent(in)                   :: dim
   integer,       intent(out)                  :: flag
   integer                                     :: i, n_old
   integer,          dimension(:), allocatable :: val_temp
   type(key_string), dimension(:), allocatable :: key_temp

   flag = -3
   n_old = ht%dim
   if(dim < ht%num) return ! new size inadequate, return with flag = -3

   call move_alloc(ht%key,key_temp)
   call move_alloc(ht%val,val_temp)
   call ht%init(dim,flag)
   if(flag /= 0) return ! with flag from init

   if(n_old > 0) then
      do i = 1,n_old
         if(.not. any(key_temp(i)%string(1:1) == [ht%empty,ht%deleted])) then
            call ht%insert(key_temp(i)%string,val_temp(i),flag)
         end if
      end do
   end if

   deallocate(key_temp,val_temp)

end subroutine
!==========================================================

!==========================================================
recursive subroutine tinsert_int(ht,key,val,flag)
!==========================================================
   class(ht_int), intent(inout) :: ht
   integer,       intent(in)    :: key, val
   integer,       intent(out)   :: flag
   integer                      :: i, j, dist

   flag = -4
   dist = 1
   i = modulo(ht%fhash(key),ht%dim) + 1

   do j = 1,ht%dim
      if(ht%key(i) == ht%empty) then
         if(ht%num+1 < ht%max_num) then
            ht%key(i) = key
            ht%val(i) = val
            ht%num = ht%num + 1
            if(dist > ht%max_dist) ht%max_dist = dist
            flag = 0
         else
            call ht%resize(2*ht%dim+1,flag)
            call ht%insert(key,val,flag)
         end if
         return
      elseif(key == ht%key(i)) then
         flag = 1 ! key already found in table
         return
      end if
      i = modulo(i,ht%dim) + 1
      dist = dist + 1
   end do

end subroutine
!==========================================================

!==========================================================
recursive subroutine tinsert_str(ht,key,val,flag)
!==========================================================
   class(ht_str), intent(inout) :: ht
   character(*),  intent(in)   :: key
   integer,       intent(in)    :: val
   integer,       intent(out)   :: flag
   integer                      :: i, j, dist

   flag = -4
   dist = 1
   i = modulo(ht%fhash(key),ht%dim) + 1

   do j = 1,ht%dim
      if(ht%key(i)%string(1:1) == ht%empty) then
         if(ht%num+1 < ht%max_num) then
            ht%key(i)%string = key
            ht%val(i) = val
            ht%num = ht%num + 1
            if(dist > ht%max_dist) ht%max_dist = dist
            flag = 0
         else
            call ht%resize(2*ht%dim+1,flag)
            call ht%insert(key,val,flag)
         end if
         return
      elseif(key == ht%key(i)%string) then
         flag = 1 ! key already found in table
         return
      end if
      i = modulo(i,ht%dim) + 1
      dist = dist + 1
   end do

end subroutine
!==========================================================

!==========================================================
pure subroutine tlookup_int(ht,key,i,flag)
!==========================================================
   class(ht_int), intent(in) :: ht
   integer,       intent(in) :: key
   integer,       intent(out):: i, flag
   integer                   :: j, dim

   flag = -1
   dim = max(ht%dim,1)
   i = modulo(ht%fhash(key),dim) + 1

   do j = 1,ht%max_dist
      if(ht%key(i) == key) then
         flag = 0
         return ! with i = matching index
      elseif(ht%key(i) == ht%empty) then
         return ! with flag = -1
      end if
      i = modulo(i,dim) + 1
   end do
end subroutine
!==========================================================

!==========================================================
pure subroutine tlookup_str(ht,key,i,flag)
!==========================================================
   class(ht_str), intent(in) :: ht
   character(*),  intent(in) :: key
   integer,       intent(out):: i, flag
   integer                   :: j, dim

   flag = -1
   dim = max(ht%dim,1)
   i = modulo(ht%fhash(key),dim) + 1

   do j = 1,ht%max_dist
      if(ht%key(i)%string == key) then
         flag = 0
         return ! with i = matching index
      elseif(ht%key(i)%string(1:1) == ht%empty) then
         return ! with flag = -1
      end if
      i = modulo(i,dim) + 1
   end do
end subroutine
!==========================================================

!==========================================================
pure subroutine tset_int(ht,key,val,flag)
!==========================================================
   class(ht_int), intent(inout) :: ht
   integer,       intent(in)    :: key, val
   integer,       intent(out)   :: flag
   integer                      :: i

   call ht%lookup(key,i,flag)

   if(flag == 0) ht%val(i) = val

end subroutine
!==========================================================

!==========================================================
pure subroutine tset_str(ht,key,val,flag)
!==========================================================
   class(ht_str), intent(inout) :: ht
   character(*),  intent(in)    :: key
   integer,       intent(in)    :: val
   integer,       intent(out)   :: flag
   integer                      :: i

   call ht%lookup(key,i,flag)

   if(flag == 0) ht%val(i) = val

end subroutine
!==========================================================

!==========================================================
pure subroutine tget_int(ht,key,val,flag)
!==========================================================
   class(ht_int), intent(in) :: ht
   integer,       intent(in) :: key
   integer,       intent(out):: val, flag
   integer                   :: i

   call ht%lookup(key,i,flag)

   if(flag == 0) val = ht%val(i)

end subroutine
!==========================================================

!==========================================================
pure subroutine tget_str(ht,key,val,flag)
!==========================================================
   class(ht_str), intent(in) :: ht
   character(*),  intent(in) :: key
   integer,       intent(out):: val, flag
   integer                   :: i

   call ht%lookup(key,i,flag)

   if(flag == 0) val = ht%val(i)

end subroutine
!==========================================================

!==========================================================
pure subroutine trem_simple_int(ht,key,flag)
!==========================================================
   class(ht_int), intent(inout) :: ht
   integer,       intent(in)    :: key
   integer,       intent(out)   :: flag
   integer                      :: i

   call ht%lookup(key,i,flag)
   if(flag == -1) return

   ht%key(i) = ht%deleted
   ht%val(i) = 0
   ! Don't decrease ht%num since inserts still have to skip
   ! over deleted keys.

end subroutine
!==========================================================

!==========================================================
pure subroutine trem_simple_str(ht,key,flag)
!==========================================================
   class(ht_str), intent(inout) :: ht
   character(*),  intent(in)    :: key
   integer,       intent(out)   :: flag
   integer                      :: i

   call ht%lookup(key,i,flag)
   if(flag == -1) return

   ht%key(i)%string = ht%deleted
   ht%val(i) = 0
   ! Don't decrease ht%num since inserts still have to skip
   ! over deleted keys.

end subroutine
!==========================================================

!==========================================================
pure subroutine trem_shift_int(ht,key,flag)
!==========================================================
   class(ht_int), intent(inout) :: ht
   integer,       intent(in)    :: key
   integer,       intent(out)   :: flag
   integer                      :: i, j, k, l

   call ht%lookup(key,i,flag)
   if(flag == -1) return

   ! Loop through entries after the one to be deleted (TBD),
   ! If that entry would naturally want to be earlier than TBD
   ! Move contents to TBD and set up that entry location
   ! to be deleted

   j = i ! Start at element to be deleted (TBD)
   do l = 1,ht%dim ! Avoid inf loop for full table
      j = modulo(j,ht%dim) + 1 ! Look at next entry
      if(ht%key(j) /= ht%empty) then
         k = modulo(ht%fhash(ht%key(j)),ht%dim) + 1 ! Get desired position for entry j
         ! If desired position leq to TBD
         if((j > i .and. (k <= i .or.  k > j)) .or. &
            (j < i .and. (k <= i .and. k > j)) ) then ! swap
            ht%key(i) = ht%key(j)
            ht%val(i) = ht%val(j)
            i = j
         end if
      else
         exit ! If empty, we can safely exit early
      end if
   end do
   ht%key(i) = ht%empty
   ht%val(i) = 0
   ht%num    = ht%num - 1

end subroutine
!==========================================================

!==========================================================
pure subroutine trem_shift_str(ht,key,flag)
!==========================================================
   class(ht_str), intent(inout) :: ht
   character(*),  intent(in)    :: key
   integer,       intent(out)   :: flag
   integer                      :: i, j, k, l

   call ht%lookup(key,i,flag)
   if(flag == -1) return

   ! Loop through entries after the one to be deleted (TBD),
   ! If that entry would naturally want to be earlier than TBD
   ! Move contents to TBD and set up that entry location
   ! to be deleted

   j = i ! Start at element to be deleted (TBD)
   do l = 1,ht%dim ! Avoid inf loop for full table
      j = modulo(j,ht%dim) + 1 ! Look at next entry
      if(ht%key(j)%string(1:1) /= ht%empty) then
         k = modulo(ht%fhash(ht%key(j)%string),ht%dim) + 1 ! Get desired position for entry j
         ! If desired position leq to TBD
         if((j > i .and. (k <= i .or.  k > j)) .or. &
            (j < i .and. (k <= i .and. k > j)) ) then ! swap
            ht%key(i)%string = ht%key(j)%string
            ht%val(i) = ht%val(j)
            i = j
         end if
      else
         exit ! If empty, we can safely exit early
      end if
   end do
   ht%key(i)%string(1:1) = ht%empty
   ht%val(i) = 0
   ht%num    = ht%num - 1

end subroutine
!==========================================================

!==========================================================
end module
!==========================================================
