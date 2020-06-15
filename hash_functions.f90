!==========================================================
module hash_functions
!==========================================================

!==========================================================
! Private variables and procedures
!==========================================================

integer, private, parameter :: mmh_seed = 7006975885128933000, &
                               djb2_init = 5381, &
                               fnv_offset = -3750763034362896400, &
                               fnv_prime = 1099511628211
                               ! fnv1 64-bit (uint8) offset -> decimal -> int8

private :: mmh3_64_block, mmh3_64_final, rotl_64, fmix_64, &
           mmh2_64_block, mmh2_64_final

!==========================================================

interface mmh3_64
   procedure mmh3_64_int, mmh3_64_str
end interface

interface mmh2_64
   procedure mmh2_64_int, mmh2_64_str
end interface

interface djb2_64
   procedure djb2_64_int, djb2_64_str
end interface

interface djb2a_64
   procedure djb2a_64_int, djb2a_64_str
end interface

interface sdbm_64
   procedure sdbm_64_int, sdbm_64_str
end interface

interface fnv1_64
   procedure fnv1_64_int, fnv1_64_str
end interface

interface fnv1a_64
   procedure fnv1a_64_int, fnv1a_64_str
end interface

!==========================================================
contains
!==========================================================

!==========================================================
pure function mmh3_64_int(key) result(hash)
!==========================================================
   ! see notes for mmh3_64_str.
   ! This version is for 8 byte integers, skipping some unnecessary parts
   integer, intent(in) :: key
   integer             :: hash, h1, h2, k1, k2

   h1 = mmh_seed
   h2 = mmh_seed
   k1 = key
   k2 = 0

   ! Treat 8 byte int as though it is 16 byte
   call mmh3_64_block(k1,k2,h1,h2)
   hash = mmh3_64_final(h1,h2)
end function
!==========================================================

!==========================================================
pure function mmh3_64_str(key) result(hash)
!==========================================================
   ! derived from MurmurHash3_x64_128 by Austin Appleby
   !     (https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp)
   ! while looking at Jannis Teunissen's version
   !     (https://github.com/jannisteunissen/murmur3-fortran/blob/master/m_murmur3.f90)

   character(*), intent(in) :: key
   integer             :: hash, h1, h2, k1, k2, &
                          klen, nblocks, i, tlen, ki8
   integer(1)          :: ki
   integer, parameter  :: shifts(15) = [0,8,16,24,32,40,48,56, &
                                        0,8,16,24,32,40,48]
   integer, parameter  :: c1 = -8663945395140668459, &
                          c2 = -3677843016744856600

   h1 = mmh_seed
   h2 = mmh_seed
   klen = len_trim(key)
   nblocks = klen/16

   ! Loop over complete 16 byte blocks
   if(nblocks > 0) then
      do i = 1,nblocks
         k1 = transfer(key(16*(i-1)+1:16*(i-1)+8),k1)
         k2 = transfer(key(16*(i-1)+9:16*i),k1)
         call mmh3_64_block(k1,k2,h1,h2)
      end do
   end if

   ! Tail
   k1 = 0
   k2 = 0
   tlen = modulo(klen,16)
   if(tlen > 0) then
      if(tlen > 8) then
         do i = tlen,9,-1
            ki8 = transfer(key(nblocks*16+i:nblocks*16+i),ki)
            k2 = ieor(k2,shiftl(ki8,shifts(i)))
         end do
         k2 = k2 * c2
         k2 = rotl_64(k2,33)
         k2 = k2 * c1
         h2 = ieor(h2,k2)
         tlen = 8
      end if

      do i = tlen,1,-1
         ki8 = transfer(key(nblocks*16+i:nblocks*16+i),ki)
         k1 = ieor(k1,shiftl(ki8,shifts(i)))
      end do
      k1 = k1 * c1
      k1 = rotl_64(k1,31)
      k1 = k1 * c2
      h1 = ieor(h1,k1)
   end if

   ! Finalize
   hash = mmh3_64_final(h1,h2)

end function
!==========================================================

!==========================================================
pure subroutine mmh3_64_block(k1,k2,h1,h2)
!==========================================================
   ! see notes for mmh3_64_str.
   integer, intent(inout):: k1, k2, h1, h2
   integer, parameter    :: c1 = -8663945395140668459, &
                            c2 = -3677843016744856600

   k1 = k1 * c1
   k1 = rotl_64(k1,31)
   k1 = k1 * c2

   h1 = ieor(h1,k1)
   h1 = rotl_64(h1,27)
   h1 = h1 + h2
   h1 = 5*h1 + 1390208809

   k2 = k2 * c2
   k2 = rotl_64(k2,33)
   k2 = k2 * c1

   h2 = ieor(h2,k2)
   h2 = rotl_64(h2,31)
   h2 = h2 + h1
   h2 = 5*h2 + 944331445

end subroutine
!==========================================================

!==========================================================
pure function mmh3_64_final(h1,h2) result(hash)
!==========================================================
   ! see notes for mmh3_64_str.
   integer, intent(in)    :: h1, h2
   integer                :: hash, hi1, hi2

   hi1 = ieor(h1,16)
   hi2 = ieor(h2,16)

   hi1 = hi1 + hi2
   ! h2 = h2 + h1

   hi1 = fmix_64(hi1)
   ! h2 = fmix_64(h2)

   hash = hi1
end function
!==========================================================

!==========================================================
pure integer function rotl_64(x,r)
!==========================================================
   integer, intent(in) :: x, r
   rotl_64 = ior(shiftl(x,r),shiftr(x,64-r))
end function
!==========================================================

!==========================================================
pure integer function fmix_64(x)
!==========================================================
   integer, intent(in) :: x
   fmix_64 = ieor(x,shiftr(x,33))
   fmix_64 = fmix_64 * (-49064778989727740)
   fmix_64 = ieor(x,shiftr(x,33))
   fmix_64 = fmix_64 * (-4265267296055464877)
   fmix_64 = ieor(fmix_64,shiftr(fmix_64,33))
end function
!==========================================================

!==========================================================
pure function mmh2_64_int(key) result(hash)
!==========================================================
   ! see notes for mmh2_64_str.
   integer, intent(in) :: key
   integer             :: hash, h, k
   ! uint8 => decimal => int8 => times 8 (overflow)
   integer, parameter  :: m8 = 3829533694005035232

   h = ieor(mmh_seed,m8)
   k = key

   call mmh2_64_block(k,h)

   hash = mmh2_64_final(h)

end function
!==========================================================

!==========================================================
pure function mmh2_64_str(key) result(hash)
!==========================================================
   ! derived from MurmurHash64A by Austin Appleby
   !     (https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp)
   character(*), intent(in) :: key
   integer                  :: hash, h, k, i, &
                               klen, tlen, nblocks
   integer(1)               :: ki
   integer, parameter       :: m = -4132994306676758500, &
                               r = 47
   integer, parameter  :: shifts(7) = [0,8,16,24,32,40,48]

   klen = len_trim(key)
   nblocks = klen/8

   h = ieor(mmh_seed,klen*m)

   if(nblocks > 0) then
      do i = 1,nblocks
         k = transfer(key(8*(i-1)+1:8*i),k)
         call mmh2_64_block(k,h)
      end do
   end if

   ! Tail
   tlen = modulo(klen,8)
   if(tlen > 0) then
      do i = tlen,1,-1
         k = transfer(key(nblocks*8+i:nblocks*8+i),ki)
         h = ieor(h,shiftl(k,shifts(i)))
      end do
      h = h*m
   end if

   hash = mmh2_64_final(h)

end function
!==========================================================

!==========================================================
pure subroutine mmh2_64_block(k,h)
!==========================================================
   ! see notes for mmh2_64_str.
   integer, intent(inout) :: k, h
   integer, parameter     :: m = -4132994306676758500, &
                             r = 47

   k = k * m
   k = ieor(k,shiftr(k,r))
   k = k * m
   h = ieor(h,k)
   h = h * m

end subroutine
!==========================================================

!==========================================================
pure function mmh2_64_final(h) result(hi)
!==========================================================
   ! see notes for mmh2_64_str.
   integer, intent(in) :: h
   integer             :: hi
   integer, parameter  :: m = -4132994306676758500, &
                          r = 47

   hi = ieor(h,shiftr(h,r))
   hi = hi * m
   hi = ieor(hi,shiftr(hi,r))

end function
!==========================================================

!==========================================================
pure function djb2_64_int(key) result(hash)
!==========================================================
   ! attributable to Dan Bernstein
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   integer, intent(in) :: key
   integer             :: hash, i, ki

   hash = djb2_init
   do i = 1,8
      ki = ibits(key,8*(i-1),8)
      hash = (hash * 33) + ki
   end do

end function
!==========================================================

!==========================================================
pure function djb2_64_str(key) result(hash)
!==========================================================
   ! attributable to Dan Bernstein
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   character(*), intent(in) :: key
   integer                  :: hash, i, klen, ki
   integer(1)               :: split

   klen = len_trim(key)

   hash = djb2_init
   do i = 1,klen
      ki = transfer(key(i:i),split)
      hash = (hash * 33) + ki
   end do

end function
!==========================================================

!==========================================================
pure function djb2a_64_int(key) result(hash)
!==========================================================
   ! attributable to Dan Bernstein
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   integer, intent(in) :: key
   integer             :: hash, i, ki

   hash = djb2_init
   do i = 1,8
      ki = ibits(key,8*(i-1),8)
      hash = ieor(33*hash,ki)
   end do

end function
!==========================================================

!==========================================================
pure function djb2a_64_str(key) result(hash)
!==========================================================
   ! attributable to Dan Bernstein
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   character(*), intent(in) :: key
   integer                  :: hash, i, klen, ki
   integer(1)               :: split

   klen = len_trim(key)

   hash = djb2_init
   do i = 1,klen
      ki = transfer(key(i:i),split)
      hash = ieor(33*hash,ki)
   end do

end function
!==========================================================

!==========================================================
pure function sdbm_64_int(key) result(hash)
!==========================================================
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   integer, intent(in) :: key
   integer             :: hash, i, ki

   hash = 0
   do i = 1,8
      ki = ibits(key,8*(i-1),8)
      hash = hash * 65599 + ki
   end do

end function
!==========================================================

!==========================================================
pure function sdbm_64_str(key) result(hash)
!==========================================================
   !     (http://www.cse.yorku.ca/~oz/hash.html)
   character(*), intent(in) :: key
   integer                  :: hash, i, klen, ki
   integer(1)               :: split

   klen = len_trim(key)

   hash = 0
   do i = 1,klen
      ki = transfer(key(i:i),split)
      hash = hash * 65599 + ki
   end do

end function
!==========================================================

!==========================================================
pure function fnv1_64_int(key) result(hash)
!==========================================================
   ! Fowler/Noll/Vo
   !     (http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1)
   integer, intent(in)  :: key
   integer              :: hash, i, ki

   hash = fnv_offset
   do i = 1,8
      hash = hash * fnv_prime
      ki = ibits(key,8*(i-1),8)
      hash = ieor(hash,ki)
   end do

end function
!==========================================================

!==========================================================
pure function fnv1_64_str(key) result(hash)
!==========================================================
   ! Fowler/Noll/Vo
   !     (http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1)
   character(*), intent(in)  :: key
   integer                   :: hash, i, klen, ki
   integer(1)                :: split

   klen = len_trim(key)

   hash = fnv_offset
   do i = 1,klen
      hash = hash * fnv_prime
      ki = transfer(key(i:i),split)
      hash = ieor(hash,ki)
   end do

end function
!==========================================================

!==========================================================
pure function fnv1a_64_int(key) result(hash)
!==========================================================
   ! Fowler/Noll/Vo variation
   !     (http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1a)
   integer, intent(in) :: key
   integer             :: hash, i, ki, h, h1

   hash = fnv_offset
   do i = 1,8
      ki = ibits(key,8*(i-1),8)
      hash = ieor(hash,ki)
      hash = hash * fnv_prime
   end do

end function
!==========================================================

!==========================================================
pure function fnv1a_64_str(key) result(hash)
!==========================================================
   ! Fowler/Noll/Vo variation
   !     (http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1)
   character(*), intent(in)  :: key
   integer                   :: hash, i, klen, ki
   integer(1)                :: split

   klen = len_trim(key)

   hash = fnv_offset
   do i = 1,klen
      ki = transfer(key(i:i),split)
      hash = ieor(hash,ki)
      hash = hash * fnv_prime
   end do

end function
!==========================================================

!==========================================================
end module
!==========================================================
