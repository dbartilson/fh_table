# fh_table : Fortran hash functions and hash table

The main features of `fh_table` are:

- Open-addressed, linear-probed hash table.
- Two hash table 'types' are available, one with integer keys and one with variable-length string keys. Both use integer values paired to the key. 
- Preprocessing is not necessary.
- Implementation of a variety of hash functions in a separate module file, including [MurmurHash3](https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp), [MurmurHash2](https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp), [djb2/a](http://www.cse.yorku.ca/~oz/hash.html), [sdbm](http://www.cse.yorku.ca/~oz/hash.html), and [FNV-1/a](http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1). 
- Procedure pointers are used to allow the hash table to use different hash functions and key/value removal methods.

**License:** MIT

**Author:** Daniel T. Bartilson

## Usage

The hash functions are available in a module file `hash_functions`, which may be used as below. An example of usage and speed testing the various hash algorithms is done in `hash_function_test`, compilable with the `makefile`.

````Fortran
program hash_test
  use hash_functions

  write(*,*) mmh3_64('hello')
  write(*,*) mmh2_64(42)
  write(*,*) djb2_64('world')
  write(*,*) djb2a_64(21)
  write(*,*) sdbm_64('foo')
  write(*,*) fnv1_64(7)
  write(*,*) fnv1a_64('bar')
end program
````

The hash table derived types are available in a module file `hash_table_module`. An example showing all of the available options and methods for an integer-keyed table are shown below. String-keyed tables operate similarly. An example of usage and operability is shown in `hash_table_test`, compilable with the `makefile`.

````Fortran
program ht_test
  use hash_table_module ! uses hash_functions
  integer      :: value, flag
  type(ht_int) :: table_int

  call table_int%init(10,flag,alg='mmh3 ',load_factor=0.85)          ! initialize, size 10
  table_int%remove => trem_simple_int                                ! redefine deletion method
  call table_int%insert(42,1003,flag)                                ! insert key/val (42, 1003)
  call table_int%set(42,1,flag)                                      ! overwrite val
  call table_int%resize(20,flag)                                     ! resize table to 20
  call table_int%get(42,value,flag); write(*,*) value                ! get val
  call table_int%remove(42,flag)                                     ! remove key/val
  call table_int%destruct(flag)                                      ! destroy table
end program
````

## Notes 

- `fh_table` requires a Fortran 2008 compiler and has been tested with gfortran 8.1.0. `implicit none` and default 8-byte real/integers are assumed to be baked in during compilation. 
- Storing string keys is slower than storing integer keys because (1) the hash functions are faster for integers (see the `hash_function_test` exe) and (2) integer keys are stored in an aligned fashion and require no reallocations, while storing variable-length strings will typically result in misalignment and will default to reallocation on assignment.
- While hash tables and hash functions are available for integer and variable-length string key types, the included hash tables only use integers for the value of the key/value pair. This was done for simplicity. If you need to reference a string (or other type) value, I recommend storing that in a separate array and using the 'value' of the hash table to store the corresponding index.

## References and Related

There are a number of open-source Fortran hash table implementations available. I recommend you look at these if you are looking for other options.

- The [Fortran Wiki](http://fortranwiki.org/fortran/show/Hash+tables) contains a good starting point for finding other hash table implementations.
- [`fortran_hash_table`](https://github.com/pdebuyl/fortran_hash_table) by Pierre de Buyl implements a <string,string> option in a single file.
- [`ffhash`](https://github.com/jannisteunissen/ffhash) by Jannis Teunissen is a great option for a generic table (any data types) but uses preprocessing.

I looked at a number of sources for making this project:

- [rahulroy9202's legendary StackExchange answer](https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed) started me down this path and influenced the hash algorithms I included.
- The [wikibooks page for Hash Tables](https://en.wikibooks.org/wiki/Data_Structures/Hash_Tables) was surprisingly useful in getting me started.
- Jannis Teunissen's [`murmur3-fortran`](https://github.com/jannisteunissen/murmur3-fortran) and [`ffhash`](https://github.com/jannisteunissen/ffhash) were invaluable as references.
- Reference links are included in the comment blocks of each hash function in the `hash_functions` module file.
