(func $__malloc (param $size i32) (result i32)
  (local $ptr i32)
  (local.set $ptr (global.get $__next_free_address))
  (global.set $__next_free_address (i32.add (local.get $ptr) (local.get $size)))
  (local.get $ptr)
)

(func $__memcpy (param $dst i32) (param $src i32) (param $len i32)
  (local $i i32)
  (local.set $i (i32.const 0))
  (loop $loop
    (br_if 1 (i32.ge_u (local.get $i) (local.get $len)))
    (i32.store8
      (i32.add (local.get $dst) (local.get $i))
      (i32.load8_u (i32.add (local.get $src) (local.get $i)))
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br $loop)
  )
)

(func $__strpack (param $ptr i32) (param $len i32) (result i64)
  (i64.or
    (i64.extend_i32_u (local.get $ptr))
    (i64.shl (i64.extend_i32_u (local.get $len)) (i64.const 32))
  )
)

(func $__strcat (param $str1 i64) (param $str2 i64) (result i64)
  (local $ptr1 i32)
  (local $len1 i32)
  (local $ptr2 i32)
  (local $len2 i32)
  (local $new_len i32)
  (local $new_ptr i32)

  (local.set $ptr1 (i32.wrap_i64 (local.get $str1)))
  (local.set $len1 (i32.wrap_i64 (i64.shr_u (local.get $str1) (i64.const 32))))
  (local.set $ptr2 (i32.wrap_i64 (local.get $str2)))
  (local.set $len2 (i32.wrap_i64 (i64.shr_u (local.get $str2) (i64.const 32))))

  (local.set $new_len (i32.add (local.get $len1) (local.get $len2)))
  (local.set $new_ptr (call $__malloc (local.get $new_len)))

  (call $__memcpy (local.get $new_ptr) (local.get $ptr1) (local.get $len1))
  (call $__memcpy
    (i32.add (local.get $new_ptr) (local.get $len1))
    (local.get $ptr2)
    (local.get $len2)
  )

  (i64.or
    (i64.extend_i32_u (local.get $new_ptr))
    (i64.shl (i64.extend_i32_u (local.get $new_len)) (i64.const 32))
  )
)

(func $__strcmp (param $str1 i64) (param $str2 i64) (result i32)
  (local $ptr1 i32)
  (local $len1 i32)
  (local $ptr2 i32)
  (local $len2 i32)
  (local $i i32)
  (local $c1 i32)
  (local $c2 i32)

  (local.set $ptr1 (i32.wrap_i64 (local.get $str1)))
  (local.set $len1 (i32.wrap_i64 (i64.shr_u (local.get $str1) (i64.const 32))))
  (local.set $ptr2 (i32.wrap_i64 (local.get $str2)))
  (local.set $len2 (i32.wrap_i64 (i64.shr_u (local.get $str2) (i64.const 32))))

  (if (i32.ne (local.get $len1) (local.get $len2))
    (then (return (i32.const 0)))
  )

  (local.set $i (i32.const 0))
  (loop $loop
    (local.set $c1 (i32.load8_u (i32.add (local.get $ptr1) (local.get $i))))
    (local.set $c2 (i32.load8_u (i32.add (local.get $ptr2) (local.get $i))))
    (if (i32.ne (local.get $c1) (local.get $c2))
      (then (return (i32.const 0)))
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $loop (i32.lt_u (local.get $i) (local.get $len1)))
  )

  (i32.const 1)
)

(func $__i64_pow (param $base i64) (param $exp i64) (result i64)
  (local $result i64)
  (local $current i64)
  
  (if (i64.lt_s (local.get $exp) (i64.const 0))
    (then 
      (if (i64.eq (local.get $base) (i64.const 1))
        (then (return (i64.const 1)))
      )
      (if (i64.eq (local.get $base) (i64.const -1))
        (then 
          (return 
            (select 
              (i64.const 1) 
              (i64.const -1) 
              (i64.eqz (i64.and (local.get $exp) (i64.const 1)))
            )
          )
        )
      )
      (return (i64.const 0))
    )
  )
  
  (if (i64.eqz (local.get $exp))
    (then (return (i64.const 1)))
  )
  (if (i64.eq (local.get $exp) (i64.const 1))
    (then (return (local.get $base)))
  )
  
  (local.set $result (i64.const 1))
  (local.set $current (local.get $base))
  
  (loop $loop
    (if (i64.ne (local.get $exp) (i64.const 0))
      (then
        (if (i64.eqz (i64.and (local.get $exp) (i64.const 1)))
          (then)
          (else
            (local.set $result (i64.mul (local.get $result) (local.get $current)))
          )
        )
        (local.set $current (i64.mul (local.get $current) (local.get $current)))
        (local.set $exp (i64.shr_u (local.get $exp) (i64.const 1)))
        (br $loop)
      )
    )
  )
  
  (local.get $result)
)

(func $__i32_pow (param $base i32) (param $exp i32) (result i32)
 (local $result i64)

 (local.set $result (call $__i64_pow
   (i64.extend_i32_s (local.get $base))
   (i64.extend_i32_s (local.get $exp))
 ))

 (i32.wrap_i64 (local.get $result))
)

(func $__i64_mod (param $a i64) (param $b i64) (result i64)
  (local $quotient i64)
  (local $product i64)

  (local.set $quotient (i64.div_s (local.get $a) (local.get $b)))
  (local.set $product (i64.mul (local.get $quotient) (local.get $b)))
  (i64.sub (local.get $a) (local.get $product))
)

(func $__i32_mod (param $a i32) (param $b i32) (result i32)
  (local $result i64)
  (local.set $result (call $__i64_mod
    (i64.extend_i32_s (local.get $a))
    (i64.extend_i32_s (local.get $b))
  ))
  (i32.wrap_i64 (local.get $result))
)

(func $__i64_to_str (param $num i64) (param $base i32) (result i64)
  (local $str_ptr i32)
  (local $str_len i32)
  (local $tmp i64)
  (local $is_negative i32)
  (local $digits i32)
  (local $i i32)
  (local $digit i32)
  (local $zero i64)
  (local $counter i64)

  (local.set $zero (i64.const 0))
  (if (i64.eq (local.get $num) (local.get $zero))
    (then
      (local.set $str_ptr (call $__malloc (i32.const 1)))
      (i32.store8 (local.get $str_ptr) (i32.const 48))
      (return 
        (i64.or
          (i64.extend_i32_u (local.get $str_ptr))
          (i64.shl (i64.extend_i32_u (i32.const 1)) (i64.const 32))
        )
      )
    )
  )

  (local.set $is_negative (i32.and 
    (i64.lt_s (local.get $num) (i64.const 0))
    (i32.eq (local.get $base) (i32.const 10))
  ))
  (local.set $tmp 
    (if (result i64) (local.get $is_negative)
      (then (i64.sub (i64.const 0) (local.get $num)))
      (else (local.get $num))
    )
  )

  (local.set $digits (i32.const 0))
  (local.set $counter (local.get $tmp))
  (loop $count_loop
    (local.set $digits (i32.add (local.get $digits) (i32.const 1)))
    (local.set $counter
      (if (result i64) (i32.eq (local.get $base) (i32.const 10))
        (then (i64.div_u (local.get $counter) (i64.const 10)))
        (else (i64.shr_u (local.get $counter) (i64.const 4)))
      )
    )
    (br_if $count_loop (i64.ne (local.get $counter) (i64.const 0)))
  )

  (local.set $str_len
    (i32.add (local.get $digits)
      (if (result i32) (local.get $is_negative) (then (i32.const 1)) (else (i32.const 0)))
    )
  )

  (local.set $str_ptr (call $__malloc (local.get $str_len)))

  (if (local.get $is_negative)
    (then (i32.store8 (local.get $str_ptr) (i32.const 45)))
  )

  (local.set $i (i32.sub (local.get $str_len) (i32.const 1)))
  (loop $fill_loop
    (local.set $digit
      (if (result i32) (i32.eq (local.get $base) (i32.const 10))
        (then (i32.wrap_i64 (i64.rem_u (local.get $tmp) (i64.const 10))))
        (else (i32.wrap_i64 (i64.and (local.get $tmp) (i64.const 15))))
      )
    )
    (i32.store8
      (i32.add (local.get $str_ptr) (local.get $i))
      (if (result i32) (i32.eq (local.get $base) (i32.const 10))
        (then (i32.add (local.get $digit) (i32.const 48)))
        (else 
          (select
            (i32.add (local.get $digit) (i32.const 48))
            (i32.add (local.get $digit) (i32.const 87))
            (i32.lt_u (local.get $digit) (i32.const 10))
          )
        )
      )
    )
    (local.set $tmp
      (if (result i64) (i32.eq (local.get $base) (i32.const 10))
        (then (i64.div_u (local.get $tmp) (i64.const 10)))
        (else (i64.shr_u (local.get $tmp) (i64.const 4)))
      )
    )
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (br_if $fill_loop
      (i32.gt_s  (local.get $i)
        (if (result i32) (local.get $is_negative) (then (i32.const 0)) (else (i32.const -1)))
      )
    )
  )

  (i64.or
    (i64.extend_i32_u (local.get $str_ptr))
    (i64.shl (i64.extend_i32_u (local.get $str_len)) (i64.const 32))
  )
)

(func $__i32_to_str (param $num i32) (param $base i32) (result i64)
  (call $__i64_to_str
    (i64.extend_i32_s (local.get $num))
    (local.get $base)
  )
)

(func $__pad (param $str i64) (param $padder i32) (param $padding i32) (param $right i32) (result i64)
  (local $ptr i32)
  (local $len i32)
  (local $new_ptr i32)
  (local $new_len i32)
  (local $str_start i32)
  (local $pad_start i32)
  (local $i i32)

  (local.set $ptr (i32.wrap_i64 (local.get $str)))
  (local.set $len (i32.wrap_i64 (i64.shr_u (local.get $str) (i64.const 32))))

  (local.set $new_len (i32.add (local.get $len) (local.get $padding)))
  (local.set $new_ptr (call $__malloc (local.get $new_len)))

  (if (i32.eq (local.get $right) (i32.const 1))
    (then
      (local.set $str_start (local.get $new_ptr))
      (local.set $pad_start (i32.add (local.get $new_ptr) (local.get $len)))
    )
    (else
      (local.set $pad_start (local.get $new_ptr))
      (local.set $str_start (i32.add (local.get $new_ptr) (local.get $padding)))
    )
  )

  (call $__memcpy (local.get $str_start) (local.get $ptr) (local.get $len))

  (local.set $i (i32.const 0))
  (loop $loop_pad
    (i32.store8
      (i32.add (local.get $pad_start) (local.get $i))
      (local.get $padder)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $loop_pad (i32.lt_u (local.get $i) (local.get $padding)))
  )

  (i64.or
    (i64.extend_i32_u (local.get $new_ptr))
    (i64.shl (i64.extend_i32_u (local.get $new_len)) (i64.const 32))
  )
)

(func $__to_upper (param $str i64) (result i64)
  (local $ptr i32)
  (local $len i32)
  (local $i i32)
  (local $c i32)

  (local.set $ptr (i32.wrap_i64 (local.get $str)))
  (local.set $len (i32.wrap_i64 (i64.shr_u (local.get $str) (i64.const 32))))

  (local.set $i (i32.const 0))
  (loop $loop
    (local.set $c (i32.load8_u (i32.add (local.get $ptr) (local.get $i))))
    (if (i32.and
          (i32.ge_u (local.get $c) (i32.const 97))
          (i32.le_u (local.get $c) (i32.const 122))
        )
      (then (local.set $c (i32.sub (local.get $c) (i32.const 32))))
    )
    (i32.store8 (i32.add (local.get $ptr) (local.get $i)) (local.get $c))
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $loop (i32.lt_u (local.get $i) (local.get $len)))
  )

  (local.get $str)
)
