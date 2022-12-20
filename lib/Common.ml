open Base

let array_of x n = Array.init ~f:(Fn.const x) n
