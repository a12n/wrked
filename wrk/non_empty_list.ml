type 'a t = 'a * 'a list

let of_list = function
  | x :: xs -> x, xs
  | []      -> invalid_arg "Non_empty_list.of_list"

let to_list (x, xs) = x :: xs
