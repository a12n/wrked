type 'a t = 'a * 'a list

let of_list = function
    h :: t -> h, t
  | [] -> raise (Invalid_argument "Non_empty_list.of_list")

let to_list (h, t) = h :: t
