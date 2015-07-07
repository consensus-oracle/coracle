open Common

let localhost = Unix.inet_addr_of_string "127.0.0.1"

let sockaddr_of_id n = Unix.ADDR_INET (localhost, 5000+ int_of_id n)
let id_of_sockaddr = function
	|  Unix.ADDR_INET (_, pt) -> id_of_int (pt-5000)
	|  Unix.ADDR_UNIX name -> raise (Not_implemented "unix sockets not yet handled" ) 