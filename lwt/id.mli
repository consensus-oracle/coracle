open Common

val sockaddr_of_id: id -> Unix.sockaddr
val id_of_sockaddr: Unix.sockaddr -> id