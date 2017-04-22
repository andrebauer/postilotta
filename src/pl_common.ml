open Lwt

let ok x = Lwt.return (Ok x)

let (>>*=) m f =
  m >>= function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x
(** Same as [Lwt_result.>>=] *)

