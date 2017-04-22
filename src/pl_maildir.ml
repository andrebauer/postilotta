open Lwt
open Astring
open Pl_common

module type MAILFS =
sig
  type t
    
  val fs : t
end

module Make
    (FS: Mirage_fs_lwt.S)
    (Mailfs: MAILFS with type t = FS.t) = struct

  module Mail = struct

    type mark = Delete | Keep | Removed
    
    type t = {
      path: string;
      hash: string option;
      mark: mark;
      size: int;
      id: int;
    }

    let fs = Mailfs.fs
    
    let load ~path ~id =
      FS.size fs path >>*= fun size' ->
      let size = Int64.to_int size' in
      return_ok {
        path;
        hash = None;
        mark = Keep;
        size;
        id;
      }
    
    let retrieve t =
      FS.read fs t.path 0 t.size

    let top t length =
      FS.read fs t.path 0 length 
      (* TODO: This is not a correct implementation of top,  *)

    let mark t m = { t with mark = m }

    let remove t =
      let open Lwt_result in
      FS.destroy fs t.path >|= fun () ->
      { t with mark = Removed }
  end
  

  module Directory = struct


  end
end
