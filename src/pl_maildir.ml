open Lwt
open Astring
open Pl_common

module type MAILFS =
sig
  type t
    
  val fs : t
end


module type MAILDIR = sig
  type mark = Delete | Keep | Removed
  type t = {
    path : string;
    hash : string option;
    mark : mark;
    size : int;
    id : int;
  }
  (* val fs : Mailfs.t *)

  (* type error = private [> parse_error | Mirage_fs.error ] *)
  
  val load :
    path:string ->
    id:int -> (t, Mirage_fs.error) Result.result Lwt.t

  (*
  val read :
    t ->
    (Mirage_fs.S.page_aligned_buffer list, Mirage_fs.error) Result.result Lwt.t
    *)  
  val mark :
    t -> mark -> t
    
  val remove :
    t -> (t, Mirage_fs.write_error) Lwt_result.t
end

module Make
    (FS: Mirage_fs_lwt.S with type error := Mirage_fs.error)
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

    type error = private [> parse_error | Mirage_fs.error ] 
    
    let read t =
      FS.read fs t.path 0 t.size
  
    
    let read_lines t n =
      (FS.read fs t.path 0 t.size : (Cstruct.t list, Mirage_fs.error) Lwt_result.t :> (Cstruct.t list, error) Lwt_result.t) >|= function
      | Error e -> Error e
      | Ok buffer_list ->
          let buffer = Cstruct.concat buffer_list in
          byte_stuff_cstruct buffer

    let mark t m = { t with mark = m }

    let remove t =
      let open Lwt_result in
      FS.destroy fs t.path >|= fun () ->
      { t with mark = Removed }
  end
  

  module Directory = struct


  end
end
