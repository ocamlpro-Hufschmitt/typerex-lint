(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

(** [iter_files ~recdir apply dirname] iters on the given dirname and apply
    the function [apply] to all the found files. If [recdir] is set to false
    it only scans the files in the given directory and does not iter recursively
    in the subdirectories. *)
val iter_files : ?recdir:bool -> (string -> unit) -> string -> unit

(** [substitute str substs] subsitutes the string [str] with the given
     substitutes list [substs]. It replaces all the '$ID' by the
     matching string in the list. *)
val substitute : string -> (string * string) list -> string

(** [absolute_path root filename] give the absolute path of a file. *)
val absolute_path : string -> string -> string

(** [relative root filename] give the relative to root path of a file. *)
val relative_path : string -> string -> string

(** [find_root root_dir basenames] recurcively looks for the basenames in the
     path which marks the root of a project and contains the db file. *)
val find_root : File.t -> string list -> File.t

(** [is_in_path root file path] checks if the file is in the path. *)
val is_in_path : string -> string -> string -> bool

val normalize_path : string -> string -> string

val mk_temp_dir : unit -> string
