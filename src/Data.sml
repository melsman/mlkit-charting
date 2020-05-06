

structure Data =

(* similar to definitions in mlkit_bench/Bench.sml *)

type kb = int
type time = Time.time

type line = {
  cname:string,                 (* compiler name *)
  cversion:string,              (* compiler version *)
  date:Date.date,               (* date of comp/run *)
  mach:string,                  (* machine identifier (cpu, os, arch) *)
  pname:string,                 (* program name *)
  plen:int,                     (* program length (lines) *)
  ctime:time,                   (* compile time *)
  binsz:kb,                     (* size of binary executable *)
  runs:MemTime.report list,     (* the runs *)
  err:string                    (* err string ("": no errors) *)
}

local open Json
in
fun lookS obj x =
    case objLook obj x of
        SOME (STRING s) => s
      | SOME _ => die ("value associated with " ^ x ^ " in json is not a string")
      | NONE => die ("no value associated with " ^ x ^ " in json")

fun lookI obj x =
    case objLook obj x of
        SOME (NUMBER s) =>
        (case CharVector.
      | SOME _ => die ("value associated with " ^ x ^ " in json is not a string")
      | NONE => die ("no value associated with " ^ x ^ " in json")

fun toLine (OBJECT obj) : line =
    {cname=objLook obj "cname",

fun fromJsonString (s:string) : line list =
    Json.foldlArrayJson (fn (t,ls) => toLine t :: ls) nil s
end
