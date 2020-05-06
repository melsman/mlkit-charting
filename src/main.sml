
val body = case Js.getElementById Js.document "body" of
               SOME e => e
             | NONE => raise Fail "cannot find body element"

open Js.Element
val () =
    Js.appendChild body (tag "h2" ($"Hi there"))
