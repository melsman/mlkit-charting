
(* Some generic utilities *)

fun die s = raise Fail ("main.sml: " ^ s)

open Js.Element infix &

fun connect nil = $""
  | connect [e] = e
  | connect (e :: es) = e & connect es

fun collect (sel:'a->string) (xs:'a list) : string list =
    Listsort.sort
        String.compare
        (foldl (fn (l, acc) =>
                   let val c = sel l
                   in if Listutil.mem c acc then acc
                      else c::acc
                   end) nil xs)

(* ------------------- *)
(* Some useful widgets *)
(* ------------------- *)


fun select (e:Js.elem) (options:(string*string)list) (onChange: string -> bool) : unit -> string =
    let val options' = map (fn (v,s) => taga "option" [("value",v)] ($s)) options
        val sel = taga "select" [("class", "custom-select custom-select-sm")]
                       (connect options')
        val () = Js.installEventHandler sel Js.onchange (fn () => onChange(Js.value sel))
    in Js.appendChild e sel
     ; fn () => Js.value sel
    end

(* A generic Bootstrap navBar that supports lazy evaluation of the underlying pages. *)

local
type 'a thunk = (unit -> 'a) ref
fun thunk (f: unit -> 'a) = ref f
fun eval (t: 'a thunk) =
    let val v = !t()
    in t := (fn () => v)
     ; v
    end
in
fun navbar (parent:Js.elem) (items: {pg_title:string, pg_gen: unit -> Js.elem} list) : unit =
    let val target_elem = taga0 "div" [("class","page-container")]
        val current : Js.elem option ref = ref NONE
        fun clearCurrent () =
            case !current of
                NONE => ()
              | SOME li => (Js.setAttribute li "class" "nav-item";
                            current := NONE)
        fun setCurrent li =
            (Js.setAttribute li "class" "nav-item active";
             current := SOME li)

        val lis =
            map (fn {pg_title=s,pg_gen=f} =>
                    let val title_elem = taga "a" [("class","nav-link"),("href","#")] ($s)
                        val li = taga "li" [("class","nav-item")] title_elem
                        val t = thunk f
                        fun onClickHandler () =
                            (clearCurrent();
                             setCurrent li;
                             JsUtil.removeChildren target_elem;
                             Js.appendChild target_elem (eval t);
                             true)
                        val () = Js.installEventHandler title_elem Js.onclick onClickHandler
                    in (li, onClickHandler)
                    end) items
        val () = case lis of (x,f)::_ => (f(); ()) | _ => ()
        val ul = taga "ul" [("class","navbar-nav")] (connect (map (#1) lis))
    in Js.appendChild parent
                      (taga "nav" [("class","navbar navbar-expand-sm navbar-light bg-light")]
                         (taga "a" [("class","navbar-brand"),("href","#")]
                               (taga0 "img" [("src","https://elsman.com/mlkit/images/mlkitsquare.png"),
                                            ("width","30"),
                                            ("height","30"),
                                            ("class","d-inline-block align-top"),
                                            ("alt","")] & ($" Benchmarking")) &
                          taga "button" [("class","navbar-toggler"),
                                         ("type","button"),
                                         ("data-toggle","collapse"),
                                         ("data-target","#navbarNav"),
                                         ("aria-controls","navbarNav"),
                                         ("aria-expanded","false"),
                                         ("aria-label","Toggle navigation")]
                              (taga0 "span" [("class","navbar-toggler-icon")]) &
                          taga "div" [("class","collapse navbar-collapse"),
                                      ("id","navbarNav")]
                              ul) &
                    tag "p" target_elem)
    end
end

(* A generic Highchart barChart widgets that supports reloading of series and which can be
 * controlled from a selection box. It also supports redrawing upon change of the underlying
 * data. *)

type dataspec = {kind:string,title:string,getnum:Data.line -> real}

fun genGraph (picker_elem, graph_parent_elem) (dataspecs:dataspec list) (ymeasure:string)
             (getData:unit -> Data.line list) (redraw: (unit->unit) -> unit) : unit =
    let fun chartEntitiesFromDataSpec (data:Data.line list) (kind:string) =
            let val (ytitle,getnum: Data.line -> real) =
                  case List.find (fn ds => #kind ds = kind) dataspecs of
                      SOME ds => (#title ds, #getnum ds)
                    | NONE => ("unknown", fn _ => 0.0)
                val comps = collect #cname data
                val categories = collect #pname data
            in {ytitle=ytitle,
                categories= categories,
                series= map (fn cn =>
                                let val data = List.filter (fn l => #cname l = cn) data
                                    val rs = map (fn p =>
                                                     case List.filter (fn l => #pname l = p) data of
                                                         [l] => getnum l
                                                       | _ => 0.0) categories
                                in (cn, rs)
                                end) comps}
            end
        val k0 = case dataspecs of ds::_ => #kind ds | _ => "unknown"
        val c = let val {ytitle,categories,series} = chartEntitiesFromDataSpec (getData()) k0
                in Highchart.barChart graph_parent_elem
                                      {title="",
                                       categories=categories,
                                       ytitle=ytitle,
                                       ymeasure=ymeasure,
                                       series=series}
                end
        fun reloadChart k =
            let val data = getData()
                val {ytitle,categories,series} = chartEntitiesFromDataSpec data k
            in Highchart.setyAxisTitle c ytitle
             ; Highchart.setxAxisCategories c categories
             ; Highchart.removeSeries c
             ; app (Highchart.addSeries c) series
             ; true
            end
        val select_items = map (fn ds => (#kind ds,#title ds)) dataspecs
        val getValue = if length dataspecs = 1 then fn () => k0
                       else select picker_elem select_items reloadChart
        val () = redraw (fn () => (reloadChart (getValue()); ()))
    in ()
    end

(* The page DOM *)

val body = case Js.getElementById Js.document "body" of
               SOME e => e
             | NONE => raise Fail "cannot find body element"

val list_elem = tag0 "div"

val raw_elem =
    tag "div" (tag "h2" ($"Links to Raw Data") & list_elem)


val report_file_elem = tag0 "div"
val date_elem = tag0 "div"
val mach_elem = tag0 "div"

val header_block_elem =
    taga "div" [("class","card w-100")]
         (taga "div" [("class","card-body")]
               (taga "h6" [("class","card-subtitle")]
                     ($"Report Snapshot") &
                tag "p" report_file_elem &
                taga "h6" [("class","card-subtitle")]
                     ($"Computation date") &
                tag "p" date_elem &
                taga "h6" [("class","card-subtitle")]
                     ($"Machine") &
                tag "p" mach_elem
               ))

val exectime_picker_elem = tag0 "div"
val exectime_graph_elem = tag0 "div"

val memusage_picker_elem = tag0 "div"
val memusage_graph_elem = tag0 "div"

val comptime_picker_elem = $""  (* dummy - not used *)
val comptime_graph_elem = tag0 "div"

val plen_picker_elem = $""  (* dummy - not used *)
val plen_graph_elem = tag0 "div"

val exec_block_elem =
    taga "div" [("class","card w-100")]
         (taga "div" [("class","card-body")]
               (tag "h2" ($"Execution Time") &
                tag "p" exectime_picker_elem &
                tag "p" exectime_graph_elem &
                tag "h2" ($"Memory Usage") &
                tag "p" memusage_picker_elem &
                tag "p" memusage_graph_elem &
                tag "h2" ($"Compilation Time") &
                tag "p" comptime_graph_elem &
                tag "h2" ($"Program Length") &
                tag "p" plen_graph_elem))

val about_elem =
    tag "div"
        (tag "h2" ($"About these Benchmarks") &
             tag "p" ($"The source code for the benchmarks are available from " &
                       taga "a" [("href","https://github.com/melsman/mlkit-bench")]
                       ($"https://github.com/melsman/mlkit-bench") & ($".")))

val snapshot_elem =
    tag "div"
        (tag "p" header_block_elem &
         tag "p" exec_block_elem)

val () = navbar body [{pg_title="Snapshot", pg_gen=fn () => snapshot_elem},
                      {pg_title="Historic", pg_gen=fn() => $("To Come")},
                      {pg_title="Raw Data", pg_gen=fn () => raw_elem},
                      {pg_title="About", pg_gen=fn () => about_elem}]

local
  val data : Data.line list ref = ref nil
  val datalisteners : (unit -> unit) list ref = ref nil
in
fun getData () : Data.line list = !data
fun setData (d:Data.line list) =
    (data := d; List.app (fn f => f()) (!datalisteners))
fun redraw f = datalisteners := f :: (!datalisteners)
end

fun getMachines (lines: Data.line list) : string list =
    let val d = List.filter (fn l => #cname l = "MLKIT") lines
    in collect #mach d
    end

fun getMachineVersion (lines: Data.line list) : string option =
    case getMachines lines of
        x :: _ => SOME x
      | _ => NONE

fun setMachine_elem mach =
    (JsUtil.removeChildren mach_elem;
     Js.appendChild mach_elem ($mach))

fun setDate_elem date =
    (JsUtil.removeChildren date_elem;
     Js.appendChild date_elem ($date))

fun filebaseOfUrl s =
    case rev(String.tokens (fn c => c = #"/") s) of
        file :: _ => OS.Path.base file  (* drop extension *)
      | _ => die ("fileOfUrl: failed to parse url " ^ s)

fun decomposeFilebase (filebase:string) =
    case String.tokens (fn c => c = #"_") filebase of
        ["report",v,mt,d] => SOME (v,mt,d)
      | _ => NONE


fun setReportFiles_elem (dataset:(string * Data.line list) list) : unit =
    let val dataset = map (fn (url,data) => (filebaseOfUrl url, data)) dataset
        val reports = map #1 dataset
        fun f r =
            case decomposeFilebase r of
                SOME (v,mt,d) =>
                let val data =
                        case Listutil.lookupOpt dataset r of
                            SOME data => data
                          | NONE => die "setReportFilesElem.no data"
                    val m = case getMachineVersion data of
                                SOME m => m
                              | NONE => "-"
                in setDate_elem d
                 ; setMachine_elem m
                 ; setData data
                 ; true
                end
              | NONE => die ("setReportFilesElem.expected file name format 'report_version_machine_YYYY-MM-DD' - got " ^ r)
        val e = tag0 "div"
        val _ = select e (map (fn v => (v,v)) reports) f
        val () = case reports of
                     r :: _ => (f r; ())
                   | _ => ()
    in Js.appendChild report_file_elem e
    end

fun avg nil = 0.0
  | avg xs = foldl (op +) 0.0 xs / (real (length xs))

val execTimeDataSpecs : dataspec list =
    [{kind="real", title="Real time (sec)", getnum=avg o (map #real) o #runs},
     {kind="user", title="User time (sec)", getnum=avg o (map #user) o #runs},
     {kind="sys",  title="Sys time (sec)",  getnum=avg o (map #sys) o #runs}]

val memUsageDataSpecs : dataspec list =
    [{kind="rss", title="Resident set size (kb)", getnum=avg o (map (real o #rss)) o #runs},
     {kind="binsz", title="Size of executable (kb)", getnum=real o #binsz}]

val compTimeDataSpecs : dataspec list =
    [{kind="ctime", title="Compilation time (sec)", getnum= #ctime}]

val plenDataSpecs : dataspec list =
    [{kind="plen",  title="Program length (lines)",  getnum=real o #plen}]

val () =
    Data.getReports
        (fn links =>
            let val hrefs = map (fn s => tag "li" (taga "a" [("href",s)] ($s))) links
                val ul = tag "ul" (connect hrefs)
                val () = Js.appendChild list_elem ul
            in Data.processLinks links (fn dataset =>
                                           ( setReportFiles_elem dataset
                                           ; genGraph (exectime_picker_elem, exectime_graph_elem)
                                                      execTimeDataSpecs "sec" getData redraw
                                           ; genGraph (memusage_picker_elem, memusage_graph_elem)
                                                      memUsageDataSpecs "kb" getData redraw
                                           ; genGraph (comptime_picker_elem, comptime_graph_elem)
                                                      compTimeDataSpecs "sec" getData redraw
                                           ; genGraph (plen_picker_elem, plen_graph_elem)
                                                      plenDataSpecs "lines" getData redraw))
            end)
