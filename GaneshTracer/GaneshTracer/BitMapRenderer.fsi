namespace GaneshTracer

open System.Drawing
open Colour

module BitMapRenderer =
    val renderToScreen: Color[][] -> unit
    val writeToFile: Color[][] -> string -> unit