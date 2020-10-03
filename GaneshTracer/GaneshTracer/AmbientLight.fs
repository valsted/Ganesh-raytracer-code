namespace GaneshTracer

open Colour

module AmbientLight = 
    type AmbientLight =
         | AL of Colour * float

    let intensity (AL(_,i)) = i
    let color (AL(c,_)) = c
    let mkAmbientLight col i = AL(col, i)