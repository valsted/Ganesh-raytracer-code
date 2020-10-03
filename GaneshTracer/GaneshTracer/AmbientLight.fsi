namespace GaneshTracer

open Colour

module AmbientLight =
    type AmbientLight

    val intensity : AmbientLight -> float
    val color : AmbientLight -> Colour
    val mkAmbientLight : colour : Colour -> intensity : float -> AmbientLight