namespace GaneshTracer

open Colour

//See .fsi for comments on what the functions are intended to do
module Material = 
    type Material = M of Colour * float

    let color (M(c, _)) = c
    let reflectivity (M(_,r)) = r
    let create (color) (reflection) = M (color, reflection)
    let createFlat (color) = M(color, 0.0) 
    let createMirror = M(mkColour 1.0 1.0 1.0, 1.0)
    let black = M(mkColour 0.0 0.0 0.0, 0.0)  
    let white = M(mkColour 1.0 1.0 1.0, 0.0)  
    let grey  = M(mkColour 0.5 0.5 0.5, 0.0)  
    let red   = M(mkColour 1.0 0.1 0.1, 0.0)  
    let green = M(mkColour 0.1 1.0 0.1, 0.0)  
    let blue  = M(mkColour 0.1 0.1 1.0, 0.0)  