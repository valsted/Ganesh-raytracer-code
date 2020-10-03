namespace GaneshTracer

open System.Drawing
open Base

module Colour =
    exception ColourException of string

    type Colour =
        struct
            val R: float
            val G: float
            val B: float
            new (r: float, g: float, b:float) = {R = r; G = g; B = b}
        end

    let mkColour r g b = if r < 0.0 || g < 0.0 || b < 0.0 then raise (ColourException "No negative values allowed.") else Colour(r,g,b)
    let getR (c: Colour) = c.R
    let getG (c: Colour) = c.G
    let getB (c: Colour) = c.B
    let scale (c: Colour) s = if s < 0.0 then raise (ColourException "No negative values allowed.") else Colour(c.R * s, c.G * s, c.B * s)
    let merge w (c1: Colour) (c2: Colour) = if w < 0.0 || 1.0 < w 
                                                     then raise (ColourException "Weight must be between 0 and 1.")
                                                     else let w' = 1.0 - w
                                                          mkColour(w*c1.R + w'*c2.R) (w*c1.G + w'*c2.G) (w*c1.B + w'*c2.B)

    let average (c1: Color) (c2: Color) = 
        Color.FromArgb(
            (int (c1.R + c2.R)) / 2, 
            (int (c1.G + c2.G)) / 2, 
            (int (c1.B + c2.B)) / 2 )

    let avgColors (colors: Color list) = 
        let div = colors.Length
        let sum (r1, g1, b1) (r2, g2, b2) = (r1+r2, g1+g2, b1+b2)
        let avgToColor (r, g, b) = Color.FromArgb(r/div, g/div, b/div)
        List.fold(fun acc (col:Color) -> sum acc ((int col.R), (int col.G), (int col.B))) (0, 0, 0) colors
        |> avgToColor

    let averageOpt (samples: Color option list) = 
        let mutable divisor = 0

        let sumFunc (acc: int*int*int) (colOpt: Color option) = 
            match colOpt with
            | None   -> acc
            | Some c -> divisor <- (divisor + 1)
                        ((fstT acc) + (int c.R), (sndT acc) + (int c.G), (trdT acc) + (int c.B))

        let summarized = List.fold(sumFunc) (0, 0, 0) samples
        Color.FromArgb(
            (fstT summarized)/ divisor, 
            (sndT summarized)/ divisor, 
            (trdT summarized)/ divisor )

    let difference (c1: Color) (c2: Color) = abs (int (c1.R - c2.R)) + abs (int (c1.G - c2.G)) + abs (int (c1.B - c2.B))  

    let colOptDiff (c1: Color option) (c2: Color option) = 
        if c1.IsNone || c2.IsNone then false
        else let (cA, cB) = (c1.Value , c2.Value)
             let diff = let rdif = (cA.R - cB.R)
                        let gdif = (cA.G - cB.G)
                        let bdif = (cA.B - cB.B)
                        sqrt (float ((rdif * rdif) + (gdif * gdif) + (bdif * bdif)))
             diff > 12.0  // Pragmatic test showed this to be a good threshold (range 8.0-13.0 seems reasonable)

    let toColor (c: Colour) = 
        Color.FromArgb(
            min (int (sqrt c.R*255.)) 255, 
            min (int (sqrt c.G*255.)) 255, 
            min (int (sqrt c.B*255.)) 255) 

    // Color conversion with gamma correction
    let encodeColor (gamma: float) (c: Colour) = 
        Color.FromArgb(
            min 255 (int ((c.R ** gamma) * 255.)),
            min 255 (int ((c.G ** gamma) * 255.)),
            min 255 (int ((c.B ** gamma) * 255.)) )

    let fromColor (c:System.Drawing.Color) = 
        let speed = 1.0/255.
        let nonpowr = float (c.R)*speed
        let r = (nonpowr * nonpowr)
        let nonpowg = float (c.G)*speed
        let g = (nonpowg * nonpowg)
        let nonpowb = float (c.B)*speed
        let b = (nonpowb * nonpowb)
        Colour(r,g,b)
    
    let mkLightSafe (c: Colour) =
        let r = c.R
        let g = c.G
        let b = c.B
        let r' = if r > 0.0001 then r else r + 0.0001
        let g' = if g > 0.0001 then g else g + 0.0001
        let b' = if r > 0.0001 then b else b + 0.0001
        Colour(r', g', b')

    type Colour with
      static member ( + ) (c1: Colour, c2: Colour) = Colour(c1.R + c2.R, c1.G + c2.G, c1.B + c2.B)
      static member ( - ) (c1: Colour, c2: Colour) = Colour(c1.R - c2.R, c1.G - c2.G, c1.B - c2.B)
      static member ( * ) (c1: Colour, c2: Colour) = Colour(c1.R * c2.R, c1.G * c2.G, c1.B * c2.B)
      static member ( * ) (s:float, c) = scale c s