namespace GaneshTracer

open System
open Base
open Scene
open Shape
open Material
open Light
open AmbientLight
open Colour

module Camera = 
    type TraceMode = 
        | Standard    //Standard rayfiring
        | Interpolate //Fires half the rays, guesses the rest
        | AA          //Fires all rays, averages result to reduce aliasing
        | Hybrid      //Fires half the rays, retraces the missing that are near edges and guesses the rest
        | HybridAA    //Same as hybrid mode, but post-processes AA on detected edges

    type Camera =
        | C of Ray[][]        

    let getRays (C(r)) = r
    let resolution (C(r)) = (r.Length, r.[0].Length)
        
    // New attempt at creating a viewport with perspective that works in all kinds of wierd rotations
    let viewPortPerspective (camPosition : Point) (lookAt : Point) (up : Vector) (resolutionWidth : int) (resolutionHeight : int) (pixelSize : float) (zoom : float) (upV : Vector)=
        let w = direction camPosition lookAt     
        let u = normCrossProduct upV w 
        let v = crossProduct w u

        let upModifier = v * pixelSize;
        let leftModifier = u * pixelSize;
  
        let center = camPosition + (w*zoom)
        let corner = center + (upModifier * (-1.0 + ((float resolutionHeight)) * 0.5)) + (leftModifier * (((float resolutionWidth) * 0.5)))
        
        // Calculate all points on the viewplane!
        let points = 
            [|
                for h in 0 .. resolutionHeight-1 ->
                    Array.Parallel.init resolutionWidth (fun wi -> 
                        (corner + (-wi * leftModifier) + (-h * upModifier))
                    )
            |]
        
        // Cute helper method
        let helper w h =
            direction camPosition points.[h].[w]

        Array.Parallel.init resolutionWidth (fun w -> 
                 Array.init resolutionHeight (fun h -> Ray(camPosition, helper w h)) )
    
    //Function that creates a camera
    let mkCamera (position : Point) (lookAt : Point) (upV : Vector) zoom width height resX resY = 
        //Calculates the width and height of each pixel
        let pixelWidth = width/(float)resX
        let pixelHeight = height/(float)resY
        
        let w = direction position lookAt     
        let u = normCrossProduct upV w 
        let v = crossProduct w u

        let viewPort = viewPortPerspective position lookAt v resX resY (System.Math.Max(pixelWidth, pixelHeight)) zoom upV
        C(viewPort)

    //Creates a secondary ray based on the given information
    let calcNextRay (hitPoint : Point) (direction : Vector) (normal : Vector) =
        let d' = normalize (direction-(2.0*(dotProduct normal direction))*normal)
        Ray(hitPoint, d')
    
    //Combines two colours based on reflection
    let combineColours (c1 : Colour) (c2 : Colour) (reflection : float) =
        let rf = ((getR c1) * (1.0-reflection)) + ((getR c2) * reflection)
        let gf = ((getG c1) * (1.0-reflection)) + ((getG c2) * reflection)
        let bf = ((getB c1) * (1.0-reflection)) + ((getB c2) * reflection)
        mkColour rf gf bf
    
    let traceRay (ray: Ray) (scene: Scene) (amColor: Colour) =   
        // Lighting engine
        let calcColor (hp: Point) (norm: Vector) (mat: Material) = 
            // Check for intersection in scene, beating distance to light
            let isBlocked (shadowLine: Vector) (hit: Point) (shadowRay: Ray) = 
                let blocked = scene.Hit (shadowRay) System.Double.MaxValue
                match blocked with
                | None          -> false
                | Some(f,_,_) -> if f > (Magnitude shadowLine) then false 
                                   else true
            
            // Iterate over all lights in scene and add influence (if unblocked)
            let mutable lighting = amColor
            for i in 0 .. scene.Lights.Length-1 do
                let shadowLine = (getPos scene.Lights.[i]) - hp
                let shadowRay = Ray(hp, normalize shadowLine)
                let shadeFactor = dotProduct norm shadowRay.Direction           // Angle between norm and shadowray
                if shadeFactor < 0.0 then ()                                    // Angle over 90 degrees, light is "below" surface of shape
                else if (isBlocked shadowLine hp shadowRay) then ()             // Don't update
                     else lighting <- (lighting + (lightCast shadowRay scene.Lights.[i] shadeFactor)) // Add lightcast
            (Material.color mat) * lighting                                     // Compute final colour
        
        // Intersection engine
        let rec fireRay (r: Ray) (depth: int) (limit: int) = 
            match scene.Hit r System.Double.MaxValue with
            | None -> mkColour 0.0 0.0 0.0
            | Some (dist, detailFunc, pres) -> 
              let (mat, norm, hp) = detailFunc ()
              let refl = reflectivity mat 
              let compNorm = if norm * r.Direction > 0.0 then normalize (-norm)
                             else normalize norm
              let compHp = (hp) + (pres * compNorm) 
              if refl > 0.0 && depth < limit then 
                let nextRay = calcNextRay compHp r.Direction compNorm
                let hitColor = calcColor compHp compNorm mat
                combineColours (hitColor) (fireRay nextRay (depth + 1) limit) refl
              else 
                calcColor compHp compNorm mat
        fireRay ray 0 scene.ReflectLimit  // Kick off raytracing routine


    /// Sample position
    type SamplePos = | Topleft    | Top     | Topright
                     | Left       | Center  | Right
                     | Bottomleft | Bottom  | Bottomright
    
    // Predefined patterns for 2D sampling
    let guessSamplePattern = [ Top; Left; Right; Bottom ]
    let aaSamplePattern = [ Top; Left; Center; Right; Bottom ]
    let allSamplePattern = [ Topleft; Top; Topright; Left; Center; Right; Bottomleft; Bottom; Bottomright ]
    let diagSamplePattern = [ Topleft; Topright; Bottomleft; Bottomright ]

    /// Attempts to retrieve color of pixel in the given samplePosition
    let sample (pixels: Drawing.Color[][]) (x: int) (y: int) (sample: SamplePos) =
        match sample with 
        | Topleft     -> if x <> 0 && y <> 0 then Some(pixels.[x-1].[y-1]) else None
        | Top         -> if y <> 0 then Some(pixels.[x].[y-1]) else None
        | Topright    -> if x <> (pixels.Length-1) && y <> 0 then Some(pixels.[x+1].[y-1]) else None
        | Left        -> if x <> 0 then  Some(pixels.[x-1].[y]) else None       
        | Center      -> Some(pixels.[x].[y]) 
        | Right       -> if x <> (pixels.Length-1) then  Some(pixels.[x+1].[y]) else None
        | Bottomleft  -> if x <> 0 && y <> (pixels.[0].Length-1) then Some(pixels.[x-1].[y+1]) else None
        | Bottom      -> if y <> (pixels.[0].Length-1) then Some(pixels.[x].[y+1]) else None   
        | Bottomright -> if x <> (pixels.Length-1) && y <> (pixels.[0].Length-1) then Some(pixels.[x+1].[y+1]) else None

    /// Picks a pattern of samples from given pixels
    let getSamples (pixels: Drawing.Color[][]) (x: int) (y: int) (samplePattern: SamplePos list) = 
        let addSample (acc: Drawing.Color list) (smp: SamplePos) = 
            match sample pixels x y smp with
            | None      -> acc
            | Some(col) -> col::acc
        List.fold(addSample) [] samplePattern

    /// Samples area around pixel to check if on an edge
    let nearEdge (pixels: Drawing.Color[][]) (x: int) (y: int) = 
        let hdif = colOptDiff (sample pixels x y Left) (sample pixels x y Right)  // Checks if horizontal difference is above threshold
        let vdif = colOptDiff (sample pixels x y Top) (sample pixels x y Bottom)  // Same as above, but vertical
        if hdif || vdif then true else false

    /// Averages a set of samples in the given pattern to a single color. (favorMid: put more weight on the center sample at x,y)
    let interpolate (pixels: Drawing.Color[][]) (x: int) (y: int) (samplePattern: SamplePos list) (favorMid: bool) = 
        let samples = if favorMid then Center::samplePattern else samplePattern 
        (getSamples pixels x y samples) |> avgColors

    /// Raytraces the scene
    let trace (scene: Scene) (cam: Camera) (mode: TraceMode) (gamma: float option) = 
        let (width, height) = resolution cam 
        let amColor = (intensity scene.Ambient) * (color scene.Ambient)         // Precompute ambientlight influence       
        let rays = getRays cam
        let convert = 
            match gamma with
            | None    -> toColor            // Lecturenotes defined color conversion
            | Some(g) -> encodeColor g      // Customizable gamma color conversion (g in range 0.2 - 0.8)

        let shouldTrace (x: int) (y: int) = (x % 2) = (y % 2) //If both x & y are even (or odd) they should be traced.

        // Array iteration helper functions for 2D arrays.
        let init2D (func: int -> int -> 'a) = Array.Parallel.init width  ( fun x -> 
                                                       Array.init height ( fun y -> func x y) )        
        let map2D (array2: 'a[][]) (func: 'a[][] -> int -> int -> 'a) = Array.Parallel.mapi ( fun x col -> 
                                                                                 Array.mapi ( fun y elem -> func array2 x y ) col) array2

        match mode with
        | Standard    -> init2D (fun x y -> convert (traceRay rays.[x].[y] scene amColor))

        | Interpolate -> init2D (fun x y -> if shouldTrace x y then convert (traceRay rays.[x].[y] scene amColor)  else Drawing.Color.Black)     // Trace every other ray
                         |> map2D <| (fun grid x y -> if shouldTrace x y then grid.[x].[y] else (interpolate grid x y guessSamplePattern false)) // Calculate avg to fill in rest
                         
        | AA          -> init2D (fun x y -> convert (traceRay rays.[x].[y] scene amColor) )         // Trace every ray
                         |> map2D <| (fun grid x y -> (interpolate grid x y aaSamplePattern true))  // Apply faux AA for all pixels

        | Hybrid      -> init2D (fun x y -> if shouldTrace x y then convert (traceRay rays.[x].[y] scene amColor)  else Drawing.Color.Black)  // Trace every other ray
                         |> map2D <| (fun grid x y -> if shouldTrace x y then grid.[x].[y]     // Already traced, just reuse
                                                      else if nearEdge grid x y then           // Detect edge
                                                                (convert (traceRay rays.[x].[y] scene amColor) ) // Trace properly
                                                           else (interpolate grid x y guessSamplePattern false)) // Guess by avg.

        | HybridAA    -> init2D (fun x y -> if shouldTrace x y then convert (traceRay rays.[x].[y] scene amColor)  else Drawing.Color.Black)
                         |> map2D <| (fun grid x y -> if shouldTrace x y then grid.[x].[y] 
                                                      else if nearEdge grid x y then 
                                                                (convert (traceRay rays.[x].[y] scene amColor) )
                                                           else (interpolate grid x y guessSamplePattern false))
                         |> map2D <| (fun grid x y -> if nearEdge grid x y then (interpolate grid x y diagSamplePattern true)
                                                      else grid.[x].[y])    // Apply faux AA for all pixels