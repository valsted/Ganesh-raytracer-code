namespace GaneshTracer

open Base
open Colour
open Material

// See .fsi file for documentation
module Light = 
    type Light =
        | PointLight of Point * Colour * float
        | FallOff of Point * Colour * float
        | FallOffL of Point * Colour * float

    let clamp i = 
        if i > 1.0 then 1.0
        else if i < 0.0 then 0.0
        else i

    let flatten (c: Colour, i: float) = (clamp i) * c
    
    let mkPointLight point colour intensity = PointLight(point, colour, intensity)
    let mkFallOff point colour intensity = FallOff(point, colour, intensity)
    let mkFallOffLog point colour intensity = FallOffL(point, colour, intensity)

    // Default light is Pointlight
    let mkLight p c i = mkPointLight p c i

    let getPos light = 
        match light with
        | PointLight(p,_,_) -> p
        | FallOff(p,_,_)    -> p
        | FallOffL(p,_,_)   -> p

    let lightInfluence (shadowRay: Ray) (norm: Vector) (light: Light) = 
        let shade = dotProduct norm shadowRay.Direction //shading factor inline
        if shade > 0.0 then
            match light with
            | PointLight(p,c,i) -> flatten (c, i * shade)
            | FallOff(p,c,i)    -> let tmp = (Magnitude ((getPos light) - shadowRay.Origin))
                                   flatten (c, (i * shade) / (tmp * tmp))
            | FallOffL(p,c,i)   -> flatten (c, shade * (log (Magnitude ((getPos light) - shadowRay.Origin)) - i))
        else mkColour 0.0 0.0 0.0

    let lightCast (shadowRay: Ray) (light: Light) (shadeFactor: float) = 
        match light with
            | PointLight(p,c,i) -> flatten (c, i * shadeFactor)
            | FallOff(p,c,i)    -> let tmp = (Magnitude ((getPos light) - shadowRay.Origin))
                                   flatten (c, (i * shadeFactor) / (tmp * tmp))
            | FallOffL(p,c,i)   -> flatten (c, shadeFactor * (log (Magnitude ((getPos light) - shadowRay.Origin)) - i))