namespace GaneshTracer

open Base
open Colour
open Material

(* Provides lighting functionality for the raytracer. 
   The light type is able to calculate the lightcast at a given point in the scene
   (Doesn't take into account that it might be blocked though.) *)
module Light = 

    /// Type holding the different types of light. Default : Pointlight
    type Light   
     
    /// Creates a point light
    val mkPointLight : Point -> Colour -> float -> Light

    /// Creates a point light with falloff (inverse square law)
    val mkFallOff : Point -> Colour -> float -> Light

    /// Creates a point light with logarithmic falloff
    val mkFallOffLog : Point -> Colour -> float -> Light

    /// Default light (currently point without falloff)
    val mkLight : Point -> Colour -> float -> Light

    /// Gets the position of the light
    val getPos : Light -> Point

    /// Returns the colour the light provides at the given hitpoint
    val lightInfluence : Ray -> Vector -> Light -> Colour

    /// Given a ray light and shading angle between light and norm of hitpoint, it returns the lightcast
    val lightCast : Ray -> Light -> float -> Colour
 