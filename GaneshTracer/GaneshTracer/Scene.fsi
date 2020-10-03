namespace GaneshTracer

open Light
open AmbientLight
open Shape
open Base

module Scene = 
    
    [<Sealed>]
    [<Class>]
    type Scene =
            member Lights : Light[]
            member Ambient : AmbientLight
            member ReflectLimit : int
            member Hit : Ray -> float -> (float * (unit -> Material.Material * Vector * Point) * float) option

    val mkScene : Shape list -> Light list -> AmbientLight -> int -> Scene