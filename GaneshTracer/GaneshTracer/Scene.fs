namespace GaneshTracer

open Light
open Base
open Datastructure
open AmbientLight
open Shape

module Scene = 
    // Traverse the scene with a R tree
    let RFunc tree randyRay = tree |> Datastructure.hitShapes <| randyRay
    // Go linear through all shapes

    let KDFunc tree randyRay distance = Datastructure.sceneHitFunc tree randyRay distance

    let LinearHit (shapes: Shape[]) (ray: Ray) (distToBeat: float) =
        let distances = Array.map (fun (s: Shape) -> s.updatedHitFunction ray) shapes
        
        let mutable indexOfBest = -1
        let mutable bestDist = System.Double.MaxValue
        for i in 0 .. shapes.Length-1 do
            match distances.[i] with
            | None      -> ()
            | Some(d,_) -> if d < bestDist then 
                              bestDist <- d
                              indexOfBest <- i

        if indexOfBest <> -1 && bestDist < distToBeat then 
            let shape = shapes.[indexOfBest]
            let (dist, details) = Option.get (distances.[indexOfBest])
            Some(dist, details, shape.precision)
        else None
    
    [<Sealed>]
    type Scene (hitFunc: Ray -> float -> (float * (unit -> Material.Material * Vector * Point) * float) option, lights: Light[], ambient: AmbientLight, reflects: int) =
         member this.Lights = lights
         member this.Ambient = ambient
         member this.ReflectLimit = reflects
         member this.Hit (ray : Ray) (distToBeat: float) = hitFunc ray distToBeat

    let combinedHitFunction (withBB : KD) (withoutBB : Shape[]) (r : Ray) (distToBeat : float) =
        let kd = KDFunc withBB
        let linear = LinearHit withoutBB
        // Calculate nearest hit, starting with linear
        let r1 = linear r distToBeat
        if r1.IsSome then
            let (dist1, func1, p1) = r1.Value
            let r2 = kd r distToBeat
            if r2.IsSome then
                let (dist2, func2, p2) = r2.Value
                if dist1 < dist2 then
                    r1
                else r2 
            else r1
        else kd r distToBeat
      
    let mkScene (shapes : Shape list) lightL ambientL (maxReflections : int) = 
        let shapeArray = (Array.ofList shapes)
        if shapes.Length < 5 then
           let hitFunc = LinearHit shapeArray
           Scene(hitFunc, Array.ofList(lightL), ambientL, maxReflections)
        else 
           let (withBB, withoutBB) = Array.partition (fun (s : Shape) -> s.boundsOpt.IsSome) shapeArray
           let hitFunc = combinedHitFunction (withBB |> Datastructure.sceneTree) withoutBB
           Scene(hitFunc, Array.ofList(lightL), ambientL, maxReflections)
