namespace GaneshTracer
module CSG =
    open Shape
    open Base
    open Material
    open System

    (* ############################
       ## HELPER METHODS FOR CSG ##
       ############################ *)
    
    //Function used to get the hitpoint from a hitoption.
    let getHit (hitOpt:(float*(unit -> Material.Material*Vector*Point)) option) = (() |> (snd (Option.get hitOpt))) |> trdT

    //Function used to switch out the hitpoint in a hitoption.
    let changeDist (hitOpt:(float*(unit -> Material.Material*Vector*Point)) option) (additionalDist:float) = 
        let old = Option.get hitOpt
        let oldUnit = snd old
        let newDist = (fst old) + additionalDist
        Some(newDist, oldUnit)
    
    //Variable used to move a hitpoint with the given epsilon, when refiring a ray.
    let rayOffset = 0.000006

    //Variable used to determine whether the first shape should be drawn, when two shapes are on top of each other.
    let nearnessFactor = 0.0000061

    (* ############################
       ##      CSG METHODS       ##
       ############################ *)

    //Function used to group two shapes together - also rendering internal edges.
    let group (s1:Shape) (s2:Shape) : Shape =
        let hitFunc (ray:Ray) =
            let hit1 = s1.updatedHitFunction ray
            let hit2 = s2.updatedHitFunction ray
            match (hit1,hit2) with
            | (Some(d1,_), Some(d2,_)) -> if d1 < d2 then hit1 else hit2
            | (Some(_),None) -> hit1
            | (None,Some(_)) -> hit2
            | (None,None)    -> None
        let inside (p : Point) = (s1.inside p) || (s2.inside p)
        let newBounds = 
            match s1.boundsOpt, s2.boundsOpt with
            | Some(fst), Some(snd) -> Some(fst + snd)
            | _                    -> None
        Shape(hitFunc, inside, newBounds, Math.Max(s1.precision,s2.precision))
    
    //Function used to find intersection between two shapes - only rendering the overlap of the two shapes.
    let intersection (s1:Shape) (s2:Shape) : Shape =
        let hitFunc (ray:Ray) : ((float*(unit -> Material.Material*Vector*Point)) option) =
            let rec acceleRaytor (newRay:Ray) (dist:float) =
                let isInS1 = s1.inside newRay.Origin
                let isInS2 = s2.inside newRay.Origin
                let hit1 = s1.updatedHitFunction newRay
                let hit2 = s2.updatedHitFunction newRay
                match isInS1, isInS2 with
                | true, true -> 
                    match hit1, hit2 with //Inside both shapes.
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then changeDist hit1 dist
                        else changeDist hit2 dist
                    | _ -> None
                | true, false -> 
                    match hit1, hit2 with //Inside s1.
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                        else changeDist hit2 dist
                    | _ -> None
                | false, true -> 
                    match hit1, hit2 with //Inside s2.
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then changeDist hit1 dist
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | _ -> None
                | false, false -> 
                    match hit1, hit2 with //Outside both shapes.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | _ -> None
            acceleRaytor ray 0.0
        let inside (p : Point) = (s1.inside p) && (s2.inside p)
        let newBounds = 
            match s1.boundsOpt, s2.boundsOpt with
            | Some(fst), Some(snd) ->
                if (fst.MaximumX * fst.MinimumX + fst.MaximumY * fst.MinimumY + fst.MaximumZ * fst.MinimumZ) < 
                   (snd.MaximumX * snd.MinimumX + snd.MaximumY * snd.MinimumY + snd.MaximumZ * snd.MinimumZ) then 
                       Some(fst)
                else 
                       Some(snd)
            | Some(fst), None      -> Some(fst)
            | None, Some(snd)      -> Some(snd)
            | None, None           -> None
        Shape(hitFunc,inside,newBounds, Math.Max(s1.precision,s2.precision))

    //Function used to union two shapes - without rendering internal edges.
    let union (s1:Shape) (s2:Shape) : Shape =
        let hitFunc (ray:Ray) =
            let rec acceleRaytor (newRay:Ray) (dist:float) =
                let isInS1 = s1.inside newRay.Origin
                let isInS2 = s2.inside newRay.Origin
                let hit1 = s1.updatedHitFunction newRay
                let hit2 = s2.updatedHitFunction newRay
                match isInS1, isInS2 with
                | true, true -> 
                    match hit1, hit2 with //Inside both shapes.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | _ -> None
                | true, false -> 
                    match hit1, hit2 with //Inside s1.
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then changeDist hit1 dist
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | Some(_), None -> changeDist hit1 dist
                    | _             -> None
                | false, true ->
                    match hit1, hit2 with //Inside s2.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                        else changeDist hit2 dist
                    | None, Some(_) -> changeDist hit2 dist
                    | _             -> None
                | false, false -> 
                    match hit1, hit2 with //Outside both shapes.
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 <= d2 then changeDist hit1 dist
                        else changeDist hit2 dist
                    | Some(_), None -> changeDist hit1 dist
                    | None, Some(_) -> changeDist hit2 dist
                    | None, None    -> None
            acceleRaytor ray 0.0
        let inside (p : Point) = (s1.inside p) || (s2.inside p)
        let newBounds = 
            match s1.boundsOpt, s2.boundsOpt with
            | Some(fst), Some(snd) -> Some(fst + snd)
            | _                    -> None
        Shape(hitFunc, inside, newBounds, Math.Max(s1.precision,s2.precision))

    //Function used to subtract one shape (s2) from another shape (s1).
    let subtraction (s1:Shape) (s2:Shape) : Shape = 
        let hitFunc (ray:Ray) : ((float*(unit -> Material.Material*Vector*Point)) option) =
            let rec acceleRaytor (newRay:Ray) (dist:float) =
                let isInS1 = s1.inside newRay.Origin
                let isInS2 = s2.inside newRay.Origin
                let hit1 = s1.updatedHitFunction newRay
                let hit2 = s2.updatedHitFunction newRay
                match isInS1, isInS2 with
                | true, true ->
                    match hit1, hit2 with //Inside both shapes
                    | Some(d1,_), Some(d2,_) -> 
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 > d2 then changeDist hit2 dist
                        else acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                    | _ -> None
                | true, false ->
                    match hit1, hit2 with //Inside s1.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 < d2 then changeDist hit1 dist
                        else changeDist hit2 dist
                    | Some(_), None -> changeDist hit1 dist
                    | _             -> None
                | false, true ->
                    match hit1, hit2 with //Inside s2.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 < d2 then acceleRaytor (Ray(((getHit hit1) + (rayOffset * newRay.Direction)), newRay.Direction)) (d1 + dist)
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | _ -> None
                | false, false ->
                    match hit1, hit2 with //Outside both shapes.
                    | Some(d1,_), Some(d2,_) ->
                        if abs(d1 - d2) <= nearnessFactor then changeDist hit1 dist
                        else
                        if d1 < d2 then changeDist hit1 dist
                        else acceleRaytor (Ray(((getHit hit2) + (rayOffset * newRay.Direction)), newRay.Direction)) (d2 + dist)
                    | Some(_), None -> changeDist hit1 dist
                    | _             -> None
            acceleRaytor ray 0.0
        let inside (p : Point) = (s1.inside p) && not (s2.inside p)
        let newBounds = if s1.boundsOpt.IsNone then None else Some(s1.bounds)
        Shape(hitFunc, inside, newBounds, Math.Max(s1.precision,s2.precision))