namespace GaneshTracer
   
open Base
open Material
open Texture
open Shape
open PolynomialCalc
open Datastructure
open BaseShape
open System

module ShapeFactory =
    type Face = Right | Left | Top | Bottom | Front | Back

    let triangleTexCoords (alpha:float) (beta:float) (gamma:float) (a:float*float) (b:float*float) (c:float*float) =
        let u = alpha * (fst a) + beta * (fst b) + gamma * (fst c)
        let v = alpha * (snd a) + beta * (snd b) + gamma * (snd c)
        (u,v)

    let sphereTexCoords (v:Vector) =
        let texv = 1.0 - ((System.Math.Acos(v.Y)) / System.Math.PI)
        let phi = System.Math.Atan2 (v.X, v.Z)
        match phi with
        | f when f < 0.0 -> (((phi + 2.0 * System.Math.PI)/(2.0 * System.Math.PI)), texv)
        | _              -> ((phi / (2.0 * System.Math.PI)), texv)

    let boxTexCoords (p:Point) (a:Point) (h:float) (w:float) (face:Face) =
        match face with
        | Right ->
            let u = (p.Y - a.Y) / w
            let v = (p.Z - a.Z) / h
            (u, v)
        | Left ->
            let u = (p.Y - a.Y) / h
            let v = (p.Z - a.Z) / w
            (u, v)
        | Top ->
            let u = (p.X - a.X) / h
            let v = (p.Z - a.Z) / w
            (u, v)
        | Bottom ->
            let u = (p.X - a.X) / w
            let v = (p.Z - a.Z) / h
            (u, v)
        | Front ->
            let u = (p.X - a.X) / w
            let v = (p.Y - a.Y) / h
            (u, v)
        | Back ->
            let u = (p.X - a.X) / h
            let v = (p.Y - a.Y) / w
            (u, v)

    let hollowCylinderCoords (v:Vector) (p:Point) (h:float) =
        let texv = (p.Y / h) + 0.5
        let phi = System.Math.Atan2 (v.X, v.Z)
        match phi with
        | f when f < 0.0 -> (((phi + 2.0 * System.Math.PI) / (2.0 * System.Math.PI)), texv)
        | _              -> ((phi / (2.0 * System.Math.PI)), texv)
    
    let discTexCoords (p: Point) (r: float) =
        let speed2R = 1.0/(2.0*r)
        let u = (p.X + r) * speed2R
        let v = (p.Y + r) * speed2R
        (u, v)
    
    let triangleBounds ((o:Point), (p:Point), (q:Point)) = 
        let minP = Point(min (min o.X p.X) q.X, min (min o.Y p.Y) q.Y, min (min o.Z p.Z) q.Z)
        let maxP = Point(max (max o.X p.X) q.X, max (max o.Y p.Y) q.Y, max (max o.Z p.Z) q.Z)
        mkBoundingBox minP maxP
    
    let insideFake (p : Point) = false

    let mkCenteredSphere radius texture =
        let r2 = radius * radius
        let hitFunc (ray:Ray) =
            let p = ray.Origin
            let v = ray.Direction
            let a = (v.X*v.X)+(v.Y*v.Y)+(v.Z*v.Z)
            let b = (2.0*p.X*v.X)+(2.0*p.Y*v.Y)+(2.0*p.Z*v.Z)
            let c = (p.X*p.X)+(p.Y*p.Y)+(p.Z*p.Z) - r2
            let distOp = solveSecondDegree a b c
            if distOp.IsNone then None
            else let dist = Option.get distOp
                 let details () =
                     let hitpoint = p + (dist*v)
                     let norm = mkNormVector hitpoint.X hitpoint.Y hitpoint.Z
                     let mat = Texture.getMaterialAt texture (sphereTexCoords norm)
                     (mat, norm, hitpoint)
                 Some(dist, details)
        let inside (p : Point) = (p.X*p.X) + (p.Y*p.Y) + (p.Z*p.Z) - (radius*radius) <= 0.0
        Shape(hitFunc, inside, Some(mkBoundingBox (Point(-radius, -radius, -radius)) (Point(radius, radius, radius))))
    
    //Creates a sphere with radius 1.0 with center 0,0,0
    let mkSimpleSphere = mkCenteredSphere 1.0 Texture.mkGrayTexture

    let mkSphere (center:Point) (radius:float) texture = 
        let basic = mkCenteredSphere radius texture
        Transformation.transformShape [Transformation.Move(center.X, center.Y, center.Z)] basic
    
    type Hitface = IFront | IBack | IRight | ILeft | ITop | IBottom | OFront | OBack | ORight | OLeft | OTop | OBottom | NoHit

    let mkBox (L:Point) (H:Point) (front:Texture) (back:Texture) (top:Texture) (bottom:Texture) (left:Texture) (right:Texture) = 
        let distFunc (r:Ray) =
            let xIntersection = boxIntersection L.X H.X r.Origin.X r.Direction.X
            let yIntersection = boxIntersection L.Y H.Y r.Origin.Y r.Direction.Y
            let zIntersection = boxIntersection L.Z H.Z r.Origin.Z r.Direction.Z
            
            let t = max (fst xIntersection) (max (fst yIntersection) (fst zIntersection))
            let t' = min (snd xIntersection) (min (snd yIntersection) (snd zIntersection))

            if (t < t') && (t' > 0.0) then
                let mutable hitFace = NoHit

                if t > 0.0 then //From outside at distance t
                    let tx = fst xIntersection
                    let ty = fst yIntersection
                    let tz = fst zIntersection
                    match tx, ty, tz with
                    | tx, ty, tz when tx >= ty && tx >= tz        -> if r.Direction.X > 0.0 then hitFace <- OLeft else hitFace <- ORight
                    | tx, ty, tz when ty >= tx && ty >= tz        -> if r.Direction.Y > 0.0 then hitFace <- OBottom else hitFace <- OTop
                    | _                                           -> if r.Direction.Z > 0.0 then hitFace <- OBack else hitFace <- OFront
                else //From inside at distance t'
                    let tx' = snd xIntersection
                    let ty' = snd yIntersection
                    let tz' = snd zIntersection
                    match tx', ty', tz' with
                    | tx', ty', tz' when tx' <= ty' && tx' <= tz'  -> if r.Direction.X > 0.0 then hitFace <- IRight else hitFace <- ILeft
                    | tx', ty', tz' when ty' <= tx' && ty' <= tz'  -> if r.Direction.Y > 0.0 then hitFace <- ITop else hitFace <- IBottom
                    | _                                            -> if r.Direction.Z > 0.0 then hitFace <- IFront else hitFace <- IBack
                
                let hPoint = r.Origin + t * r.Direction
                let hPoint' = r.Origin + t' * r.Direction

                match hitFace with
                | OFront  -> let mat = getMaterialAt front (boxTexCoords hPoint (Point (L.X, L.Y, H.Z)) (H.Y - L.Y) (H.X - L.X) Front)
                             Some(t, (fun () -> (mat, Vector (0.0, 0.0, 1.0), hPoint)))
                | OBack   -> let mat = getMaterialAt back (boxTexCoords hPoint (Point (L.X, L.Y, L.Z)) (H.Y - L.Y) (H.X - L.X) Back)
                             Some(t, (fun () -> (mat, Vector (0.0, 0.0, -1.0), hPoint)))
                | ORight  -> let mat = getMaterialAt right (boxTexCoords hPoint (Point (H.X, L.Y, L.Z)) (H.Y - L.Y) (H.Z - L.Z) Right)
                             Some(t, (fun () -> (mat, Vector (1.0, 0.0, 0.0), hPoint)))
                | OLeft   -> let mat = getMaterialAt left (boxTexCoords hPoint (Point (L.X, L.Y, L.Z)) (H.Y - L.Y) (H.Z - L.Z) Left)
                             Some(t, (fun () -> (mat, Vector (-1.0, 0.0, 0.0), hPoint)))
                | OTop    -> let mat = getMaterialAt top (boxTexCoords hPoint (Point (L.X, H.Y, L.Z)) (H.X - L.X) (H.Z - L.Z) Top)
                             Some(t, (fun () -> (mat, Vector (0.0, 1.0, 0.0), hPoint)))
                | OBottom -> let mat = getMaterialAt bottom (boxTexCoords hPoint (Point (L.X, L.Y, L.Z)) (H.X - L.X) (H.Z - L.Z) Bottom)
                             Some(t, (fun () -> (mat, Vector (0.0, -1.0, 0.0), hPoint)))
                | IFront  -> let mat = getMaterialAt front (boxTexCoords hPoint' (Point (L.X, L.Y, L.Z)) (H.Y - L.Y) (H.X - L.X) Front)
                             Some(t', (fun () -> (mat, Vector (0.0, 0.0, -1.0), hPoint')))
                | IBack   -> let mat = getMaterialAt back (boxTexCoords hPoint' (Point (L.X, L.Y, H.Z)) (H.Y - L.Y) (H.X - L.X) Back)
                             Some(t', (fun () -> (mat, Vector (0.0, 0.0, 1.0), hPoint')))
                | IRight  -> let mat = getMaterialAt right (boxTexCoords hPoint' (Point (L.X, L.Y, L.Z)) (H.Y - L.Y) (H.Z - L.Z) Right)
                             Some(t', (fun () -> (mat, Vector (-1.0, 0.0, 0.0), hPoint')))
                | ILeft   -> let mat = getMaterialAt left (boxTexCoords hPoint' (Point (H.X, L.Y, L.Z)) (H.Y - L.Y) (H.Z - L.Z) Left)
                             Some(t', (fun () -> (mat, Vector (1.0, 0.0, 0.0), hPoint')))
                | ITop    -> let mat = getMaterialAt top (boxTexCoords hPoint' (Point (L.X, L.Y, L.Z)) (H.X - L.X) (H.Z - L.Z) Top)
                             Some(t', (fun () -> (mat, Vector (0.0, -1.0, 0.0), hPoint')))
                | IBottom -> let mat = getMaterialAt bottom (boxTexCoords hPoint' (Point (L.X, H.Y, L.Z)) (H.X - L.X) (H.Z - L.Z) Bottom)
                             Some(t', (fun () -> (mat, Vector (0.0, 1.0, 0.0), hPoint')))
                | _       -> None
            else None
        let inside (p : Point) = (p.X > L.X && p.X < H.X) && (p.Y > L.Y && p.Y < H.Y) && (p.Z > L.Z && p.Z < H.Z)
        Shape(distFunc, inside, Some(mkBoundingBox L H))
    
    let mkSimpleTextureBox (L:Point) (H:Point) (tex:Texture) =
        mkBox L H tex tex tex tex tex tex
    
    let mkTriangle (o: Point) (p: Point) (q: Point) (mat: Material) =
        let hitFunc (r: Ray) =
            let u = distance o p
            let v = distance o q
            let normal = normCrossProduct u v
            let a = (o.X - p.X)
            let b = (o.X - q.X)
            let c = (r.Direction.X)
            let d = (o.X - r.Origin.X)
            let e = (o.Y - p.Y)
            let f = (o.Y - q.Y)
            let g = (r.Direction.Y)
            let h = (o.Y - r.Origin.Y)
            let i = (o.Z - p.Z)
            let j = (o.Z - q.Z)
            let k = (r.Direction.Z)
            let l = (o.Z - r.Origin.Z)
            let D = a*(f*k - g*j) + b*(g*i - e*k) + c*(e*j - f*i)
            match D with
            | n when n = 0.0 -> None
            | n ->
                let speedN = 1.0/n
                let beta = (d*(f*k-g*j)+b*(g*l-h*k)+c*(h*j-f*l)) * speedN
                let gamma = (a*(h*k-g*l)+d*(g*i-e*k)+c*(e*l-h*i)) * speedN
                let t = (a*(f*l-h*j)+b*(h*i-e*l)+d*(e*j-f*i)) * speedN
                let theta = beta + gamma
                if not (t > 0.0 && 0.0 < beta && beta < 1.0 && 0.0 < gamma && gamma < 1.0 && 0.0 < theta && theta < 1.0) then None
                else
                let details () =
                        let hitPoint = o + beta*u + gamma*v
                        (mat, normal, hitPoint)
                Some(t,details)

        Shape(hitFunc, insideFake, Some(triangleBounds (o, p, q)))
           
    let smoothNormal (beta:float) (gamma:float) (na:Vector) (nb:Vector) (nc:Vector) =
        let alpha = 1.0 - beta - gamma
        let v = alpha*na + beta*nb + gamma*nc
        normalize v
        
    //Triangle for use in meshes
    let mkMeshTriangle (ov: Vertex) (pv: Vertex) (qv: Vertex) (smooth: bool) (tex: Texture) =
        //Extracting Vertex Points
        let o = ov.P
        let p = pv.P
        let q = qv.P
        
        let u = distance o p
        let v = distance o q
        let a = (o.X - p.X)
        let b = (o.X - q.X)
        let e = (o.Y - p.Y)
        let f = (o.Y - q.Y)
        let i = (o.Z - p.Z)
        let j = (o.Z - q.Z)
        let precalc = e * j - f * i

        //Calculating the hit function
        let hitFunc (r: Ray) =
            let c = (r.Direction.X)
            let d = (o.X - r.Origin.X)
            let g = (r.Direction.Y)
            let h = (o.Y - r.Origin.Y)
            let k = (r.Direction.Z)
            let l = (o.Z - r.Origin.Z)
            let D = a*(f * k - g * j) + b*(g * i - e * k) + c * precalc
            match D with
            | n when n = 0.0 -> None
            | n ->
                let speedN = 1.0 / n
                let beta = (d*(f*k-g*j)+b*(g*l-h*k)+c*(h*j-f*l)) * speedN
                let gamma = (a*(h*k-g*l)+d*(g*i-e*k)+c*(e*l-h*i)) * speedN
                let t = (a*(f*l-h*j)+b*(h*i-e*l)+d*(precalc)) * speedN
                let theta = beta + gamma
                if not (t > 0.0 && 0.0 < beta && beta < 1.0 && 0.0 < gamma && gamma < 1.0 && 0.0 < theta && theta < 1.0) then None
                else
                let normal = if smooth then //Creates a normal based on the vertex normals
                                smoothNormal beta gamma ov.Norm pv.Norm qv.Norm
                             else normCrossProduct v u //The normal normal
                let details () =
                    let hitPoint = o + beta*u + gamma*v
                    let alpha = 1.0 - theta
                    let mat = Texture.getMaterialAt tex (triangleTexCoords alpha beta gamma (ov.UV) (pv.UV) (qv.UV))
                    (mat, normal, hitPoint)
                Some(t,details)
        Shape(hitFunc, insideFake, Some (triangleBounds (o, p, q)))

    let groundDistFunc (ray:Ray) = 
        if System.Math.Abs(ray.Direction.Z) > 0.0
        then
            let t = -ray.Origin.Z / ray.Direction.Z 
            if t < 0.0
            then None
            else Some(t)
        else
            if System.Math.Abs(ray.Origin.Z) > 0.0
            then None
            else Some(0.0)

    let mkSimpleGround (mat:Material) = 
        let hitFunc (r:Ray) =
            let distOp = groundDistFunc r
            if distOp = None
            then None
            else let dist = Option.get distOp
                 let details () =
                    let hitpoint = r.Origin + r.Direction * dist
                    (mat, Vector(0.0, 0.0, 1.0), hitpoint)
                 Some(dist, details)
        Shape(hitFunc, insideFake, None)

    let mkTexturedGround (tex:Texture) = 
        let hitFunc (r:Ray) =
            let distOp = groundDistFunc r
            if distOp = None
            then None
            else let dist = Option.get distOp
                 let details () =
                    let hitpoint = r.Origin + r.Direction * dist
                    (getMaterialAt tex (hitpoint.X, hitpoint.Y), Vector(0.0, 0.0, 1.0), hitpoint)
                 Some(dist, details)
        Shape(hitFunc, insideFake, None)
    
    let mkSimpleRect (a : Point) (width : float) (height : float) (tex : Texture) = 
        let speedWidth = 1.0/width
        let speedHeight = 1.0/height
        let ax = a.X
        let ay = a.Y
        let az = a.Z

        let hitFunc (r: Ray) =
            let d = r.Direction
            let o = r.Origin

            if Math.Abs(d.Z) < 0.00001 then None //Ray is parallel to the plane of the rectangle
            else 
                let dist = (az - o.Z) / d.Z
                if dist < 0.0 then None
                else 
                    let px = o.X + dist*d.X
                    let py = o.Y + dist*d.Y
                    let hitP = Point(px,py,az)
                
                    if (ax <= px && px <= (ax + width)) && (ay <= py && py <= (ay + height)) 
                    then
                        let details () = 
                                let mat = getMaterialAt tex (((px - ax)*speedWidth), ((py - ay)*speedHeight))
                                (mat, Vector(0.0,0.0,1.0), hitP)
                        Some(dist, details)
                    else None
        let topRight = Point(width,height,az)
        Shape(hitFunc, insideFake, Some(mkBoundingBox a topRight))

    let mkRectangle  (a : Point) (c : Point) (b : Point) (tex : Texture)  = 
        let width  = Math.Abs((Magnitude (distance a b)))
        let height = Math.Abs((Magnitude (distance a c)))
        let simpleRect = mkSimpleRect (Point(0.0,0.0,0.0)) width height tex
        let u = direction a b
        let v = direction a c
        let w = crossProduct u v
        let O = Transformation.Othornormal(u,v,w)
        let T = Transformation.Move(a.X,a.Y,a.Z)
        Transformation.transformShape [O;T] simpleRect

    let mkCenteredDisc (r: float) (tex: Texture) = 
        let diskDistFunc (ray:Ray) = 
            let planarHit = groundDistFunc ray
            match planarHit with
            | None                 -> None
            | Some f when f <= 0.0 -> None
            | Some f               -> let hp = ray.Origin + (ray.Direction * f)        
                                      if (Magnitude (ToVector hp)) < r then Some (f,hp) //The hp is reused in the hitFunc below to save computations
                                      else None
        let hitFunc (ray:Ray) = 
            let distHpOp = diskDistFunc (ray: Ray)
            if distHpOp = None then None
            else let distHp = Option.get distHpOp
                 let detail () = 
                    let hp = snd distHp
                    let mat = getMaterialAt tex (discTexCoords hp r)
                    (mat, Vector(0.0, 0.0, 1.0), hp) 
                 Some((fst distHp), detail)
        Shape(hitFunc, insideFake, Some(mkBoundingBox (Point(-r, -r, 0.0)) (Point(r, r, 0.0))))

    let mkFlatDisc (center: Point) (radius: float) (tex: Texture) = 
        let disk = mkCenteredDisc radius tex
        Transformation.transformShape [Transformation.Rotate(1.57079633, Transformation.Axis.X); Transformation.Move(center.X, center.Y, center.Z)] disk

    let mkDisc (center: Point) (radius: float) (tex: Texture) =
        let disk = mkCenteredDisc radius tex
        Transformation.transformShape [Transformation.Move(center.X, center.Y, center.Z)] disk

    let mkOpenCylinder (center: Point) (radius: float) (height: float) (tex: Texture) =
        let hitFunc (ray: Ray) =
            let d = ray.Direction
            let o = ray.Origin
            let a = (d.X*d.X) + (d.Z*d.Z)
            let b = 2.0*((o.X*d.X)+(o.Z*d.Z))
            let c = ((o.X*o.X)+(o.Z*o.Z)-(radius*radius))
            let distOp = solveSecondDegreeBothValues a b c
            if Option.isNone distOp then None
            else 
            let distances = Option.get distOp
            let firstDist = Option.get (fst distances)
            let secondDist = Option.get (snd distances)
            let minDist = min firstDist secondDist
            let maxDist = max firstDist secondDist
            if not (minDist > 0.0 || maxDist > 0.0) then None
            else
            let minPoint = o + (minDist*d)
            let maxPoint = o + (maxDist*d) 

            let py1 = minPoint.Y
            let py2 = maxPoint.Y
            
            let mutable minOnCylinder = (-height*0.5) <= py1 && py1 <= (height*0.5)
            let mutable maxOnCylinder = (-height*0.5) <= py2 && py2 <= (height*0.5)
            if not (minOnCylinder || maxOnCylinder) then None
            else
            if minDist < 0.0 then minOnCylinder <- false
            if maxDist < 0.0 then maxOnCylinder <- false
            if not (minOnCylinder || maxOnCylinder) then None
            else
            let finalPointDist : (Point*float) = if minOnCylinder && maxOnCylinder then (minPoint, minDist)
                                                 else if minOnCylinder then (minPoint,minDist)
                                                 else if maxOnCylinder then (maxPoint,maxDist)    
                                                 else failwith "Check if one or more solutions are on the cylinder: failed. Should not fail."
            let dist = snd finalPointDist
            let details () =
                let point = fst finalPointDist
                let normal = let speedR = 1.0/radius
                             if point.Z > 0.0
                             then Vector((point.X*speedR),0.0,(point.Z*speedR))
                             else -(Vector((point.X*speedR),0.0,(point.Z*speedR)))
                let mat = Texture.getMaterialAt tex (hollowCylinderCoords normal point height)
                (mat, normal, point)
            Some(dist, details)
        let shape = Shape(hitFunc, insideFake, Some(mkBoundingBox (Point(-radius - height*0.5, -radius - height*0.5, -radius - height*0.5)) (Point(radius + height*0.5, radius + height*0.5, radius + height*0.5))))
        Transformation.transformShape [Transformation.Move(center.X, center.Y, center.Z)] shape

    let mkClosedCylinder (center: Point) (radius: float) (height: float) (tex: Texture) (top: Texture) (bottom: Texture) =
            let cylinder = mkOpenCylinder center radius height tex
            let topDisc = mkFlatDisc (center + Vector(0.0,(height*0.5),0.0)) radius top
            let bottomDisc = mkFlatDisc (center + Vector(0.0,(-height*0.5),0.0)) radius bottom
            let hitFunc (ray: Ray) =
                let cylOp = cylinder.updatedHitFunction ray
                let topOp = topDisc.updatedHitFunction ray
                let botOp = bottomDisc.updatedHitFunction ray
                if (Option.isNone cylOp && Option.isNone topOp && Option.isNone botOp) then None
                else
                let cyl = if Option.isNone cylOp then System.Double.MaxValue else fst (Option.get cylOp)
                let top = if Option.isNone topOp then System.Double.MaxValue else fst (Option.get topOp)
                let bot = if Option.isNone botOp then System.Double.MaxValue else fst (Option.get botOp)
                let minDist = min (min cyl top) bot
                match minDist with
                | n when n=cyl -> cylOp
                | n when n=top -> topOp
                | n when n=bot -> botOp
                | _ -> None
            let inside (p: Point) : bool = ((p.Y - center.Y) < (height*0.5) && (p.Y - center.Y) > (-height*0.5)) && (((p.X - center.X)*(p.X - center.X)) + ((p.Z- center.Z)*(p.Z - center.Z)) - (radius*radius)) <= 0.0 
            Shape(hitFunc, inside, Some(cylinder.bounds)) 