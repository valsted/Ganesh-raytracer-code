namespace GaneshTracer

open Shape
open Base

module Transformation =
 (* Used to specify the axis of a rotation and/or mirroring *)
 type Axis = X | Y | Z

 (* Used to concatenate 'transformations' from outside the module *)
 type AffineTransformation =
 | Move of float * float * float
 | Scale of float * float * float
 | Shear of Axis * Axis * float
 | Rotate of float * Axis
 | Mirror of Axis
 | Othornormal of Vector * Vector * Vector
 | List of AffineTransformation list

 (* Used for matrix calculations *)
 type Matrix = 
    struct
        val m11: float
        val m12: float
        val m13: float
        val m14: float
        val m21: float
        val m22: float
        val m23: float
        val m24: float
        val m31: float
        val m32: float
        val m33: float
        val m34: float
        val m41: float
        val m42: float
        val m43: float
        val m44: float
        new (M11: float, M12: float, M13: float, M14: float, M21: float, M22: float, M23: float, M24: float, M31: float, M32: float, M33: float, M34: float, M41: float, M42: float, M43: float, M44: float)
            = {m11 = M11; m12 = M12; m13 = M13; m14 = M14; m21 = M21; m22 = M22; m23 = M23; m24 = M24; m31 = M31; m32 = M32; m33 = M33; m34 = M34;  m41 = M41; m42 = M42;  m43 = M43; m44 = M44}
    end

(* Function that is able to multiply two Matrices together *)
 let multiply (m1: Matrix) (m2: Matrix) = 
    Matrix((m1.m11*m2.m11 + m1.m12*m2.m21 + m1.m13*m2.m31 + m1.m14*m2.m41),(m1.m11*m2.m12 + m1.m12*m2.m22 + m1.m13*m2.m32 + m1.m14*m2.m42),(m1.m11*m2.m13 + m1.m12*m2.m23 + m1.m13*m2.m33 + m1.m14*m2.m43),(m1.m11*m2.m14 + m1.m12*m2.m24 + m1.m13*m2.m34 + m1.m14*m2.m44),
           (m1.m21*m2.m11 + m1.m22*m2.m21 + m1.m23*m2.m31 + m1.m24*m2.m41),(m1.m21*m2.m12 + m1.m22*m2.m22 + m1.m23*m2.m32 + m1.m24*m2.m42),(m1.m21*m2.m13 + m1.m22*m2.m23 + m1.m23*m2.m33 + m1.m24*m2.m43),(m1.m21*m2.m14 + m1.m22*m2.m24 + m1.m23*m2.m34 + m1.m24*m2.m44),
           (m1.m31*m2.m11 + m1.m32*m2.m21 + m1.m33*m2.m31 + m1.m34*m2.m41),(m1.m31*m2.m12 + m1.m32*m2.m22 + m1.m33*m2.m32 + m1.m34*m2.m42),(m1.m31*m2.m13 + m1.m32*m2.m23 + m1.m33*m2.m33 + m1.m34*m2.m43),(m1.m31*m2.m14 + m1.m32*m2.m24 + m1.m33*m2.m34 + m1.m34*m2.m44),
           (m1.m41*m2.m11 + m1.m42*m2.m21 + m1.m43*m2.m31 + m1.m44*m2.m41),(m1.m41*m2.m12 + m1.m42*m2.m22 + m1.m34*m2.m32 + m1.m44*m2.m42),(m1.m41*m2.m13 + m1.m42*m2.m23 + m1.m34*m2.m33 + m1.m44*m2.m43),(m1.m41*m2.m14 + m1.m42*m2.m24 + m1.m43*m2.m34 + m1.m44*m2.m44))

 (* Function that is able to multiply together a transformation Matrix and a Point - returns a transformed Point *)
 let multiplyToPoint (m: Matrix) (p: Point) = 
    Point((m.m11*p.X + m.m12*p.Y + m.m13*p.Z + m.m14),(m.m21*p.X + m.m22*p.Y + m.m23*p.Z + m.m24),(m.m31*p.X + m.m32*p.Y + m.m33*p.Z + m.m34))

 (* Function that is able to multiply together a transformation Matrix and a Vector - returns a transformed Vector *)
 let multiplyToVector (m: Matrix) (v: Vector) = 
    Vector((m.m11*v.X + m.m12*v.Y + m.m13*v.Z),(m.m21*v.X + m.m22*v.Y + m.m23*v.Z),(m.m31*v.X + m.m32*v.Y + m.m33*v.Z))

 (* The equivalent of 1 when multiplying Matrices *)
 let identityMatrix = 
    Matrix(1.0,0.0,0.0,0.0,
           0.0,1.0,0.0,0.0,
           0.0,0.0,1.0,0.0,
           0.0,0.0,0.0,1.0)
 
 (* Matrix for used for moving *)
 let translationMatrix x y z =
    Matrix(1.0,0.0,0.0, x ,
           0.0,1.0,0.0, y ,
           0.0,0.0,1.0, z ,
           0.0,0.0,0.0,1.0)
 
 (* The inverse of the matrix used for moving, frequently applied to Rays *)
 let inverseTranslationMatrix x y z =
    Matrix(1.0,0.0,0.0, -x,
           0.0,1.0,0.0, -y,
           0.0,0.0,1.0, -z,
           0.0,0.0,0.0,1.0)
 
 (* Matrix for used for scaling (and mirroring + inverse mirroring)*) 
 let scalingMatrix x y z =
    Matrix( x ,0.0,0.0,0.0,
           0.0, y ,0.0,0.0,
           0.0,0.0, z ,0.0,
           0.0,0.0,0.0,1.0)

 (* The inverse of the matrix used for scaling (but not mirroring!!), frequently applied to Rays *)
 let inverseScalingMatrix x y z =
    Matrix(1.0/x, 0.0,0.0,0.0,
           0.0,1.0/y, 0.0,0.0,
           0.0,0.0,1.0/z, 0.0,
           0.0, 0.0, 0.0, 1.0)

 (* Function used to retrieve the matrix for mirroring along the specified axis *)
 let mirroring (A : Axis) =
   match A with
   | X -> scalingMatrix -1.0 1.0 1.0
   | Y -> scalingMatrix 1.0 -1.0 1.0
   | Z -> scalingMatrix 1.0 1.0 -1.0

 (* Function used for rotating around a given axis *) 
 let rotationMatrix r A =
    let c = System.Math.Cos r
    let s = System.Math.Sin r
    match A with
    | X -> Matrix(1.0,0.0,0.0,0.0,
                  0.0, c ,-s ,0.0,
                  0.0, s , c ,0.0,
                  0.0,0.0,0.0,1.0)

    | Y -> Matrix( c ,0.0, s ,0.0,
                  0.0,1.0,0.0,0.0,
                  -s ,0.0, c ,0.0,
                  0.0,0.0,0.0,1.0)

    | Z -> Matrix( c ,-s, 0.0,0.0,
                   s , c, 0.0,0.0,
                  0.0,0.0,1.0,0.0,
                  0.0,0.0,0.0,1.0)

 (* Function used for rotating around the inverse of a given axis *) 
 let inverseRotationMatrix r A = 
    let c = System.Math.Cos r
    let s = System.Math.Sin r
    match A with
    | X -> Matrix(1.0,0.0,0.0,0.0,
                  0.0, c , s ,0.0,
                  0.0,-s , c ,0.0,
                  0.0,0.0,0.0,1.0)

    | Y -> Matrix( c ,0.0,-s ,0.0,
                  0.0,1.0,0.0,0.0,
                   s ,0.0, c ,0.0,
                  0.0,0.0,0.0,1.0)

    | Z -> Matrix( c , s, 0.0,0.0,
                  -s , c, 0.0,0.0,
                  0.0,0.0,1.0,0.0,
                  0.0,0.0,0.0,1.0)

 (* Function used for finding the matrix that is to be used for shearing *) 
 let shearingMatrix Hyx Hzx Hxy Hzy Hxz Hyz =
    Matrix(1.0,Hyx,Hzx,0.0,
           Hxy,1.0,Hzy,0.0,
           Hxz,Hyz,1.0,0.0,
           0.0,0.0,0.0,1.0)
 
 (* Function used when inverting a sheare *) 
 let inverseShearingMatrix Hyx Hzx Hxy Hzy Hxz Hyz =
    let D = 1.0 - Hxy*Hyx - Hxz*Hzx - Hyz*Hzy + Hxy*Hyz*Hzx + Hxz*Hyx*Hzy
    Matrix(1.0-Hyx*Hzy, -Hyx+Hyz*Hzx, -Hzx+Hyx*Hzy, 0.0,
           -Hxy+Hxz*Hzy, 1.0-Hxz*Hzx, -Hzy+Hxy*Hzx, 0.0,
           -Hxz+Hxy*Hyz, -Hyz+Hxz*Hyx, 1.0-Hxy*Hyx, 0.0,
                0.0    ,     0.0     ,     0.0    ,  D )

 (* Function used to retrieve an appropriate shearing matrix *)
 let getShearingMatrix (axis: Axis*Axis) (distance: float) (inverse: bool) =
   match axis with
   | (Y,X) -> if inverse 
              then inverseShearingMatrix distance 0.0 0.0 0.0 0.0 0.0
              else shearingMatrix distance 0.0 0.0 0.0 0.0 0.0

   | (Z,X) -> if inverse 
              then inverseShearingMatrix 0.0 distance 0.0 0.0 0.0 0.0
              else shearingMatrix 0.0 distance 0.0 0.0 0.0 0.0
   
   | (X,Y) -> if inverse 
              then inverseShearingMatrix 0.0 0.0 distance 0.0 0.0 0.0
              else shearingMatrix 0.0 0.0 distance 0.0 0.0 0.0
   
   | (Z,Y) -> if inverse 
              then inverseShearingMatrix 0.0 0.0 0.0 distance 0.0 0.0
              else shearingMatrix 0.0 0.0 0.0 distance 0.0 0.0
   
   | (X,Z) -> if inverse 
              then inverseShearingMatrix 0.0 0.0 0.0 0.0 distance 0.0
              else shearingMatrix 0.0 0.0 0.0 0.0 distance 0.0
   
   | (Y,Z) -> if inverse 
              then inverseShearingMatrix 0.0 0.0 0.0 0.0 0.0 distance
              else shearingMatrix 0.0 0.0 0.0 0.0 0.0 distance
   | (X,X) ->  failwith "It is not possible to sheare along the same axis."
   | (Y,Y) ->  failwith "It is not possible to sheare along the same axis."
   | (Z,Z) ->  failwith "It is not possible to sheare along the same axis."

 (* Function used to create a matrix that can be used in othornormal calculations*) 
 let orthornormalMatrix (u : Vector) (v : Vector) (w : Vector) =
    Matrix(u.X, v.X, w.X, 0.0,
           u.Y, v.Y, w.Y, 0.0,
           u.Z, v.Z, w.Z, 0.0,
           0.0, 0.0, 0.0, 1.0)

 (* Function used to create an inverse matrix that can be used in othornormal calculations*)
 let inverseOrthornormalMatrix (u : Vector) (v : Vector) (w : Vector) =
    Matrix(u.X, u.Y, u.Z, 0.0,
           v.X, v.Y, v.Z, 0.0,
           w.X, w.Y, w.Z, 0.0,
           0.0, 0.0, 0.0, 1.0)

 (* Function used to transpose a 4x4 matrix *)
 let transpose (m : Matrix) =
    Matrix(m.m11,m.m21,m.m31,m.m41,
           m.m12,m.m22,m.m32,m.m42,
           m.m13,m.m23,m.m33,m.m43,
           m.m14,m.m24,m.m43,m.m44)

 (* Translates an AffineTransform to its matrix counterpart *)
 let translateTransformation (at : AffineTransformation) = 
     match at with
     | Move(x, y, z)                           -> translationMatrix x y z
     | Scale(x, y, z)                          -> scalingMatrix x y z
     | Shear(a1, a2, distance)                 -> getShearingMatrix (a1, a2) distance false
     | Rotate(r, A)                            -> rotationMatrix r A
     | Mirror(A)                               -> mirroring A
     | Othornormal(u,v,w)                      -> orthornormalMatrix u v w
     | _                                       -> failwith "The AffineTransform List type is not supported in this function"
 
 (* Translates an AffineTransform to its inverse matrix counterpart *)
 let translateInverseTransformation (at : AffineTransformation) = 
    match at with
    | Move(x, y, z)                           -> inverseTranslationMatrix x y z
    | Scale(x, y, z)                          -> inverseScalingMatrix x y z
    | Shear(a1, a2, distance)                 -> getShearingMatrix (a1, a2) distance true
    | Rotate(r, A)                            -> inverseRotationMatrix r A
    | Mirror(A)                               -> mirroring A
    | Othornormal(u,v,w)                      -> inverseOrthornormalMatrix u v w
    | _                                       -> failwith "The AffineTransform List type is not supported in this function"

 (* Translates from a list of AffineTransformations to a single transformation Matrix *)
 let affineToMatrix (l : AffineTransformation list) (inverse : bool) =
    if inverse
    then let i = List.foldBack(fun elem acc  -> (translateInverseTransformation elem) :: acc) l []   //After: T1 ... Tn
         match i with                                                                                //After: T1 ... Tn - This is correct for inverse
         | x::rest -> List.fold(fun acc elem -> multiply acc elem) x rest                        
    else let n = List.fold(fun acc elem      -> (translateTransformation elem) :: acc) [] l          //After: Tn ... T1
         match n with                                                                                //After: Tn ... T1 - This is correct for normal
         | x::rest -> List.fold(fun acc elem -> multiply acc elem) x rest                            
    
 (* Helper that generates a Ray -> Ray and Vector -> Vector (normal) functions *)
 let shapeShifter (transformation : Matrix) (inverseTransformation : Matrix) =
    //Transforms a Ray
    let rayFunc (r : Ray) = 
        let o = r.Origin
        let d = r.Direction
        let p = multiplyToPoint inverseTransformation o
        let dir = multiplyToVector inverseTransformation d
        Ray(p, dir)
    //Tranforms the normal vector
    let normalFunc (v : Vector) = multiplyToVector (transpose inverseTransformation) v    
    //Transform a point
    let pointFunc (p : Point) = multiplyToPoint inverseTransformation p
    //Transform a bounding box
    let BBfunc (BB : BoundingBox) =
        let (p1, p2, p3, p4, p5, p6, p7, p8) = getCorners BB
        let tp1 = multiplyToPoint transformation p1
        let tp2 = multiplyToPoint transformation p2
        let tp3 = multiplyToPoint transformation p3
        let tp4 = multiplyToPoint transformation p4
        let tp5 = multiplyToPoint transformation p5
        let tp6 = multiplyToPoint transformation p6
        let tp7 = multiplyToPoint transformation p7
        let tp8 = multiplyToPoint transformation p8
        bbFromCorners [|tp1;tp2;tp3;tp4;tp5;tp6;tp7;tp8|]
    Shape.transformWithNormal rayFunc normalFunc pointFunc BBfunc
    
 (* Apply transformations to a shape *)
 let transformShape (l : AffineTransformation list) (s : Shape) = shapeShifter (affineToMatrix l false) (affineToMatrix l true) s

 (* Used to combine a list of AffineTransformation into a single AffinneTransformation. *)
 /// <summary>Created for ease of use with the API.</summary>
 let combineTranformation (l : AffineTransformation list) = List(l)
  
 (* Used to transform a Shape based on the given AffineTransformation *)
 /// <summary>Created for ease of use with the API.</summary>
 let tranformShapeAPI (s: Shape) (l : AffineTransformation) =
    match l with
    | List(v) -> transformShape v s
    | _       -> transformShape [l] s