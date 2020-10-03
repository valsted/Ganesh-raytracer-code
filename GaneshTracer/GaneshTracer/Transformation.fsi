namespace GaneshTracer

open Base
open Shape

module Transformation =
/// <summary>Used to specify the axis of a rotation and/or mirroring</summary>
 type Axis = X | Y | Z

 /// <summary>Used to concatenate transformations</summary>
 type AffineTransformation =
 | Move of float * float * float
 | Scale of float * float * float
 | Shear of Axis * Axis * float
 | Rotate of float * Axis
 | Mirror of Axis
 | Othornormal of Vector * Vector * Vector
 | List of AffineTransformation list
 
 /// <summary>Returns a new shape with the transformations applied</summary>
 val transformShape : AffineTransformation list -> Shape -> Shape
 
 /// <summary>Created for ease of use with the API.</summary>
 val combineTranformation : AffineTransformation list -> AffineTransformation

 /// <summary>Created for ease of use with the API.</summary>
 val tranformShapeAPI : Shape -> AffineTransformation -> Shape