namespace GaneshTracer

open Base
open Material
open Scene
open Colour
open Light
open System

module Camera =
    type Camera

    type TraceMode = 
        | Standard    //Standard single rayfiring
        | Interpolate //Fires half the rays, guesses the rest
        | AA          //Fires all rays, averages result to reduce aliasing
        | Hybrid      //Fires half the rays, retraces the missing that are near edges and guesses the rest
        | HybridAA    //Same as hybrid mode, but post-processes AA on detected edges

    /// <summary> 
    /// Construct a camera at 'position' pointed at 'lookat'. The 'up' vector describes which way is up.
    /// The viewplane is has dimensions 'width' and 'height', has a pixel resolution of 'resX' 
    /// times 'resY', and is 'distance' units in front of the camera. </summary>
    /// <param name="position">(Point) position - The Camera's position in world coordinates. </param>
    /// <param name="lookat">(Point) lookAt - The center of the View Plane (what we call viewPort). </param>
    /// <param name="up">(Vector) upV - A vector that describes which way that is "up". </param>
    /// <param name="distance">(float) zoom - A zoom value that represents the distance to the viewPort. </param>
    /// <param name="width">(float) width - Width of the viewPort. </param>
    /// <param name="height">(float) height - Height of the viewPort. </param>
    /// <param name="resX">(int) resX - Width of the viewPort in terms of pixels. </param>
    /// <param name="resY">(int) resY - Height of the viewPort in terms of pixels. </param>
    /// <param name="pixelWidth"> (float) pixelWidth - Width of a single pixel. </param>
    /// <param name="pixelHeight"> (float) pixelHeight - Height of a single pixel. </param>
    /// <param name="viewPort"> (Ray[,] viewPort - The ViewPort. </param>
    val mkCamera : position : Point -> lookat : Point -> up : Vector -> distance : float -> 
        width : float -> height : float -> resX : int -> resY : int -> Camera
    
    /// Traces a single ray (recusively) 
    val traceRay : Ray -> Scene -> Colour -> Colour

    /// RayTraces a scene with the given camera. 
    /// Tracemode specifies the rendering pattern and post-processing used.
    /// Optionally takes a gamma value for conversion to screen space, uses default (sqrt based) otherwise
    val trace : Scene -> Camera -> TraceMode -> gamma: float option -> Drawing.Color[][]