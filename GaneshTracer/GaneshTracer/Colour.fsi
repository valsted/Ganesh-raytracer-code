namespace GaneshTracer

open System.Drawing

module Colour =
    exception ColourException of string
    [<Struct>]
    type Colour =
      static member ( + ) : Colour * Colour -> Colour
      static member ( - ) : Colour * Colour -> Colour
      static member ( * ) : Colour * Colour -> Colour
      static member ( * ) : float  * Colour -> Colour
    
    /// <summary>
    /// Creates a Colour from three float values specifying red, green og blue
    /// </summary>
    val mkColour : float -> float -> float -> Colour
    /// <summary>
    /// Gets the red value of a Colour
    /// </summary>
    val getR : Colour -> float
    /// <summary>
    /// Gets the green value of a Colour
    /// </summary>
    val getG : Colour -> float
    /// <summary>
    /// Gets the blue value of a Colour
    /// </summary>
    val getB : Colour -> float
    /// <summary>
    /// Scales a Colour by a floating point
    /// </summary>
    val scale: Colour -> float -> Colour
    /// <summary>
    /// Combines two Colours by a weight between 0.0 and 1.0
    /// </summary>
    val merge : float -> Colour -> Colour -> Colour
    /// <summary>
    /// Finds the average of two 8bit (system.drawing) colors
    /// </summary>
    val average : Color -> Color -> Color
    /// <summary>
    /// Finds the average of a list of 8bit (system.drawing) colors
    /// </summary> 
    val avgColors : Color list -> Color
    /// <summary>
    /// Finds the average of a sample set of Color options
    /// </summary>
    val averageOpt : Color option list -> Color
    /// <summary>
    /// Calculates the absolute difference (summarized pr component) of two colors.
    /// </summary>
    val difference : Color -> Color -> int
    /// <summary>
    /// Finds the difference of two 8bit (system.drawing) color options
    /// </summary>
    val colOptDiff : Color option -> Color option -> bool
    /// <summary>
    /// Converts the Colour into a standard Color
    /// </summary>
    val toColor : Colour -> Color 
    /// <summary>
    /// Maps the floating point colour values to 8-bit integer colour space using the supplied gamma
    /// </summary>
    val encodeColor : float -> Colour -> Color
    /// <summary>
    /// Creates a Colour from a Color
    /// </summary>
    val fromColor : Color -> Colour
    /// Converts a color to a light safe one. That is, make sure every component has a minimum value
    /// Such that no colors disappear in for instance 100% blue lights
    val mkLightSafe : Colour -> Colour
