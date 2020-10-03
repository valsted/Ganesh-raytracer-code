namespace GaneshTracer

open Colour

//Module for working with materials for textures
module Material = 
    type Material

    //Gets the internal values of the given material
    val color : Material -> Colour
    val reflectivity : Material -> float

    //Creates a material with set colour and reflection
    val create : Colour -> reflection : float -> Material

    //Creates matte material with colour
    val createFlat : Colour -> Material

    //Creates white material with 100% reflection
    val createMirror : Material

    //Functions for creating flat materials of named colour
    val black : Material
    val white : Material
    val grey : Material
    val red : Material
    val green : Material
    val blue : Material