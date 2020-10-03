namespace GaneshTracer

open Material

module Texture =
    type Texture 
    
    /// <summary>
    /// Takes in a function, which applies a material to x y coordinates, and returns texture.
    /// </summary>
    val mkTexture : (float -> float -> Material) -> Texture

    /// <summary>
    /// Returns a texture, where the same material is applied to all x y coordinates.
    /// </summary>
    val mkMatTexture : Material -> Texture

    /// <summary>
    /// Extracts the mapping function for a given Texture.
    /// </summary>
    val getMappingFunc : Texture -> (float -> float -> Material)

    /// <summary>
    /// Returns the material at the given pair of surface coordinates.
    /// </summary>
    val getMaterialAt : Texture -> float * float -> Material

    /// <summary>
    //Returns a texture, where the same material with the specified color is applied to all x y coordinates.
    /// </summary>
    val mkBlackTexture : Texture
    val mkWhiteTexture : Texture
    val mkGrayTexture  : Texture
    val mkRedTexture   : Texture
    val mkGreenTexture : Texture
    val mkBlueTexture  : Texture
    
    /// <summary>
    /// Textures for testing purposes.
    /// </summary>
    val mkCheckeredTexture : Texture
    val mkMixedTexture : Texture

    /// <summary>
    /// Returns a texture based on a coordinate map and a file.
    /// </summary>
    val mkTextureFromFile : string -> Texture