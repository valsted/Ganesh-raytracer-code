namespace GaneshTracer

module ShapeFactory =
    open Base
    open Material
    open Texture
    open Shape
    open PolynomialCalc
    open BaseShape

    /// <summary>
    /// Creates a sphere centered around (0,0,0) with a specified radius
    /// </summary>
    val mkCenteredSphere : radius:float -> Texture -> Shape
    /// <summary>
    /// Creates a grey sphere centered around (0,0,0) with a radius 1.0
    /// </summary>
    val mkSimpleSphere : Shape
    /// <summary>
    /// Creates a sphere with a custom center,radius and texture
    /// </summary>
    val mkSphere : center:Point -> radius:float -> Texture -> Shape
    /// <summary>
    /// Creates a box with the two corners minPoint and maxPoint and a texture for each side
    /// </summary>
    val mkBox : minPoint:Point -> maxPoint:Point -> front:Texture -> back:Texture -> top:Texture -> bottom:Texture -> left:Texture -> right:Texture -> Shape
    /// <summary>
    /// Creates a box with the two corners minPoint and maxPoint and a texture
    /// </summary>
    val mkSimpleTextureBox : minPoint:Point -> maxPoint:Point -> tex:Texture -> Shape
    /// <summary>
    /// Creates the triangle between the points o, p and q with the given material
    /// </summary>
    val mkTriangle : o:Point -> p:Point -> q:Point -> Material -> Shape
    /// <summary>
    /// Creates a triangle for a mesh given three vertices and a bool that indicates if smooth shading should be used
    /// </summary>
    val mkMeshTriangle : ov:Vertex -> pv:Vertex -> qv:Vertex -> smooth:bool -> tex:Texture -> Shape 
    /// <summary>
    /// Creates an infinite plane i y=0 with a single material
    /// </summary>
    val mkSimpleGround: mat:Material -> Shape
    /// <summary>
    /// Creates an infinite plane i y=0 with a texture attached
    /// </summary>
    val mkTexturedGround: tex:Texture -> Shape
    /// <summary>
    /// Creates a rectangle with origin in bottomleft and a plane defined by Vec(bottomleft - topLeft) & Vec(bottomLeft - bottomRight)
    /// </summary>
    val mkRectangle : bottomLeft: Point -> topLeft: Point -> bottomRight: Point -> tex: Texture -> Shape
    /// <summary
    /// Creates a disc in (0,0,0) with the defined radius and texture
    /// </summary>
    val mkCenteredDisc: radius:float -> tex:Texture -> Shape
    /// <summary>
    /// Creates a disc at the given centerPoint with defined radius and texture
    /// </summary>
    val mkDisc: center:Point -> radius:float -> tex:Texture -> Shape
    /// <summary>
    /// Creates a disc at the given centerPoint with defined radius and texture
    /// </summary>
    val mkFlatDisc: center:Point -> radius:float -> tex:Texture -> Shape
    /// <summary>
    /// Creates an open cylinder with the given height and radius and centered around the given point
    /// </summary>
    val mkOpenCylinder: center:Point -> radius:float -> height:float -> tex:Texture -> Shape
    /// <summary>
    /// Creates ac closed cylinder with the given height and radius and centered around the given point
    /// </summary>
    val mkClosedCylinder: center:Point -> radius:float -> height:float -> tex:Texture -> top:Texture -> bottom:Texture -> Shape