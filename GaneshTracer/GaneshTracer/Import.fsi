namespace GaneshTracer

open FParsec
open Base

module Import =    
    val parsePLY: string -> bool -> (Vertex * Vertex * Vertex)[] option