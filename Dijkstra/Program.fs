﻿open FsCheck
open FsCheck.Xunit
open FSharpx.Collections

type Node = { n : int}
type Nodes = Node Set

module PQ = PriorityQueue

type Edge = { 
    u : Node;
    v : Node;
    ω_uv : int;
}
type Edges = Edge Set
let exampleNodes = Set.ofList [ {n = 1}; {n = 2}; {n = 3}; {n = 4}; {n = 5}; {n = 6} ]
let exampleEdges = Set.ofList [ { u = {n = 1}; v = {n = 2}; ω_uv = 7 }; 
                                { u = {n = 1}; v = {n = 3}; ω_uv = 9 }; 
                                { u = {n = 2}; v = {n = 3}; ω_uv = 10 }; 
                                { u = {n = 2}; v = {n = 4}; ω_uv = 15 }; 
                                { u = {n = 3}; v = {n = 4}; ω_uv = 11 }; 
                                { u = {n = 3}; v = {n = 6}; ω_uv = 2 }; 
                                { u = {n = 4}; v = {n = 5}; ω_uv = 6 }; 
                                { u = {n = 5}; v = {n = 6}; ω_uv = 9 } ]

type d_Node = int * Node

[<Property>]
let ``PriorityQueue test with distanceNodes `` (x: d_Node)
                                               (y: d_Node)
                                               (z: d_Node) = 
    let pq = PQ.empty false 
             |> PQ.insert x 
             |> PQ.insert y
             |> PQ.insert z
    let ((firstD, _),a) = PQ.pop pq
    let ((sndD,_),b) = PQ.pop a 
    let ((thirdD, _),_) = PQ.pop b 
    firstD <= sndD && sndD <= thirdD

let updateExplored explored updates =  
    updates |> Set.fold (fun accMap (key,value) -> Map.add value key accMap) explored

let updateQ Q updates =  
    updates |> Set.fold (fun q dn -> PQ.insert dn q) Q 


let adjacent u e = 
    e |> Set.filter (fun edge -> edge.u = u)

let relax (d_u: int) explored vs = 
    let closerOrUndiscovered explored ((dist,n):d_Node) = 
        match Map.tryFind n explored with
            | Some existingDist -> dist < existingDist
            | None -> true
    vs 
      |> Set.map (fun v -> d_u + v.ω_uv, v.v)
      |> Set.filter (fun dn -> closerOrUndiscovered explored dn) 

//https://web.engr.oregonstate.edu/~glencora/wiki/uploads/dijkstra-proof.pdf

[<TailCall>]
let dijkstra edges n =  
   let rec innerDijkstra Q explored =  
        match (PQ.tryPop Q) with
            | None  -> explored 
            | Some ((d_u,u), Q') ->  
                let relaxed: d_Node Set = adjacent u edges 
                                            |> relax d_u explored 
                innerDijkstra (updateQ Q' relaxed) 
                              (updateExplored explored relaxed)
   let pq = PQ.empty false |> PQ.insert (0, n)
   innerDijkstra pq (Map.ofList [n, 0]) 

let startNode = {n = 1}

let romeData = System.IO.File.ReadAllLines("rome99.txt")
let vertexCount = int romeData.[0]
let edgeCount = int romeData.[1]
let romeEdges = romeData.[2..]
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun line -> { u = {n = int line.[0]}; v = {n = int line.[1]}; ω_uv = int line.[2] })
                    |> Set.ofArray
               
[<Xunit.Fact>]
let ``Dijkstra test`` () = 
    let result = dijkstra exampleEdges startNode
    let expected = Map.ofList [ ({n = 1}, 0); ({n = 2}, 7); ({n = 3}, 9); ({n = 4}, 20); ({n = 5}, 26); ({n = 6}, 11) ]
    Xunit.Assert.Equal<Map<Node,int>>(expected, result)

[<EntryPoint>]
let main argv = 
    dijkstra exampleEdges startNode |> printfn "Dijstra %A"
    dijkstra romeEdges ({n = 0}) |> printfn "Dijstra Rome %A"
    for edge in (dijkstra romeEdges {n = 0}) do
        printfn "%A" edge
    0 


