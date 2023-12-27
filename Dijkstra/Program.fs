﻿open FsCheck
open FsCheck.Xunit
open FSharpx.Collections

type Node = { Id : int}
type Nodes = Node Set

type Edge = { 
    u : Node;
    v : Node;
    w_u_v : int;
}
type Edges = Edge Set
let exampleNodes = Set.ofList [ {Id = 1}; {Id = 2}; {Id = 3}; {Id = 4}; {Id = 5}; {Id = 6} ]
let exampleEdges = Set.ofList [ { u = {Id = 1}; v = {Id = 2}; w_u_v = 7 }; 
                                { u = {Id = 1}; v = {Id = 3}; w_u_v = 9 }; 
                                { u = {Id = 2}; v = {Id = 3}; w_u_v = 10 }; 
                                { u = {Id = 2}; v = {Id = 4}; w_u_v = 15 }; 
                                { u = {Id = 3}; v = {Id = 4}; w_u_v = 11 }; 
                                { u = {Id = 3}; v = {Id = 6}; w_u_v = 2 }; 
                                { u = {Id = 4}; v = {Id = 5}; w_u_v = 6 }; 
                                { u = {Id = 5}; v = {Id = 6}; w_u_v = 9 } ]

type DistanceNode = int * Node

[<Property>]
let ``PriorityQueue test with distanceNodes `` (x: DistanceNode)
                                               (y: DistanceNode)
                                               (z: DistanceNode) = 
    let pq = PriorityQueue.empty false 
             |> PriorityQueue.insert x 
             |> PriorityQueue.insert y
             |> PriorityQueue.insert z
    let ((firstD, _),a) = PriorityQueue.pop pq
    let ((sndD,_),b) = PriorityQueue.pop a 
    let ((thirdD, _),_) = PriorityQueue.pop b 
    firstD <= sndD && sndD <= thirdD

let updateDiscovered originalMap updates =  
    updates |> Set.fold (fun accMap (key,value) -> Map.add value key accMap) originalMap

let updateFrontier R updates =  
    updates |> Set.fold (fun q dn -> PriorityQueue.insert dn q) R 

let closerOrUndiscovered explored ((dist,n):DistanceNode) = 
    match Map.tryFind n explored with
        | Some existingDist -> dist < existingDist
        | None -> true

let adjacent u e = 
    e |> Set.filter (fun edge -> edge.u = u)

let relax adjacent (d_u: int) explored = 
    adjacent 
      |> Set.map (fun u_v -> d_u + u_v.w_u_v, u_v.v)
      |> Set.filter (fun dn -> closerOrUndiscovered explored dn) 

//https://web.engr.oregonstate.edu/~glencora/wiki/uploads/dijkstra-proof.pdf

[<TailCall>]
let dijkstra edges n =  
   let rec innerDijkstra R explored =  
        match (PriorityQueue.tryPop R) with
            | None  -> explored 
            | Some ((d_v,u), R') ->  
                let adjacent = adjacent u edges
                let relaxed = relax adjacent d_v explored
                innerDijkstra (updateFrontier R' relaxed) 
                              (updateDiscovered explored relaxed)
   let pq = PriorityQueue.empty false |> PriorityQueue.insert (0, n)
   innerDijkstra pq (Map.ofList [n, 0]) 

let startNode = {Id = 1}

let romeData = System.IO.File.ReadAllLines("rome99.txt")
let vertexCount = int romeData.[0]
let edgeCount = int romeData.[1]
let romeEdges = romeData.[2..]
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun line -> { u = {Id = int line.[0]}; v = {Id = int line.[1]}; w_u_v = int line.[2] })
                    |> Set.ofArray
               
[<Xunit.Fact>]
let ``Dijkstra test`` () = 
    let result = dijkstra exampleEdges startNode
    let expected = Map.ofList [ ({Id = 1}, 0); ({Id = 2}, 7); ({Id = 3}, 9); ({Id = 4}, 20); ({Id = 5}, 26); ({Id = 6}, 11) ]
    Xunit.Assert.Equal<Map<Node,int>>(expected, result)

[<EntryPoint>]
let main argv = 
    dijkstra exampleEdges startNode |> printfn "Dijstra %A"
    dijkstra romeEdges ({Id = 0}) |> printfn "Dijstra Rome %A"
    for edge in (dijkstra romeEdges {Id = 0}) do
        printfn "%A" edge
    0 


