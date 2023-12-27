open FsCheck
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

// https://fsharpforfunandprofit.com/posts/convenience-types/#most-f-types-are-automatically-comparable 
// By adding the distance that I want to compare on first, it is possible to use the default comparison operator
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

let updateMapWithSet map updates =  
    updates |> Set.fold (fun accMap (key,value) -> Map.add value key accMap) map

let updatePQWithSet Q updates =  
    updates |> Set.fold (fun q element -> PQ.insert element q) Q 

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

// Using notation from https://www-student.cse.buffalo.edu/~atri/cse331/support/notation/shortest-path.html
// Somewhat different in my book
// d' is the map from Node to best known upper bound on known nodes at any time,
// d' u would give upper bound to u
// R is the region to explore, R' is the updated region to explore after popping the smallest element
// d_u is the known distance to node u
// s is the source node
// Ε is the set of all edges in the Graph.
[<TailCall>]
let dijkstra Ε s =  
   let rec innerDijkstra R d' =  
        match (PQ.tryPop R) with
            | None  -> d' 
            | Some ((d_u,u), R') ->  
                let relaxed: d_Node Set = adjacent u Ε 
                                            |> relax d_u d' 
                innerDijkstra (updatePQWithSet R' relaxed) 
                              (updateMapWithSet d' relaxed)
   let pq = PQ.empty false |> PQ.insert (0, s)
   innerDijkstra pq (Map.ofList [s, 0]) 

let s = {n = 1}

let romeData = System.IO.File.ReadAllLines("rome99.txt")
let vertexCount = int romeData.[0]
let edgeCount = int romeData.[1]
let romeEdges = romeData.[2..]
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun line -> { u = {n = int line.[0]}; v = {n = int line.[1]}; ω_uv = int line.[2] })
                    |> Set.ofArray
               
[<Xunit.Fact>]
let ``Dijkstra test`` () = 
    let result = dijkstra exampleEdges s
    let expected = Map.ofList [ ({n = 1}, 0); ({n = 2}, 7); ({n = 3}, 9); ({n = 4}, 20); ({n = 5}, 26); ({n = 6}, 11) ]
    Xunit.Assert.Equal<Map<Node,int>>(expected, result)

[<EntryPoint>]
let main argv = 
    dijkstra exampleEdges s |> printfn "Dijstra %A"
    dijkstra romeEdges ({n = 0}) |> printfn "Dijstra Rome %A"
    for edge in (dijkstra romeEdges {n = 0}) do
        printfn "%A" edge
    0 


