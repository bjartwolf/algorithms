open FsCheck
open FsCheck.Xunit
open FSharpx.Collections

type Node = { Id : int}
type Nodes = Node Set

type Edge = { 
    Source : Node;
    Target : Node;
    Weight : int;
}
type Edges = Edge Set
let exampleNodes = Set.ofList [ {Id = 1}; {Id = 2}; {Id = 3}; {Id = 4}; {Id = 5}; {Id = 6} ]
let exampleEdges = Set.ofList [ { Source = {Id = 1}; Target = {Id = 2}; Weight = 7 }; 
                                { Source = {Id = 1}; Target = {Id = 3}; Weight = 9 }; 
                                { Source = {Id = 2}; Target = {Id = 3}; Weight = 10 }; 
                                { Source = {Id = 2}; Target = {Id = 4}; Weight = 15 }; 
                                { Source = {Id = 3}; Target = {Id = 4}; Weight = 11 }; 
                                { Source = {Id = 3}; Target = {Id = 6}; Weight = 2 }; 
                                { Source = {Id = 4}; Target = {Id = 5}; Weight = 6 }; 
                                { Source = {Id = 5}; Target = {Id = 6}; Weight = 9 } ]

type DistanceNode = int * Node

[<Property>]
let ``PriorityQueue test with distanceNodes `` (x: DistanceNode)
                                               (y: DistanceNode)
                                               (z: DistanceNode) = 
    let pq = PriorityQueue.empty false 
             |> PriorityQueue.insert y 
             |> PriorityQueue.insert x
             |> PriorityQueue.insert z
    let ((firstD, _),a) = PriorityQueue.pop pq
    let ((sndD,_),b) = PriorityQueue.pop a 
    let ((thirdD, _),_) = PriorityQueue.pop b 
    firstD <= sndD && sndD <= thirdD

let updateMap (originalMap: Map<Node,int>) (updates: DistanceNode Set) :Map<Node,int> =
    updates |> Set.fold (fun accMap (key,value) -> Map.add value key accMap) originalMap

let updatePriorityQueue (stack: IPriorityQueue<DistanceNode>) (updates: DistanceNode Set): IPriorityQueue<DistanceNode>  =
    updates |> Set.fold (fun q dn -> PriorityQueue.insert dn q) stack 

let closerOrUndiscovered (explored: Map<Node,int>) ((dist,n):DistanceNode) : bool = 
    match Map.tryFind n explored with
        | Some existingDist -> dist < existingDist
        | None -> true

[<TailCall>]
let dijkstra (e: Edges) (start: Node) : Map<Node, int> =
   let rec innerDijkstra (frontier: IPriorityQueue<DistanceNode>) (explored : Map<Node,int>) = 
        match (PriorityQueue.tryPop frontier) with
            | None  -> explored 
            | Some ((currentDistance,currentNode),remainingFrontier) ->  
                let closerNeighbors = e |> Set.filter (fun edge -> edge.Source = currentNode)
                                        |> Set.map (fun edge -> currentDistance + edge.Weight, edge.Target)
                                        |> Set.filter (fun dn -> closerOrUndiscovered explored dn) 
                innerDijkstra (closerNeighbors |> updatePriorityQueue remainingFrontier)  (closerNeighbors |> updateMap explored)
   let pq = PriorityQueue.empty false 
            |> PriorityQueue.insert (0, start)
   innerDijkstra pq (Map.empty |> Map.add start 0) 

let startNode = {Id = 1}

let romeData = System.IO.File.ReadAllLines("rome99.txt")
let vertexCount = int romeData.[0]
let edgeCount = int romeData.[1]
let romeEdges = romeData.[2..]
                    |> Array.map (fun line -> line.Split(' '))
                    |> Array.map (fun line -> { Source = {Id = int line.[0]}; Target = {Id = int line.[1]}; Weight = int line.[2] })
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
    0 


