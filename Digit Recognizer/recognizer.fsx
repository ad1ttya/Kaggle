open System.IO

type Observation = { Label : string; Pixels: int [] }

type Distance = int[] * int[] -> int 

let toObserve(csvData: string) =
  let columns = csvData.Split(',')
  let label = columns.[0]
  let pixels = columns.[1..] |> Array.map int
  { Label = label; Pixels = pixels}
  
let reader path =
  let data = File.ReadAllLines path
  data.[1..]
  |> Array.map toObserve
  
let trainingPath = @"C:\Workspace\Kaggle\Digit Recognizer\data\trainingsample.csv"
let trainingData = reader trainingPath

let manhattanDistance (pixels1,pixels2) =
  Array.zip pixels1 pixels2
  |> Array.map (fun (x,y) -> abs (x-y))
  |> Array.sum
  
let euclideanDistance (pixels1, pixels2) =
  Array.zip pixels1 pixels2
  |> Array.map (fun (x,y) -> pown (x-y) 2)
  |> Array.sum
  
let train (trainingset:Observation[]) (dist: Distance) =
  let classify (pixels:int[]) =
    trainingset
    |> Array.minBy (fun x -> dist (x.Pixels, pixels))
    |> fun x -> x.Label
  classify
  
let classifierManhattan = train trainingData manhattanDistance
let classifierEuclidean = train trainingData euclideanDistance

let validationPath = @"C:\Workspace\Kaggle\Digit Recognizer\data\validationsample.csv"
let validationData = reader validationPath

validationData
  |> Array.averageBy (fun x -> if classifierEuclidean x.Pixels = x.Label then 1. else 0.)
  |> printfn "Correct: %.3f"