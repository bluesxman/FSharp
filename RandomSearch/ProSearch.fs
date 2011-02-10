open System
open System.IO

let numFile = @"C:\Numbers.bin"

let cost i = i

let genFile qty min max file =
    let rand = new Random()
    use out = new BinaryWriter(File.Create(file))
    let nums = seq {for i in 1 .. qty -> rand.Next(min, max)}
    nums |> Seq.iter out.Write

let searchFile file samples =
    use input = new BinaryReader(File.OpenRead(file))
    let rand = new Random()
    let bounce = fun () -> input.BaseStream.Seek((int64 (rand.Next(0, int input.BaseStream.Length / 4) * 4), SeekOrigin.Begin)) |> ignore
    let nums = seq{for i in 1 .. samples -> bounce() ; input.ReadInt32()}
    let pick best i = if cost i < cost best then i else best
    nums |> Seq.fold pick (Seq.head nums)

genFile 10000000 0 1000000 numFile
printf "minimum found = %d\n" (searchFile numFile 1000)