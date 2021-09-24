#nowarn "40"

open Combinator
open Common
open Parse

let dataSrc = 
  //System.IO.File.ReadAllText("test.psl")
  "fun smin(d1, d2, k)
{
    let h = clamp(0.5 + 0.5 * (d2 - d1) / k, 0.0, 1.0);
    lerp(d2, d1, h) - k * h * (1.0 - h)
}

fun map(p)
{
    let d1 = length(p) - 0.3;
    let d2 = length(p + float3(0.2, sin(time())*0.3, 0)) - 0.3;
    smin(d1, d2, 0.05)
}

fun march(ro, rd)
{
    let t = 0;
    let i = 0;
    while (i < 15)
    {
        let dist = map(ro + t * rd);
        let t = t + dist;
        let i = i + 1;
    }
    t
}

let p = 2.0 * (uv() - 0.5);
let ro = float3(0.0, 0.0, -1.0);
let rd = normalize(float3(p, p, 1));

let d = march(ro, rd);
if (d < 1)
{
    let hit = ro + d * rd;
    let a = map(hit+float3(0.01, 0, 0)) - map(hit-float3(0.01, 0, 0));
    let b = map(hit+float3(0, 0.01, 0)) - map(hit-float3(0, 0.01, 0));
    let c = map(hit+float3(0, 0, 0.01)) - map(hit-float3(0, 0, 0.01));
    normalize(float3(a, b, c)) * 0.5 + 0.5
}
else
{
    0
}"
  |> mkMultiLineParser

let result, state = blockP dataSrc
match result with
| Success v -> printfn "Result: %A\n\nState: %A" v state 
| _ -> printfn "Error: %A" state