using ArgumentProcessor
using Test

@testset "ArgumentProcessor.jl" begin
    args = ["--a", "1.0", "-b", "true", "2.0"]
    l = layer("test", [
        argument("a", inputformat="%f"),
        argument("B", abbreviation="b", inputformat="%l")
    ], [parameter(1, inputformat="%f")])
    ArgumentProcessor.parse(args, [l]) == ((test = (args = (a = 1.0, B = true), pars = (par1 = 2.0,)),), Int64[])
end
