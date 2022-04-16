using ArgumentProcessor
using Test

@testset "Inner buffer" begin
    @test isnothing(addflag!("flag"))
    @test isnothing(addopt!("opt1", fmt=" %f"))
    @test isnothing(addpar!(1, fmt="%d"))
    t = ArgumentProcessor.parse("--flag --opt1 0.1 2")
    @test t == (flag=true, opt1=0.1, par1=2)
end
