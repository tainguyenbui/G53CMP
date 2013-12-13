// Recursive function for computing factorial. Needs conditional expr. to work.

let
    fun fac(n : Integer) : Integer =  n <= 1 ? 1 : n * fac(n - 1)
in begin
    putint(fac(8))
end
