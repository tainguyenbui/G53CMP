// Reads two numbers and then prints all numbers form the first to the
// second using a repeat loop. The first number is always printed.
let
    var m : Integer;
    var n : Integer
in begin
    getint(m);
    getint(n);
    repeat
        begin
            putint(m);
            m := m + 1
        end
    until m > n
end
