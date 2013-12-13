// As "repeat.mt", put also prints prompts using "putchr".
let
    var m : Integer;
    var n : Integer
in begin
    putchr('F');
    putchr('r');
    putchr('o');
    putchr('m');
    putchr('?');
    putchr('\n');
    getint(m);
    putchr('T');
    putchr('o');
    putchr('?');
    putchr('\n');
    getint(n);
    repeat
        begin
            putint(m);
            m := m + 1
        end
    until m > n
end
