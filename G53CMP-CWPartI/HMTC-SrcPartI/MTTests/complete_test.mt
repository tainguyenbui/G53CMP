// This is a comment.  It continues to the end of the line.
let
    var m: Integer;
    const n: Integer = 9;
	var x: Integer;
	var b: Bool;
	var c: Char
	//A comment and some nonsense: var l: Integer;
in
    begin
        m := 1 + 2 * (n + 1);
        putint(m);
		x := 0;
		
		//Task I.1
		//A simple repeat until test
		repeat 
		  begin 
			x := x + 1;
			b := b
		  end
		until x > 42;
		
		
		//Task I.2
		//Testing just the syntax. Use associativity.mt for associativity tests to check the AST.
		m := (b ? x : y) + z;
		e := e1 ? e2 : e3 ? e4 : e5;
		f := e1 ? e2 ? e3 : e4 : e5;
		
		//Task I.3
		//Testing all if then else possibilities:
		
		// if then:
		if x < 4
		  then x := 4;
		  
		// if then else:
		if true
		  then x := 1337
		else b:= false;
		 
		//if then, elsifs
		if 32 * 3 - 0 / 5 == 3
			then m := 3
		elsif 4 > 4
          then 
		     begin
                x := 3;
                x := 4
             end
	     	 s   elselif 5 > 4
		  then b := 3 < 4;
		
		//if, then, elsifs and else:
		if 3 > 4 
		  then putint(m)
		elsif 4 > 4
          then 
		     begin
                x := 3;
                x := 4
             end
	    elsif 5 > 4
		  then b := 3 < 4
		else putint(n);
	    
		//Task I.4
		

		// Test of escapable characters
		c := '\n';
		c := '\r';
		c := '\t';		
		c := '\\';
		c := '\'';
		
		// Some normal characters
		c := 'a';
		c := ' ';
		c := '*';
		c := '1'
		
    end
//And a comment at the end