// This is a comment.  It continues to the end of the line.
begin
    m := (b ? x : y) + z;
	
	// Test right associativity
	e := e1 ? e2 : e3 ? e4 : e5;
	// This expression should roughly look like:
	//	CmdAssign <line 4, column 9>
	//  	ExpVar "e"
	//  	ExpCond <line 4, column 14>
	//			ExpVar "e1"
	//			ExpVar "e2"
	//			ExpCond <line 4, column 24>
	//	  			ExpVar "e3"
	//	  			ExpVar "e4"
	//	  			ExpVar "e5"

	
	f := e1 ? e2 ? e3 : e4 : e5
	// This expression should roughly look like:
	//  CmdAssign <line 5, column 9>
	//		ExpVar "f"
	//		ExpCond <line 5, column 14>
	//			ExpVar "e1"
	//			ExpCond <line 5, column 19>
	//				ExpVar "e2"
	//				ExpVar "e3"
	//				ExpVar "e4"
	//			ExpVar "e5"

end
