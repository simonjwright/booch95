package body Bc.Containers.Queues is

   procedure Purge (Obj : in out Queue) is
   begin
      pragma Assert ( False, "Failure to override Purge operation");
      raise Abstraction_Violation;
      null;
   end Purge;

   procedure Add (Obj : in out Queue; Elem : in out Item) is
   begin
      pragma Assert ( False, "Failure to override Add operation");
      raise Abstraction_Violation;
      null;
   end Add;

   function "=" (Left, Right : access Queue'Class) return Boolean is
   begin
      if Left.all = Right.all then
	 return True;
      elsif Cardinality (Left.all) /= Cardinality (Right.all) then
	 return False;
      else
	 declare
	    Left_Iter : Iterator (Left);
	    Right_Iter : Iterator (Right);
	 begin
	    while not Is_Done (Left_Iter) and then 
	      not Is_Done (Right_Iter) loop
	       if Current_Item (Left_Iter).all /=
		 Current_Item (Right_Iter).all then
		  return False;
	       end if;
	       Next (Left_Iter);
	       Next (Right_Iter);
	    end loop;
	    return True;
	 end;
      end if;
   end "=";

   procedure Copy (From : access Queue'Class; To : access Queue'Class) is
      Iter : Iterator (From);
   begin
      Clear (To.all);
      Reset (Iter);
      while not Is_Done (Iter) loop
	 Append (To.all, Current_Item (Iter).all);
	 Next (Iter);
      end loop;
   end Copy;

end Bc.Containers.Queues;


