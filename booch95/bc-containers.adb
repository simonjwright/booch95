package body BC.Containers is 

   procedure Reset (Obj : in out Iterator) is
   begin
      if Cardinality (Obj.C.all) > 0 then
     Obj.Index := 1;
      else
     Obj.Index := 0;
      end if;
   end Reset;

   procedure Next (Obj : in out Iterator) is
   begin
      Obj.Index := Obj.Index + 1;
   end Next;

   function Is_Done (Obj : Iterator) return Boolean is
   begin
     return (Obj.Index < 1) or else (Obj.Index > Cardinality (Obj.C.all));
   end Is_Done;

   function Current_Item (Obj : Iterator) return Item is
   begin
      return Item_At (Obj.C.all, Obj.Index).all;
   end Current_Item;

   function Current_Item (Obj : Iterator) return Item_Ptr is
   begin
      return Item_At (Obj.C.all, Obj.Index);
   end Current_Item;

   function Visit (Obj : access Passive_Iterator) return Boolean is
      Iter : Iterator(Obj.C);
      Temp : Item;
      Success : Boolean;
   begin
      while not Is_Done(Iter) loop
	 Temp := Current_Item(Iter);
	 Apply(Temp, Success);
	 if Success then
	    Next(Iter);
	 else
	    return False;
	 end if;
      end loop;
      return True;
   end Visit;

   function Modify (Obj : access Passive_Iterator) return Boolean is
      Iter : Iterator(Obj.C);
      Temp : Item_Ptr;
      Success : Boolean;
   begin
      while not Is_Done(Iter) loop
	 Temp := Current_Item(Iter);
	 Apply(Temp, Success);
	 if Success then
	    Next(Iter);
	 else
	    return False;
	 end if;
      end loop;
      return True;
   end Modify;

   function Item_At (Obj : Container; Index : Natural) return Item_Ptr is
   begin
      pragma Assert ( False, "Failure to override Item_At operation");
      raise Abstraction_Violation;
      return null;
   end Item_At;

   function Cardinality (Obj : Container) return Integer is
   begin
      pragma Assert ( False, "Failure to override Cardinality operation");
      raise Abstraction_Violation;
      return 0;
   end Cardinality;

end BC.Containers;
