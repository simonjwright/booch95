--  This file contains the declaration of the doubly-linked list.
with Unchecked_Deallocation;
package body Bc.Containers.Lists.Double is

   use Double_Nodes;

   procedure delete is
     new Unchecked_deallocation(Double_Node, Double_Node_Ref);

   function Create(Obj : Double_List) return Double_List is
      Temp : Double_List := Double_List'(Controlled with Rep=> Obj.Rep);
   begin
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
      return Temp;
   end Create;

   function "="(L, R : Double_List) return Boolean is
   begin
      return L.Rep.all = R.Rep.all;
   end "=";

   procedure Clear(Obj : in out Double_List) is
      Curr : Double_Node_Ref := Obj.Rep;
      Ptr  : Double_Node_Ref;
   begin
      while Curr /= null loop
	 Ptr := Curr;
	 Curr := Curr.next;
	 if Ptr.Count > 1 then
	    Ptr.Count := Ptr.Count - 1;
	    exit;
	 else
	    if Curr /= null then
	       Curr.Prev := null;
	    end if;
	    delete (Ptr);
	 end if;
      end loop;
      Obj.Rep := null;
   end Clear;

   procedure Insert(Obj : in out Double_List; Elem : Item) is
   begin
      pragma Assert(Obj.Rep = null or else Obj.Rep.Prev = null,
		    "Attempt to Insert when not at Root");
      Obj.Rep := new Double_Node'(Elem, null, Obj.Rep, 1);
   end Insert;

   procedure Insert(Obj : in out Double_List; From_List : in Double_List) is
      Ptr : Double_Node_Ref := From_List.Rep;
   begin
      pragma Assert(Obj.Rep = null or else Obj.Rep.Prev = null,
		    "Attempt to Insert when not at Root");
      if Ptr /= null then
	 while Ptr.Next /= null loop
	    Ptr := Ptr.Next;
	 end loop;
      end if;
      Ptr.Next := Obj.Rep;
      if Obj.Rep /= null then
	 Obj.Rep.Prev := Ptr;
      end if;
      Obj.Rep := From_List.Rep;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Insert;

   procedure Insert(Obj : in out Double_List; Elem : Item; Before : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      if Curr = null or else Before = 1 then
	 Insert(Obj, Elem);
      else
	 while Curr /= null and then Index <= Before loop
	    Prev := Curr;
	    Curr := Curr.Next;
	    Index := Index + 1;
	 end loop;
	 pragma Assert(Curr /= null, "Attempt to Insert at NULL location");
	 Prev.Next := new Double_Node'(Elem, Prev, Curr, 1);
      end if;
   end Insert;

   procedure Insert(Obj : in out Double_List; From_List: in out Double_List;
					      Before : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Ptr  : Double_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      if Ptr /= null then
	 if Curr = null or else Before = 1 then
	   Insert(Obj, From_List);
	 else
	    pragma Assert(Ptr /= null or else Ptr.Prev = null,
			  "Attempt to Insert when Tree isn't root");
	    while Curr /= null and then Index <= Before loop
	       Prev := Curr;
	       Curr := Curr.Next;
	       Index := Index + 1;
	    end loop;
	    pragma Assert(Curr /= null, "Attempt to Insert at NULL location");
	    while Ptr.Next /= null loop
	       Ptr := Ptr.Next;
	    end loop;
	    Ptr.Next := Curr;
	    Curr.Prev := Ptr;
	    Prev.Next := From_List.Rep;
	    From_List.Rep.Prev := Prev;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Insert;

   procedure Append(Obj : in out Double_List; Elem : Item) is
      Curr : Double_Node_Ref := Obj.Rep;
   begin
      if Curr /= null then
	 while Curr.Next /= null loop
	    Curr := Curr.Next;
	 end loop;
      end if;
      if Curr /= null then
	 Curr.Next := new Double_Node'(Elem, Curr, null, 1);
      else
	 Obj.Rep := new Double_Node'(Elem, null, null, 1);
      end if;
   end Append;

   procedure Append(Obj : in out Double_List; From_List : in Double_List) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      pragma Assert(From_List.Rep /= null or else From_List.Rep.Prev /= null,
		    "Attempt to Append to a location that is not ROOT");
      if From_List.Rep /= null then
	 if Curr /= null then
	    while Curr.Next /= null loop
	       Curr := Curr.Next;
	    end loop;
         end if;
	 if Curr /= null then
	    Curr.Next := From_List.Rep;
	    From_List.Rep.Prev := Curr;
	 else
	    Obj.Rep := From_List.Rep;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Append;

   procedure Append(Obj : in out Double_List; Elem : Item; After : Natural) is
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      if Curr = null then
	 Append(Obj, ELem);
      else
	 while Curr /= null and then Index <= After loop
	    Curr := Curr.Next;
	    Index := Index + 1;
	 end loop;
	 pragma Assert(Curr /= null, "Attempt to Insert at NULL location");
	 Curr.Next := new Double_Node'(Elem, Curr, Curr.Next, 1);
      end if;
   end Append;

   procedure Append(Obj : in out Double_List; From_List : in Double_List;
					      After : Natural) is
      Curr : Double_Node_Ref := Obj.Rep;
      Ptr  : Double_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      if Ptr /= null then
	 if Curr = null then
	    Append(Obj, From_List);
	 else
	    pragma Assert(From_List.Rep /= null or else
			    From_List.Rep.Prev = null,
			     "Attempt to Insert at NULL location");
	    while Curr /= null and then Index <= After loop
	       Curr := Curr.Next;
	       Index := Index + 1;
	    end loop;
	    pragma Assert( Curr /= null, "Attempt to Insert with invalid Index");
	    while Ptr.Next /= null loop
	       Ptr := Ptr.Next;
	    end loop;
	    Ptr.Next := Curr.Next;
	    if Curr.Next /= null then
	       Curr.Next.Prev := Ptr;
	    end if;
	    From_List.Rep.Prev := Curr;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Append;

   procedure Remove(Obj : in out Double_List; From : Natural) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      while Curr /= null and then Index < From loop
	 Prev := CUrr;
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert(Curr /= null, "Attempt to Remove from Invalid location");
      if Prev /= null then
	 Prev.Next := Curr.Next;
      else
	 Obj.Rep := Curr.Next;
      end if;
      if Curr.Next /= null then
	 Curr.Next.Prev := Prev;
      end if;
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
	 Obj.Rep := Curr.Next;
	 Curr.Next := null;
      else
	 delete (Curr);
      end if;
   end Remove;

   procedure Purge (Obj : in out Double_LIst; From : Natural) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Ptr  : Double_Node_Ref;
      Index: Natural := 1;
   begin
      while Curr /= null and then Index <= From loop
	 Prev := Curr;
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert(Curr /= null, "Attempt to Purge with Invalid Index");
      if Prev /= null then
	 Prev.Next := null;
      else
	 Obj.Rep := null;
      end if;
      while Curr /= null loop
	 Curr.Prev := null;
	 Ptr := CUrr;
	 Curr := Curr.Next;
	 if Ptr.Count > 1 then
	    Ptr.Count := Ptr.Count - 1;
	    exit;
	 else
	    delete (Ptr);
	 end if;
      end loop;
   end Purge;

   procedure Purge (Obj : in out Double_List; From: Natural;Count:Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Ptr  : Double_Node_Ref;
      Index: Natural := 1;
      Cut  : Boolean := False;
   begin
      while Curr /= null and then Index <= From loop
	 Prev := Curr;
	 Curr := Curr.next;
      end loop;
      pragma Assert(Curr /= null, "Attempt to Purge with Invalid Index");
      if Prev /= null then
	 Prev.Next := null;
      else
	 Obj.Rep := null;
      end if;
      Index := 1;
      while Curr /= null and then Index <= Count loop
	 Ptr := Curr;
	 Curr := Curr.Next;
	 if Cut then
	    if Ptr.Count > 1 then
	       Ptr.COunt := Ptr.Count - 1;
	       Cut := False;
	    else
	       if Curr /= null then
		  Curr.Prev := null;
		  delete(Ptr);
	       end if;
	    end if;
	 end if;
	 Index := Index + 1;
      end loop;
      Ptr.Next := null;
      if CUrr /= null then
	 Curr.Prev := Prev;
	 if Prev /= null then
	    Prev.Next := Curr;
	 else
	    Obj.Rep := Curr;
	 end if;
      end if;
   end Purge;

   procedure Preserve(Obj : in out Double_List; From : Natural) is
      Temp : Double_List;
   begin
      Share(Temp, Obj, From);
      Share_Head(Obj, Temp);
   end Preserve;

   procedure Preserve(Obj: in out Double_List; From: Natural;Count:Positive) is
   begin
      Preserve(Obj, From);
      if Length(obj) >= Count then
	 Purge(Obj, Count);
      end if;
   end Preserve;

   procedure Share (Obj : in out Double_List; With_List: Double_List;
					      Starting_At : Positive) is
      Ptr  : Double_Node_Ref := With_List.Rep;
      Index: Natural := 1;
   begin
      pragma Assert(Ptr /= null, "Attempt to Share with NULL pointer");
      while Ptr /= null and then Index <= Starting_At loop
	 Ptr := Ptr.Next;
	 Index := Index + 1;
      end loop;
      Clear(Obj);
      Obj.Rep := Ptr;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share;

   procedure Share_Head(Obj : in out Double_List;With_List: in Double_List) is
   begin
      pragma Assert( With_List.Rep /= null, "Attempt to Share with NULL list");
      Clear(Obj);
      Obj.Rep := With_List.Rep;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share_Head;

   procedure Share_Foot(Obj : in out Double_List;With_List: in Double_List) is
      Ptr  : Double_Node_Ref := With_List.Rep;
      Index: Natural := 1;
   begin
      pragma Assert( Ptr /= null, "Attempt to Share_Foot with NULL list");
      Clear(obj);
      while Ptr.Next /= null loop
	 Ptr := Ptr.Next;
      end loop;
      Obj.Rep := Ptr;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share_Foot;

   procedure Swap_Tail (Obj : in out Double_List;
			With_List: in out Double_List) is
      pragma Assert( Obj.Rep /= null, "Attempt to Swap_Tail with NULL tree");
      pragma Assert( With_List.Rep = null or else With_List.Rep.Prev = null,
		     "Attempt to Swap_Tail with non-root new List");
	 Curr : Double_Node_Ref := With_List.Rep;
   begin
      Obj.Rep.Next := With_List.Rep;
      With_List.Rep.Prev := Obj.Rep;
      With_List.Rep := CUrr;
      if With_List.Rep /= null then
	 With_List.Rep.Prev := null;
      end if;
   end Swap_Tail;

   procedure Tail(Obj : in out Double_List) is
      Curr : Double_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Tail with NULL tree");
      Obj.Rep := Obj.Rep.Next;
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
      else
	 if Obj.Rep /= null then
	   Obj.Rep.Prev := null;
	 end if;
	 delete(Curr);
      end if;
   end Tail;

   procedure Predecessor(Obj : in out Double_List) is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Predecessor with NULL tree");
      if Obj.Rep.Prev = null then
	 Clear(Obj);
      else
	 Obj.Rep.Count := Obj.Rep.Count - 1;
	 Obj.Rep := Obj.Rep.Prev;
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Predecessor;

   procedure Set_Head(Obj : in out Double_List; Elem : Item) is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Set_Head with NULL tree");
      Obj.Rep.Elem := ELem;
   end Set_Head;

   procedure Set_Item(Obj : in out Double_List;Elem: Item;At_Loc: Positive) is
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      while Curr /= null and then Index <= At_Loc loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert(Curr /= null, "Attempt to Set_Item with invalid index");
      Curr.Elem := ELem;
   end Set_Item;

   function Length(Obj : Double_List) return Natural is
      Curr : Double_Node_Ref := Obj.Rep;
      Index : Natural := 0;
   begin
      while Curr /= null loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      return Index;
   end Length;

   function Is_Null(Obj : Double_List) return Boolean is
   begin
      return Obj.Rep = null;
   end Is_Null;

   function Is_Shared(Obj : Double_List) return Boolean is
   begin
     if Obj.Rep /= null then
	return Obj.Rep.Count > 1;
     else
	return False;
     end if;
   end Is_Shared;
   
   function Is_Head(Obj : Double_List) return Boolean is
   begin
     return Obj.Rep = null or else Obj.Rep.Prev = null;
   end Is_Head;

   function Head(Obj : Double_List) return Item is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Head with NULL tree");
      return Obj.Rep.Elem;
   end Head;

   function Head(Obj : Double_List) return Item_Ptr is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Head with NULL tree");
      return Obj.Rep.Elem'access;
   end Head;

   function Foot(Obj : Double_List) return Item is
      Curr : Double_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Foot with NULL tree");
      while Curr.Next /= null loop
	 Curr := Curr.Next;
      end loop;
      return Curr.Elem;
   end Foot;

   function Foot(Obj : Double_List) return Item_Ptr is
      Curr : Double_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Foot with NULL tree");
      while Curr.Next /= null loop
	 Curr := Curr.Next;
      end loop;
      return Curr.Elem'access;
   end Foot;

   function Item_At(Obj : Double_List; Loc : Natural) return Item is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Item with NULL tree");
      while Curr /= null and then Loc <= Index loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert( Curr /= null, "Attempt to get Item with Invalid Index");
      return Curr.Elem;
   end Item_At;

   function Item_At(Obj : Double_List; Loc : Natural) return Item_Ptr is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Item with NULL tree");
      while Curr /= null and then Loc <= Index loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert( Curr /= null, "Attempt to get Item with Invalid Index");
      return Curr.Elem'access;
   end Item_At;

   procedure Initialize( Obj : in out Double_List) is
   begin
     Obj.Rep := new Double_Node;
   end Initialize;

   procedure Adjust(Obj : in out Double_List) is
   begin
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Finalize(Obj : in out Double_List) is
   begin
      Clear(Obj);
   end Finalize;

end Bc.Containers.Lists.Double;
