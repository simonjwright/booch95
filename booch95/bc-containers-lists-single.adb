--  This file contains the declaration of the singly-linked list.
with Unchecked_Deallocation;
package body Bc.Containers.Lists.Single is

   use Single_Nodes;

   procedure delete is
     new Unchecked_deallocation(Single_Node, Single_Node_Ref);

   function Create(Obj : Single_List) return Single_List is
      Temp : Single_List := Single_List'(Controlled with Rep=> Obj.Rep);
   begin
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
      return Temp;
   end Create;

   function "="(L, R : Single_List) return Boolean is
   begin
      return L.Rep.all = R.Rep.all;
   end "=";

   procedure Clear(Obj : in out Single_List) is
      Curr : Single_Node_Ref := Obj.Rep;
      Ptr  : Single_Node_Ref;
   begin
      while Curr /= null loop
	 Ptr := Curr;
	 Curr := Curr.next;
	 if Ptr.Count > 1 then
	    Ptr.Count := Ptr.Count - 1;
	    exit;
	 else
	    delete (Ptr);
	 end if;
      end loop;
      Obj.Rep := null;
   end Clear;

   procedure Insert(Obj : in out Single_List; Elem : Item) is
   begin
      Obj.Rep := new Single_Node'(Elem, Obj.Rep, 1);
   end Insert;

   procedure Insert(Obj : in out Single_List; From_List : in Single_List) is
      Ptr : Single_Node_Ref := From_List.Rep;
   begin
      if Ptr /= null then
	 while Ptr.Next /= null loop
	    Ptr := Ptr.Next;
	 end loop;
      end if;
      Ptr.Next := Obj.Rep;
      Obj.Rep := From_List.Rep;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Insert;

   procedure Insert(Obj : in out Single_List; Elem : Item; Before : Positive) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
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
	 Prev.Next := new Single_Node'(Elem, Curr, 1);
      end if;
   end Insert;

   procedure Insert(Obj : in out Single_List; From_List: in out Single_List;
					      Before : Positive) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
      Ptr  : Single_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      if Ptr /= null then
   	 if Curr = null or else Before = 1 then
	   Insert(Obj, From_List);
	 else
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
	    Prev.Next := From_List.Rep;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Insert;

   procedure Append(Obj : in out Single_List; Elem : Item) is
      Curr : Single_Node_Ref := Obj.Rep;
   begin
      if Curr /= null then
	 while Curr.Next /= null loop
	    Curr := Curr.Next;
	 end loop;
      end if;
      if Curr /= null then
	 Curr.Next := new Single_Node'(Elem, null, 1);
      else
	 Obj.Rep := new Single_Node'(Elem, null, 1);
      end if;
   end Append;

   procedure Append(Obj : in out Single_List; From_List : in Single_List) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      if From_List.Rep /= null then
	 if Curr /= null then
	    while Curr.Next /= null loop
	       Curr := Curr.Next;
	    end loop;
         end if;
	 if Curr /= null then
	    Curr.Next := From_List.Rep;
	 else
	    Obj.Rep := From_List.Rep;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Append;

   procedure Append(Obj : in out Single_List; Elem : Item; After : Natural) is
      Curr : Single_Node_Ref := Obj.Rep;
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
	 Curr.Next := new Single_Node'(Elem, Curr.Next, 1);
      end if;
   end Append;

   procedure Append(Obj : in out Single_List; From_List : in Single_List;
					      After : Natural) is
      Curr : Single_Node_Ref := Obj.Rep;
      Ptr  : Single_Node_Ref := From_List.Rep;
      Index: Natural := 1;
   begin
      if Ptr /= null then
	 if Curr = null then
	    Append(Obj, From_List);
	 else
	    while Curr /= null and then Index <= After loop
	       Curr := Curr.Next;
	       Index := Index + 1;
	    end loop;
	    pragma Assert( Curr /= null, "Attempt to Insert with invalid Index");
	    while Ptr.Next /= null loop
	       Ptr := Ptr.Next;
	    end loop;
	    Ptr.Next := Curr.Next;
	    Curr.Next := From_List.Rep;
	    From_List.Rep.Count := From_List.Rep.Count + 1;
	 end if;
      end if;
   end Append;

   procedure Remove(Obj : in out Single_List; From : Natural) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
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
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
	 Obj.Rep := Curr.Next;
	 Curr.Next := null;
      else
	 delete (Curr);
      end if;
   end Remove;

   procedure Purge (Obj : in out Single_LIst; From : Natural) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
      Ptr  : Single_Node_Ref;
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
	 Ptr := Curr;
	 Curr := Curr.Next;
	 if Ptr.Count > 1 then
	    Ptr.Count := Ptr.Count - 1;
	    exit;
	 else
	    delete (Ptr);
	 end if;
      end loop;
   end Purge;

   procedure Purge (Obj : in out Single_List; From: Natural;Count:Positive) is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
      Ptr  : Single_Node_Ref;
      Index: Natural := 1;
      Cut  : Boolean := True;
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
	       Ptr.Count := Ptr.Count - 1;
	       Cut := False;
	    else
	       delete(Ptr);
	    end if;
	 end if;
	 Index := Index + 1;
      end loop;
      Ptr.Next := null;
      if CUrr /= null then
	 if Prev /= null then
	    Prev.Next := Curr;
	 else
	    Obj.Rep := Curr;
	 end if;
      end if;
   end Purge;

   procedure Preserve(Obj : in out Single_List; From : Natural) is
      Temp : Single_List;
   begin
      Share(Temp, Obj, From);
      Share_Head(Obj, Temp);
   end Preserve;

   procedure Preserve(Obj: in out Single_List; From: Natural;Count:Positive) is
   begin
      Preserve(Obj, From);
      if Length(obj) >= Count then
	 Purge(Obj, Count);
      end if;
   end Preserve;

   procedure Share (Obj : in out Single_List; With_List: Single_List;
					      Starting_At : Positive) is
      Ptr  : Single_Node_Ref := With_List.Rep;
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

   procedure Share_Head(Obj : in out Single_List;With_List: in Single_List) is
   begin
      pragma Assert( With_List.Rep /= null, "Attempt to Share with NULL list");
      Clear(Obj);
      Obj.Rep := With_List.Rep;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share_Head;

   procedure Share_Foot(Obj : in out Single_List;With_List: in Single_List) is
      Ptr  : Single_Node_Ref := With_List.Rep;
   begin
      pragma Assert( Ptr /= null, "Attempt to Share_Foot with NULL list");
      Clear(obj);
      while Ptr.Next /= null loop
	 Ptr := Ptr.Next;
      end loop;
      Obj.Rep := Ptr;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share_Foot;

   procedure Swap_Tail (Obj : in out Single_List;
			With_List: in out Single_List) is
      pragma Assert( Obj.Rep /= null, "Attempt to Swap_Tail with NULL tree");
      Curr : Single_Node_Ref := With_List.Rep;
   begin
      Obj.Rep.Next := With_List.Rep;
      With_List.Rep := CUrr;
   end Swap_Tail;

   procedure Tail(Obj : in out Single_List) is
      Curr : Single_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Tail with NULL tree");
      Obj.Rep := Obj.Rep.Next;
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
      else
	 delete(Curr);
      end if;
   end Tail;

   procedure Set_Head(Obj : in out Single_List; Elem : Item) is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Set_Head with NULL tree");
      Obj.Rep.Elem := ELem;
   end Set_Head;

   procedure Set_Item(Obj : in out Single_List;Elem: Item;At_Loc: Positive) is
      Curr : Single_Node_Ref := Obj.Rep;
      Index: Natural := 1;
   begin
      while Curr /= null and then Index <= At_Loc loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert(Curr /= null, "Attempt to Set_Item with invalid index");
      Curr.Elem := ELem;
   end Set_Item;

   function Length(Obj : Single_List) return Natural is
      Curr : Single_Node_Ref := Obj.Rep;
      Index : Natural := 0;
   begin
      while Curr /= null loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      return Index;
   end Length;

   function Is_Null(Obj : Single_List) return Boolean is
   begin
      return Obj.Rep = null;
   end Is_Null;

   function Is_Shared(Obj : Single_List) return Boolean is
   begin
     if Obj.Rep /= null then
	return Obj.Rep.Count > 1;
     else
	return False;
     end if;
   end Is_Shared;

   function Head(Obj : Single_List) return Item is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Head with NULL tree");
      return Obj.Rep.Elem;
   end Head;

   function Head(Obj : Single_List) return Item_Ptr is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Head with NULL tree");
      return Obj.Rep.Elem'access;
   end Head;

   function Foot(Obj : Single_List) return Item is
      Curr : Single_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Foot with NULL tree");
      while Curr.Next /= null loop
	 Curr := Curr.Next;
      end loop;
      return Curr.Elem;
   end Foot;

   function Foot(Obj : Single_List) return Item_Ptr is
      Curr : Single_Node_Ref := Obj.Rep;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Foot with NULL tree");
      while Curr.Next /= null loop
	 Curr := Curr.Next;
      end loop;
      return Curr.Elem'access;
   end Foot;

   function Item_At(Obj : Single_List; Loc : Natural) return Item is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
      Index: Positive := 1;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Item with NULL tree");
      while Curr /= null and then Loc <= Index loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert( Curr /= null, "Attempt to get Item with Invalid Index");
      return Curr.Elem;
   end Item_At;

   function Item_At(Obj : Single_List; Loc : Natural ) return Item_Ptr is
      Prev : Single_Node_Ref;
      Curr : Single_Node_Ref := Obj.Rep;
      Index: Positive := 1;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to get Item with NULL tree");
      while Curr /= null and then Loc <= Index loop
	 Curr := Curr.Next;
	 Index := Index + 1;
      end loop;
      pragma Assert( Curr /= null, "Attempt to get Item with Invalid Index");
      return Curr.Elem'access;
   end Item_At;

   procedure Initialize( Obj : in out Single_List) is
   begin
     Obj.Rep := new Single_Node;
   end Initialize;

   procedure Adjust(Obj : in out Single_List) is
   begin
      if Obj.Rep /= null then
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Finalize(Obj : in out Single_List) is
   begin
      Clear(Obj);
   end Finalize;

end Bc.Containers.Lists.Single;
