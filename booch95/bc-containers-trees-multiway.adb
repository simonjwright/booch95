--  The C++ Booch Components (Version 2.3)
--  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--
--  BCBTree.h
--
--  This file contains the declaration of the binary tree.
with Unchecked_Deallocation;
package body BC.Containers.Trees.Multiway is

   procedure Delete is new Unchecked_Deallocation(Multiway_Node,
						  Multiway_Node_Ref);

   procedure Mend(Obj : in out Multiway_Node_Ref) is
   begin
      if Obj.Child /= null then
	 Obj.Child.Parent := Obj;
      end if;
   end Mend;

  procedure Purge(Curr : in out Multiway_Node_Ref) is
  begin
     if Curr /= null then
	if Curr.Count > 1 then
	   Curr.Count := Curr.Count -1;
	else
	   declare
	      Ptr : Multiway_Node_Ref := Curr.Child;
	      Next: Multiway_Node_Ref;
	   begin
	      while Ptr /= null loop
		 Next := Ptr.Sibling;
		 Ptr.Sibling := null;
		 Purge(Ptr);
		 if Ptr /= null then
		    Ptr.Parent := null;
		 end if;
		 Ptr := Next;
	      end loop;
	      delete(Curr);
	   end;
        end if;
     end if;
   end Purge;

   function "="(Left, Right : Mway_Tree) return Boolean is
   begin
      return Left.Rep = Right.Rep;
   end "=";

   procedure Clear(Obj : in out Mway_Tree) is
   begin
      Purge(Obj.Rep);
      Delete(Obj.Rep);
   end Clear;

   procedure Insert(Obj : in out Mway_Tree; Elem : in Item) is
   begin
      pragma Assert( Obj.Rep = null or else Obj.Rep.Parent = null,
		     "Attempt to Insert at other than root");
      Obj.Rep := new Multiway_Node'(Elem, null, Obj.Rep, null,1);
      Mend(Obj.Rep);
   end Insert;

   procedure Append(Obj : in out Mway_Tree; Elem : in Item) is
   begin
      if Obj.Rep = null then
	 Obj.Rep := new Multiway_Node'(Elem, null, Obj.Rep, null,1);
	 Mend(Obj.Rep);
      else
	 Obj.Rep.Child := 
	  new Multiway_Node' (Elem,Obj.Rep,null,Obj.Rep.Child,1);
	 Mend(Obj.Rep.Child);
      end if;
   end Append;

   procedure Append(Obj : in out Mway_Tree; Elem : in Item; After : Natural) is
   begin
      if Obj.Rep = null then
	 Obj.Rep := new Multiway_Node'(Elem, null, Obj.Rep, null,1);
	 Mend(Obj.Rep);
      else 
	declare
	   Curr  : Multiway_Node_Ref := Obj.Rep.Child;
	   I     : Natural := 1;
	begin
	   if Curr = null then
	      Obj.Rep.Child :=
	       new Multiway_Node'(Elem, Obj.Rep, null, Obj.Rep.Child,1 );
	      Mend(Obj.Rep.Child);
	   else
	      while Curr /= null and then I < After loop
		 Curr := Curr.Sibling; I := I + 1;
	      end loop;
	      pragma Assert (Curr /= null, "Illegal 'After' value for Append");
	      Curr.Sibling :=
	       new Multiway_Node'(Elem, Obj.Rep,null,Curr.Sibling,1);
	      Mend(Curr.Sibling);
	   end if;
	end;
      end if;
   end Append;

   procedure Append(Obj : in out Mway_Tree; From_Tree : in out Mway_Tree) is
   begin
      if From_Tree.Rep = null then
	 return;
      end if;
      pragma Assert (From_Tree.Rep.Parent = null,
		     "From_Tree is not root for Append");
      if Obj.Rep = null then
	 Obj.Rep := From_Tree.Rep;
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      else
	 From_Tree.Rep.Sibling := Obj.Rep.Child;
	 From_Tree.Rep.Parent := Obj.Rep;
	 From_Tree.Rep.Count := From_Tree.Rep.Count + 1;
	 Obj.Rep.Child := From_Tree.Rep;
      end if;
   end Append;

   procedure Remove(Obj : in out Mway_Tree; Index : Natural) is
   begin
      pragma Assert(Obj.Rep /= null, "Attempt to Remove from a NULL tree");
      declare
	 I    : Natural := 1;
	 Prev : Multiway_Node_Ref;
	 Curr : Multiway_Node_Ref := Obj.Rep.Child;
      begin
	 while Curr /= null and then I < Index loop
	    Prev := Curr;
	    Curr := Curr.Sibling;
	    I := I + 1;
	 end loop;
	 pragma Assert (Curr /= null, "Illegal 'Index' for Remove");
	 if Prev = null then
	    Obj.Rep.Child := Curr.Sibling;
	 else
	    Prev.Sibling := Curr.Sibling;
	 end if;
	 Curr.Parent := null;
	 Curr.Sibling := null;
	 Purge(Curr);
      end;
   end Remove;

   procedure Share(Obj : in out Mway_Tree;  
		   Share_With : in Mway_Tree;
		   Child : Natural) is
      Ptr : Multiway_Node_Ref := Share_With.Rep;
      I   : Natural := 1;
   begin
      pragma Assert( Ptr /= null, "Attempt to Share with a NULL tree");
      Ptr := Ptr.Child;
      while Ptr /= null and then I < Child loop
	 Ptr := Ptr.Sibling; 
	 I := I + 1;
      end loop;
      pragma Assert (Ptr /= null, "Illegal 'Index' for Share");
      Clear(Obj);
      Obj.Rep := Ptr;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share;
      
   procedure Swap_Child(Obj : in out Mway_Tree; 
			Swap_WIth : in out Mway_Tree; 
			Child : in Natural) is
      Prev : Multiway_Node_Ref;
      Curr : Multiway_Node_Ref := Obj.Rep;
      I    : Natural := 1;
   begin
      pragma Assert(Obj.Rep /= null, "Attempt to Swap with NULL tree");
      pragma Assert(Swap_With.Rep = null and then Swap_With.Rep.Parent = null,
		    "Attempt to Swap_Child with NULL tree");
      Curr := Curr.Child;
      while Curr /= null and then I < Child loop
	 Prev := Curr;
	 Curr := Curr.Sibling;
	 I := I + 1;
      end loop;
      pragma Assert (Curr /= null, "Illegal 'Index' for Swap_Child");
      Swap_With.Rep.Sibling := Curr.Sibling;
      if Prev = null then
	 Obj.Rep.Child := Swap_With.Rep;
      else
	 Prev.Sibling := Swap_With.Rep;
      end if;
      if Swap_With.Rep /= null then
	 Swap_With.Rep.Parent := Obj.Rep;
      end if;
      Swap_With.Rep := Curr;
      Swap_With.Rep.Sibling := null;
      Swap_With.Rep.Parent := null;
   end Swap_Child;


   procedure Child(Obj : in out Mway_Tree; Child : in Natural) is
      Curr : Multiway_Node_Ref := Obj.Rep;
      I : Natural := 1;
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to move in a NULL direction");
      Curr := Curr.Child;
      while Curr /= null and then I < Child loop
	 Curr := Curr.Sibling;
	 I := I + 1;
      end loop;
      pragma Assert (Curr /= null, "Illegal value for Child");
      Curr.Count := Curr.Count + 1;
      Purge(Obj.Rep);
      Obj.Rep := Curr;
   end Child;

   procedure Parent(Obj : in out Mway_Tree) is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to move in a NULL direction");
      if Obj.Rep.Parent = null then
	 Clear(Obj);
      else
	 Obj.Rep.Count := Obj.Rep.Count -1 ;
	 Obj.Rep := Obj.Rep.Parent;
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Parent;

   procedure Set_Item(Obj : in out Mway_Tree; Elem : in Item) is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Set_Item on NULL tree");
      Obj.Rep.Elem := Elem;
   end Set_Item;

   function Arity (Obj : Mway_Tree) return Natural is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to Set_Item on NULL tree");
      declare
	 Count : Natural := 0;
	 Ptr : Multiway_Node_Ref := Obj.Rep.Child;
      begin
	 while Ptr /= null loop
	    Count := Count + 1;
	    Ptr := Ptr.Sibling;
	 end loop;
	 return Count;
      end;
   end Arity;

   function Has_Children(Obj : in Mway_Tree) return boolean is
   begin
      return Obj.Rep /= null and then Obj.Rep.Child /= null;
   end Has_Children;

   function Is_Null(Obj : in Mway_Tree) return boolean is
   begin
     return Obj.Rep = null;
   end Is_Null;

   function Is_Shared(Obj : in Mway_Tree) return boolean is
   begin
     return Obj.Rep /= null and then Obj.Rep.Count > 1;
   end Is_Shared;

   function Is_Root(Obj : in Mway_Tree) return boolean is
   begin
     return Obj.Rep /= null and then Obj.Rep.Parent = null;
   end Is_Root;

   function Item_At(Obj : in Mway_Tree) return Item is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to move in a NULL direction");
      return Obj.Rep.Elem;
   end Item_At;

   function Item_At(Obj : in Mway_Tree) return Item_Ptr is
   begin
      pragma Assert( Obj.Rep /= null, "Attempt to move in a NULL direction");
      return Obj.Rep.Elem'access;
   end Item_At;

   procedure Initialize(Obj : in out Mway_Tree) is
   begin
      null;
   end Initialize;

   procedure Finalize(Obj : in out Mway_Tree) is
   begin
      Clear(Obj);
   end Finalize;

   procedure Adjust(Obj : in out Mway_Tree) is
   begin
      if Obj.Rep /= null then 
	 Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Adjust;

end BC.Containers.Trees.Multiway;
