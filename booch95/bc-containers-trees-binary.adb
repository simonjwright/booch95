with Unchecked_Deallocation;
package body BC.Containers.Trees.Binary is

   use Ada.Finalization;
   
   procedure Delete is 
     new Unchecked_Deallocation(Binary_Node, Binary_Node_Ref);

   function Create(From : Bin_Tree) return Bin_Tree is
      Temp : Bin_Tree := (Controlled with Rep => From.Rep);
   begin
      if From.Rep /= null then
	 Temp.Rep.Count := Temp.Rep.Count + 1;
      end if;
      return Temp;
   end Create;

   function "="(Left, Right : Bin_Tree) return Boolean is
   begin
      return Left.Rep.all = Right.Rep.all;
   end "=";

   procedure Clear(Obj : in out Bin_Tree) is
   begin
      Purge(Obj.Rep);
      delete(Obj.Rep); -- Needs better management
   end Clear;

   procedure Insert(Obj : in out Bin_Tree; Elem : in Item; 
		    Child : in Child_Branch) is
   begin
      pragma Assert( Obj.Rep /= null or else Obj.Rep.Parent = null,
		     "Tried to insert when not at root");
      if Child = Left then
	 Obj.Rep := new Binary_Node'(Elem=> Elem, Parent=>null, Left=>Obj.Rep,
				     Right=>null, Count=>1);
      else
	 Obj.Rep := new Binary_Node'(Elem=> Elem, Parent=>null, Left=>null,
				     Right=>Obj.Rep, Count=>1);
      end if;
   end Insert;

   procedure Append(Obj : in out Bin_Tree; Elem : in Item; 
		    Child : in Child_Branch; After : in Child_Branch) is
   begin
      if Obj.Rep = null then
	 Obj.Rep := new Binary_Node'(Elem=> Elem, Parent=>null, Left=>null,
				     Right=>null, Count=>1);
      else
	 if After = Left then
	    if Child = Left then
	       Obj.Rep.Left := 
		new Binary_Node'(Elem=> Elem, Parent=> Obj.Rep,
				Left=> Obj.Rep.Left, Right=> null, Count=>1 );
	    else
	       Obj.Rep.Left := 
		new Binary_Node'(Elem=> Elem, Parent=> Obj.Rep, Left=> null,
				Right=> Obj.Rep.Left, COunt=>1 );
	    end if;
	 else
	    if Child = Left then
	       Obj.Rep.Right := 
		new Binary_Node'(Elem=> Elem, Parent=> Obj.Rep, 
				Left=> Obj.Rep.Right, RIght=> null, COunt=>1 );
	    else
	       Obj.Rep.Left := 
		new Binary_Node'(Elem=> Elem, Parent=> Obj.Rep, Left=>null,
				Right=> Obj.Rep.Right, Count=>1 );
	    end if;
	 end if;
      end if;
   end Append;

   procedure Remove(Obj : in out Bin_Tree; Child : in Child_Branch) is
   begin
      pragma Assert(Obj.Rep = null, "Tried to Remove from null tree");
      if Child = Left then
	 Purge(Obj.Rep.Left);
	 Obj.Rep.Left := null;
      else
	 Purge(Obj.Rep.Right);
	 Obj.Rep.Right := null;
      end if;
   end Remove;

   procedure Share(Obj : in out Bin_Tree;  
		   Share_With : in Bin_Tree;
		   Child : in Child_Branch := Right) is
      Temp : Binary_Node_Ref :=  Share_With.Rep;
   begin
      pragma Assert(Obj.Rep /= null, "Attempt to Share with null pointer");
      if Child = Left then
	 Temp := Share_With.Rep.Left;
      else
	 Temp := Share_With.Rep.Right;
      end if;
      Clear(obj);
      Obj.Rep := Temp;
      Obj.Rep.Count := Obj.Rep.Count + 1;
   end Share;

   procedure Swap_Child(Obj : in out Bin_Tree; 
			Swap_With : in out Bin_Tree;
			Child : in Child_Branch) is
      Curr : Binary_Node_Ref;
   begin
      pragma Assert(Obj.Rep = null, "Attempt to Swap with null Tree");
      pragma Assert(Swap_With.Rep = null or else Swap_With.Rep.Parent /= null,
		    "Attempt to Swap with non root Tree");
      if Child = Left then
	 Curr := Obj.Rep.Left;
	 Obj.Rep.Left := Swap_With.Rep;
      else
	 Curr := Obj.Rep.Right;
	 Obj.Rep.Right := Swap_With.Rep;
      end if;
      if Swap_With.Rep /= null then
	 Swap_With.Rep.Parent := Obj.Rep;
      end if;
      Swap_With.Rep := Curr;
      if Swap_With.Rep /= null then
	 Swap_With.Rep.Parent := null;
      end if;
   end Swap_Child;

   procedure Child(Obj : in out Bin_Tree; Child : in Child_Branch) is
   begin
      if Child = Left then
	 Left_Child(Obj);
      else
	 Right_Child(Obj);
      end if;
   end Child;
  
   procedure Left_Child(Obj : in out Bin_Tree) is
      Curr : Binary_Node_Ref;
   begin
      pragma Assert(Obj.Rep = null, "Attempt to Left_Child a null tree");
      Curr := Obj.Rep;
      Obj.Rep := Obj.Rep.Left;
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
	 if Obj.Rep /= null then
	    Obj.Rep.Count := Obj.Rep.Count + 1;
	 end if;
      else
	 if Obj.Rep /= null then
	    Obj.Rep.Parent := null;
	 end if;
	 if Curr.Right /= null then
	    Curr.RIght.Parent := null;
	 end if;
	 delete(Curr);
       end if;
   end Left_Child;

   procedure Right_Child(Obj : in out Bin_Tree) is
      Curr : Binary_Node_Ref;
   begin
      pragma Assert(Obj.Rep = null, "Attempt to Right_Child a null tree");
      Curr := Obj.Rep;
      Obj.Rep := Obj.Rep.Right;
      if Curr.Count > 1 then
	 Curr.Count := Curr.Count - 1;
	 if Obj.Rep /= null then
	    Obj.Rep.Count := Obj.Rep.Count + 1;
	 end if;
      else
	 if Obj.Rep /= null then
	    Obj.Rep.Parent := null;
	 end if;
	 if Curr.Left /= null then
	    Curr.Left.Parent := null;
	 end if;
	 delete(Curr);
       end if;
   end Right_Child;

   procedure Parent(Obj : in out Bin_Tree) is
   begin
      pragma Assert(Obj.Rep = null, "Attempt to Parent a null tree");
      if Obj.Rep.Parent = null then
	 Clear(Obj);
      else
	 Obj.Rep.Count := Obj.Rep.Count - 1;
	 Obj.Rep := Obj.Rep.Parent;
	 if Obj.Rep /= null then
	    Obj.Rep.Count := Obj.Rep.Count + 1;
	 end if;
      end if;
   end Parent;

   procedure Set_Item(Obj : in out Bin_Tree; Elem : in Item) is
   begin
      pragma Assert(Obj.Rep = null, "Attempt to Set_Item on null tree");
      Obj.Rep.Elem := Elem;
   end Set_Item;

   function Has_Children(Obj : in Bin_Tree) return boolean is
   begin
      return (Obj.Rep /= null and then 
	      (Obj.Rep.Left /= null or else Obj.Rep.Right /= null));
   end Has_Children;

   function Is_Null(Obj : in Bin_Tree) return boolean is
   begin
      return Obj.Rep = null;
   end Is_Null;

   function Is_Shared(Obj : in Bin_Tree) return boolean is
   begin
      return Obj.Rep /= null and then Obj.Rep.Count > 1;
   end Is_Shared;

   function Is_Root(Obj : in Bin_Tree) return boolean is
   begin
      return Obj.Rep /= null and then Obj.Rep.Parent = null;
   end Is_Root;

   function Item_At(Obj : in Bin_Tree) return Item is
   begin
      pragma Assert(Obj.Rep /= null, "Attempt to take Item_At with null tree");
      return Obj.Rep.Elem;
   end Item_At;

   procedure Purge(Node : in out Binary_Node_Ref) is
   begin
      if Node /= null then
	 if Node.Count > 1 then
	    Node.Count := Node.Count - 1;
	 else
	    Purge(Node.Left);
	    if Node.Left /= null then
	       Node.Left.Parent := null;
	    end if;
	    Purge(Node.Right);
	    if Node.Right /= null then
	       Node.Right.Parent := null;
	    end if;
	    delete(Node);
	 end if;
      end if;
   end Purge;

end BC.Containers.Trees.Binary;
