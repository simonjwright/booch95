-- Copyright (C) 1994-1999 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

package body BC.Containers.Trees.AVL is

  -- Supporting subprograms

  procedure Purge (Node : in out Nodes.AVL_Node_Ref) is
    use type Nodes.AVL_Node_Ref;
  begin
    if Node /= null then
      Purge (Node.Left);
      Purge (Node.Right);
      Nodes.Delete (Node);
    end if;
  end Purge;

  procedure Search_Insert (Obj : in out AVL_Tree;
                           Element : Item;
                           Node : in out Nodes.AVL_Node_Ref;
                           Increased : in out Boolean;
                           Inserted : out Boolean) is
    P1, P2 : Nodes.AVL_Node_Ref;
    use type Nodes.AVL_Node_Ref;
    use type Nodes.Node_Balance;
  begin
    Inserted := True;
    if Node = null then
      Node := new Nodes.AVL_Node'(Element => Element,
                                  Left => null,
                                  Right => null,
                                  Balance => Nodes.Middle);
      Increased := True;
    elsif Element < Node.Element then
      Search_Insert (Obj, Element, Node.Left, Increased, Inserted);
      if Increased then
        case Node.Balance is
          when Nodes.Right =>
            Node.Balance := Nodes.Middle;
            Increased := False;
          when Nodes.Middle =>
            Node.Balance := Nodes.Left;
          when Nodes.Left =>
            P1 := Node.Left;
            if P1.Balance = Nodes.Left then
              Node.Left := P1.Right;
              P1.Right := Node;
              Node.Balance := Nodes.Middle;
              Node := P1;
            else
              P2 := P1.Right;
              P1.Right := P2.Left;
              P2.Left := P1;
              Node.Left := P2.Right;
              P2.Right := Node;
              if P2.Balance = Nodes.Left then
                Node.Balance := Nodes.Right;
              else
                Node.Balance := Nodes.Middle;
              end if;
              if P2.Balance = Nodes.Right then
                P1.Balance := Nodes.Left;
              else
                P1.Balance := Nodes.Middle;
              end if;
              Node := P2;
            end if;
            Node.Balance := Nodes.Middle;
            Increased := False;
        end case;
      end if;
    elsif Node.Element < Element then
      Search_Insert (Obj, Element, Node.Right, Increased, Inserted);
      if Increased then
        case Node.Balance is
          when Nodes.Left =>
            Node.Balance := Nodes.Middle;
            Increased := False;
          when Nodes.Middle =>
            Node.Balance := Nodes.Right;
          when Nodes.Right =>
            P1 := Node.Right;
            if P1.Balance = Nodes.Right then
              Node.Right := P1.Left;
              P1.Left := Node;
              Node.Balance := Nodes.Middle;
              Node := P1;
            else
              P2 := P1.Left;
              P1.Left := P2.Right;
              P2.Right := P1;
              Node.Right := P2.Left;
              P2.Left := Node;
              if P2.Balance = Nodes.Right then
                Node.Balance := Nodes.Left;
              else
                Node.Balance := Nodes.Middle;
              end if;
              if P2.Balance = Nodes.Left then
                P1.Balance := Nodes.Right;
              else
                P1.Balance := Nodes.Middle;
              end if;
              Node := P2;
            end if;
            Node.Balance := Nodes.Middle;
            Increased := False;
        end case;
      end if;
    else
      Inserted := False;
    end if;
  end Search_Insert;

  procedure Balance_Left (Node : in out Nodes.AVL_Node_Ref;
                          Decreased : in out Boolean) is
    P1, P2 : Nodes.AVL_Node_Ref;
    Balance1, Balance2 : Nodes.Node_Balance;
    use type Nodes.Node_Balance;
  begin
    case Node.Balance is
      when Nodes.Left =>
        Node.Balance := Nodes.Middle;
      when Nodes.Middle =>
        Node.Balance := Nodes.Right;
        Decreased := False;
      when Nodes.Right =>
        P1 := Node.Right;
        Balance1 := P1.Balance;
        if Balance1 >= Nodes.Middle then
          Node.Right := P1.Left;
          P1.Left := Node;
          if Balance1 = Nodes.Middle then
            Node.Balance := Nodes.Right;
            P1.Balance := Nodes.Left;
            Decreased := False;
          else
            Node.Balance := Nodes.Middle;
            P1.Balance := Nodes.Middle;
          end if;
          Node := P1;
        else
          P2 := P1.Left;
          Balance2 := P2.Balance;
          P1.Left := P2.Right;
          P2.Right := P1;
          Node.Right := P2.Left;
          P2.Left := Node;
          if Balance2 = Nodes.Right then
            Node.Balance := Nodes.Left;
          else
            Node.Balance := Nodes.Middle;
          end if;
          if Balance2 = Nodes.Left then
            P1.Balance := Nodes.Right;
          else
            P1.Balance := Nodes.Middle;
          end if;
          Node := P2;
          P2.Balance := Nodes.Middle;
        end if;
    end case;
  end Balance_Left;

  procedure Balance_Right (Node : in out Nodes.AVL_Node_Ref;
                           Decreased : in out Boolean)  is
    P1, P2 : Nodes.AVL_Node_Ref;
    Balance1, Balance2 : Nodes.Node_Balance;
    use type Nodes.Node_Balance;
  begin
    case Node.Balance is
      when Nodes.Right =>
        Node.Balance := Nodes.Middle;
      when Nodes.Middle =>
        Node.Balance := Nodes.Left;
        Decreased := False;
      when Nodes.Left =>
        P1 := Node.Left;
        Balance1 := P1.Balance;
        if Balance1 <= Nodes.Middle then
          Node.Left := P1.Right;
          P1.Right := Node;
          if Balance1 = Nodes.Middle then
            Node.Balance := Nodes.Left;
            P1.Balance := Nodes.Right;
            Decreased := False;
          else
            Node.Balance := Nodes.Middle;
            P1.Balance := Nodes.Middle;
          end if;
          Node := P1;
        else
          P2 := P1.Right;
          Balance2 := P2.Balance;
          P1.Right := P2.Left;
          P2.Left := P1;
          Node.Left := P2.Right;
          P2.Right := Node;
          if Balance2 = Nodes.Left then
            Node.Balance := Nodes.Right;
          else
            Node.Balance := Nodes.Middle;
          end if;
          if Balance2 = Nodes.Right then
            P1.Balance := Nodes.Left;
          else
            P1.Balance := Nodes.Middle;
          end if;
          Node := P2;
          P2.Balance := Nodes.Middle;
        end if;
    end case;
  end Balance_Right;

  procedure Delete (T1, T2 : in out Nodes.AVL_Node_Ref;
                    Decreased : in out Boolean) is
    use type Nodes.AVL_Node_Ref;
  begin
    if T2.Right /= null then
      Delete (T1, T2.Right, Decreased);
      if T2.Left = null and then T2.Right = null then
        T2.Balance := Nodes.Middle;
      end if;
      if Decreased then
        Balance_Right (T2, Decreased);
      end if;
    else
      T1.Element := T2.Element;
      T1 := T2;
      T2 := T2.Left;
      Decreased := True;
    end if;
  end Delete;

  procedure Search_Delete (Obj: in out AVL_Tree;
                           Element : Item;
                           Node : in out Nodes.AVL_Node_Ref;
                           Decreased : in out Boolean;
                           Deleted : out Boolean) is
    Q : Nodes.AVL_Node_Ref;
    use type Nodes.AVL_Node_Ref;
  begin
    Deleted := False;
    if Node /= null then
      if Element < Node.Element then
        Search_Delete (Obj, Element, Node.Left, Decreased, Deleted);
        if Decreased then
          Balance_Left (Node, Decreased);
        end if;
      elsif Node.Element < Element then
        Search_Delete (Obj, Element, Node.Right, Decreased, Deleted);
        if Decreased then
          Balance_Right (Node, Decreased);
        end if;
      else
        Q := Node;
        Deleted := True;
        if Q.Right = null then
          Node := Q.Left;
          Decreased := True;
        elsif Q.Left = null then
          Node := Q.Right;
          Decreased := True;
        else
          Delete (Q, Q.Left, Decreased);
          if Decreased then
            Balance_Left (Node, Decreased);
          end if;
        end if;
        Nodes.Delete (Q);
      end if;
    end if;
  end Search_Delete;

  function Search (Obj: AVL_Tree;
                   Element: Item;
                   Node: Nodes.AVL_Node_Ref) return Boolean is
    use type Nodes.AVL_Node_Ref;
  begin
    if Node /= null then
      if Node.Element = Element then
        return True;
      elsif Element < Node.Element then
        return Search (Obj, Element, Node.Left);
      else
        return Search (Obj, Element, Node.Right);
      end if;
    else
      return False;
    end if;
  end Search;

  --end supporting functions

  function "=" (L, R : AVL_Tree) return Boolean is
    -- Once we know that the sizes are the same, we only need to check
    -- that all members of L are in R, because we don't allow
    -- duplicate members.
    procedure Check_In_Right (Elem : in Item; Found : out Boolean);
    procedure Compare is new Visit (Apply => Check_In_Right);
    Are_Equal : Boolean := True;
    procedure Check_In_Right (Elem : in Item; Found : out Boolean) is
    begin
      Found := Is_Member (R, Elem); -- to terminate early
      if not Found then
        Are_Equal := False;
      end if;
    end Check_In_Right;
  begin
    if L.Size /= R.Size then
      return False;
    end if;
    Compare (Over_The_Tree => L);
    return Are_Equal;
  end "=";

  procedure Clear (Obj : in out AVL_Tree) is
  begin
    Purge (Obj.Rep);
    Obj.Size := 0;
  end Clear;

  procedure Insert (Obj : in out AVL_Tree;
                    Element : Item;
                    Not_Found : out Boolean) is
    Increased : Boolean := False;
    Result : Boolean;
  begin
    Search_Insert (Obj, Element, Obj.Rep, Increased, Result);
    if Result then
      Obj.Size := Obj.Size + 1;
      Not_Found := True;
    else
      Not_Found := False;
    end if;
  end Insert;

  procedure Delete
     (Obj : in out AVL_Tree; Element : Item; Found : out Boolean) is
    Decreased : Boolean := False;
    Result : Boolean;
  begin
    Search_Delete (Obj, Element, Obj.Rep, Decreased, Result);
    if Result then
      Obj.Size := Obj.Size - 1;
      Found := True;
    else
      Found := False;
    end if;
  end Delete;

  function Extent (Obj : in AVL_Tree) return Natural is
  begin
    return Obj.Size;
  end Extent;

  function Is_Null (Obj : in AVL_Tree) return Boolean is
    use type Nodes.AVL_Node_Ref;
  begin
    return Obj.Rep = null;
  end Is_Null;

  function Is_Member (Obj : in AVL_Tree; Element : Item) return Boolean is
  begin
    return Search (Obj, Element, Obj.Rep);
  end Is_Member;

--    function Item_Of (Node : AVL_Node_Ref;
--                      Element : Item) return Item_Ptr is
--      use type Nodes.AVL_Node_Ref;
--    begin
--      if Node /= null then
--        if Node.Element = Element then
--          return Node.Element'access;
--        elsif Less (Element, Node.Element) then
--          return ItemOf (Node.Left, Element, Less);
--        else
--          return ItemOf (Node.Right, Element ,Less);
--        end if;
--      else
--        return null;
--      end if;
--    end ItemOf;

  procedure Access_Actual_Item (In_The_Tree : AVL_Tree;
                                Elem : Item;
                                Found : out Boolean) is
    procedure Access_Actual_Item (Node : Nodes.AVL_Node_Ref) is
      use type Nodes.AVL_Node_Ref;
    begin
      if Node /= null then
        if Node.Element = Elem then
          Found := True;
          Apply (Node.Element);
        elsif Elem < Node.Element then
          Access_Actual_Item (Node.Left);
        else
          Access_Actual_Item (Node.Right);
        end if;
      end if;
    end Access_Actual_Item;
  begin
    Found := False;
    Access_Actual_Item (In_The_Tree.Rep);
  end Access_Actual_Item;

--    function Traverse (Node : AVL_Node_Ref;
--                      Iter_Func : Iteration_Function) return Boolean is
--      Temp : AVL_Node_Ref;
--    begin
--      if Node /= null then
--        Temp := Node.Left;
--        if not Traverse (Temp, Iter_Func) then
--          return False;
--        end if;
--        if not Iter_Func (Temp.Element) then
--          return False;
--        end if;
--        Temp := Node.Right;
--        if not Traverse (Temp, Iter_Func) then
--          return False;
--        end if;
--      end if;
--      return True;
--    end Traverse;

  procedure Visit (Over_The_Tree : AVL_Tree) is
    Continue : Boolean := True;
    use type Nodes.AVL_Node_Ref;
    procedure Visit (Node : Nodes.AVL_Node_Ref) is
      use type Nodes.AVL_Node_Ref;
    begin
      if Node /= null then
        Visit (Node.Left);
        if not Continue then
          return;
        end if;
        Apply (Node.Element, Continue);
        if not Continue then
          return;
        end if;
        Visit (Node.Right);
      end if;
    end Visit;
  begin
    Visit (Over_The_Tree.Rep);
  end Visit;

  procedure Modify (Over_The_Tree : AVL_Tree) is
    Continue : Boolean := True;
    use type Nodes.AVL_Node_Ref;
    procedure Modify (Node : Nodes.AVL_Node_Ref) is
      use type Nodes.AVL_Node_Ref;
    begin
      if Node /= null then
        Modify (Node.Left);
        if not Continue then
          return;
        end if;
        Apply (Node.Element, Continue);
        if not Continue then
          return;
        end if;
        Modify (Node.Right);
      end if;
    end Modify;
  begin
    Modify (Over_The_Tree.Rep);
  end Modify;

  procedure Initialize (Obj : in out AVL_Tree) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out AVL_Tree) is
    New_Tree : AVL_Tree;
    procedure Add (Elem : in Item; OK : out Boolean);
    procedure Copy is new Visit (Apply => Add);
    procedure Add (Elem : in Item; OK : out Boolean) is
      Inserted : Boolean;
    begin
      Insert (Obj => New_Tree, Element => Elem, Not_Found => Inserted);
      -- XXX should test Inserted?
      OK := True;
    end Add;
  begin
    -- Create a deep copy of the representation
    Copy (Over_The_Tree => Obj);
    -- Replace the original representation with the copy
    Obj.Rep := New_Tree.Rep;
    -- Null out the spare reference to the copy (so that when New_Tree
    -- gets finalized on exit from this procedure, we don't Clear it
    -- down). NB, mustn't do a whole-record assignment here or we'll
    -- end up with a recursive disaster).
    New_Tree.Rep := null;
    New_Tree.Size := 0;
  end Adjust;

  procedure Finalize (Obj : in out AVL_Tree) is
  begin
    Clear (Obj);
  end;

end BC.Containers.Trees.AVL;
