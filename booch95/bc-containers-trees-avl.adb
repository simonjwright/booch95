-- $Id$

with Ada.Unchecked_Deallocation;

package body BC.Containers.Trees.AVL is

  procedure Delete is
     new Ada.Unchecked_Deallocation (AVL_Node, AVL_Node_Ref);

  -- Supporting subprograms

  procedure Purge (Node : in out AVL_Node_Ref) is
  begin
    if Node /= null then
      Purge (Node.Left);
      Purge (Node.Right);
      Delete (Node);
    end if;
  end Purge;

  procedure Search_Insert (Obj : in out AVL_Tree;
                           Elem : Item;
                           Node : in out AVL_Node_Ref;
                           Increased : out Boolean;
                           Inserted : out Boolean) is
    P1, P2 : AVL_Node_Ref;
  begin
    Inserted := True ;
    pragma Assert (Obj.Less_Than /= null, "Less_Than func is null!");
    if Node = null then
      Node := new AVL_Node'(Elem => Elem, Left=>null,
                            Right=>null,Balance=>Middle);
      Increased := True;
    elsif Obj.Less_Than (Elem, Node.Elem) then
      Search_Insert (Obj, Elem, Node.Left, Increased, Inserted);
      if Increased then
        case Node.Balance is
          when Right => -- numbers need to be an enumerated type
            Node.Balance := Middle; Increased := True;
          when Middle =>
            Node.Balance := Left;
          when Left =>
            P1 := Node.Left;
            if P1.Balance = Left then
              Node.Left := P1.Right;
              P1.Right := Node;
              Node.Balance := Middle;
              Node := P1;
            else
              P2 := P1.Right;
              P1.Right := P2.Left;
              P2.Left := P1;
              Node.Left := P2.Right;
              P2.Right := Node;
              if P2.Balance = Left then
                Node.Balance := Right;
              else
                Node.Balance := Middle;
              end if;
              if P2.Balance = Right then
                P1.Balance := Left;
              else
                P1.Balance := Middle;
              end if;
              Node := P2;
            end if;
            Node.Balance := Middle;
            Increased := True;
        end case;
      end if;
    elsif Obj.Less_Than (Node.Elem, Elem) then
      Search_Insert (Obj, Elem, Node.Right, Increased, Inserted);
      if Increased then
        case Node.Balance is
          when Left => -- numbers need to be an enumerated type
            Node.Balance := Middle;
            Increased := True;
          when Middle =>
            Node.Balance := Right;
          when Right =>
            P1 := Node.Right;
            if P1.Balance = Right then
              Node.Right := P1.Left;
              P1.Left := Node;
              Node.Balance := Middle;
              Node := P1;
            else
              P2 := P1.Left;
              P1.Left := P2.Right;
              P2.Right := P1;
              Node.Right := P2.Left;
              P2.Left := Node;
              if P2.Balance = Right then
                Node.Balance := Left;
              else
                Node.Balance := Middle;
              end if;
              if P2.Balance = Left then
                P1.Balance := Right;
              else
                P1.Balance := Middle;
              end if;
              Node := P2;
            end if;
            Node.Balance := Middle;
            Increased := True;
        end case;
      end if;
    else
      Inserted := False;
    end if;
  end Search_Insert;

  procedure Balance_Left (Node : in out AVL_Node_Ref;
                          Decreased : out Boolean) is
    P1, P2 : AVL_Node_Ref;
    Balance1, Balance2 : Balance_Values;
  begin
    case Node.Balance is
      when Left =>
        Node.Balance := Middle;
      when Middle =>
        Node.Balance := Right;
        Decreased := False;
      when Right =>
        P1 := Node.Right;
        Balance1 := P1.Balance;
        if Balance1 >= Middle then
          Node.Right := Node.Left;
          P1.Left := Node;
          if Balance1 = Middle then
            Node.Balance := Right;
            P1.Balance := Left;
            Decreased := False;
          else
            Node.Balance := Middle;
            P1.Balance := Middle;
          end if;
          Node := P1;
        else
          P2 := P2.Left;
          Balance2 := P2.Balance;
          P1.Left := P2.Right;
          P2.Right := P1;
          Node.Right := P2.Left;
          P2.Left := Node;
          if Balance2 = Right then
            Node.Balance := Left;
          else
            Node.Balance := Middle;
          end if;
          if Balance2 = Left then
            P1.Balance := Right;
          else
            P1.Balance := Middle;
          end if;
          Node := P2;
          P2.Balance := Middle;
        end if;
    end case;
  end Balance_Left;

  procedure Balance_Right (Node : in out AVL_Node_Ref;
                           Decreased : out Boolean)  is
    P1, P2 : AVL_Node_Ref;
    Balance1, Balance2 : Balance_Values;
  begin
    case Node.Balance is
      when Right =>
        Node.Balance := Middle;
      when Middle =>
        Node.Balance := Left;
        Decreased := False;
      when Left =>
        P1 := Node.Left;
        Balance1 := P1.Balance;
        if Balance1 <= Middle then
          Node.Left := Node.Right;
          P1.Right := Node;
          if Balance1 = Middle then
            Node.Balance := Left;
            P1.Balance := Right;
            Decreased := False;
          else
            Node.Balance := Middle;
            P1.Balance := Middle;
          end if;
          Node := P1;
        else
          P2 := P2.Right;
          Balance2 := P2.Balance;
          P1.Right := P2.Left;
          P2.Left := P1;
          Node.Left := P2.Right;
          P2.Right := Node;
          if Balance2 = Left then
            Node.Balance := Right;
          else
            Node.Balance := Middle;
          end if;
          if Balance2 = Right then
            P1.Balance := Left;
          else
            P1.Balance := Middle;
          end if;
          Node := P2;
          P2.Balance := Middle;
        end if;
    end case;
  end Balance_Right;

  procedure Delete (T1, T2 : in out AVL_Node_Ref;
                    Decreased : out Boolean) is
  begin
    if T2.Right /= null then
      Delete (T1, T2.Right, Decreased);
      if T2.Left = null and then T2.Right = null then
        T2.Balance := Middle;
      end if;
      if Decreased then
        Balance_Right (T2, Decreased);
      end if;
    else
      T1.Elem := T2.Elem;
      T1 := T2;
      T2 := T2.Left;
      Decreased := True;
    end if;
  end Delete;

  procedure Search_Delete (Obj: in out AVL_Tree;
                           Elem : Item;
                           Node : in out AVL_Node_Ref;
                           Decreased : out Boolean;
                           Deleted : out Boolean) is
    Q : AVL_Node_Ref;
  begin
    pragma Assert (Obj.Less_Than /= null, "Less_Than function is null!");
    Deleted := False;
    if Node /= null then
      if Obj.Less_Than (Elem, Node.Elem) then
        Search_Delete (Obj, Elem, Node.Left, Decreased, Deleted);
        if Decreased then
          Balance_Right (Node, Decreased);
        end if;
      elsif Obj.Less_Than (Node.Elem, Elem) then
        Search_Delete (Obj, Elem, Node.Right, Decreased, Deleted);
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
        Delete (Q);
      end if;
    end if;
  end Search_Delete;

  function Search (Obj: AVL_Tree;
                   Elem: Item;
                   Node: AVL_Node_Ref) return Boolean is
  begin
    pragma Assert (Obj.Less_Than /= null, "Less_Than function is null!");
    if Node /= null then
      if Node.Elem = Elem then
        return True;
      elsif Obj.Less_Than (Elem, Node.Elem) then
        return Search (Obj, Elem, Node.Left);
      else
        return Search (Obj, Elem, Node.Right);
      end if;
    else
      return False;
    end if;
  end Search;

  function ItemOf (Node : AVL_Node_Ref;
                   Elem : Item;
                   Less : Is_Less_Than) return Item_Ptr is
  begin
    if Node /= null then
      if Node.Elem = Elem then
        return Node.Elem'access;
      elsif Less (Elem, Node.Elem) then
        return ItemOf (Node.Left, Elem, Less);
      else
        return ItemOf (Node.Right, Elem ,Less);
      end if;
    else
      return null;
    end if;
  end ItemOf;

  function Traverse (Node : AVL_Node_Ref;
                    Iter_Func : Iteration_Function) return Boolean is
    Temp : AVL_Node_Ref;
  begin
    if Node /= null then
      Temp := Node.Left;
      if not Traverse (Temp, Iter_Func) then
        return False;
      end if;
      if not Iter_Func (Temp.Elem) then
        return False;
      end if;
      Temp := Node.Right;
      if not Traverse (Temp, Iter_Func) then
        return False;
      end if;
    end if;
    return True;
  end Traverse;

  --end supporting functions

  procedure Set_Less_Than_Func (Obj : in out AVL_Tree; F : Is_Less_Than) is
  begin
    Obj.Less_Than := F;
  end Set_Less_Than_Func;

  procedure Clear (Obj : in out AVL_Tree) is
  begin
    Purge (Obj.Rep);
    Obj.Size := 0;
  end Clear;

  procedure Insert (Obj : in out AVL_Tree;
                    Elem : in Item;
                    Already_Existed : out Boolean) is
    Increased, Result : Boolean;
  begin
    Search_Insert (Obj, Elem, Obj.Rep, Increased, Result);
    if Increased then
      Obj.Size := Obj.Size + 1;
    end if;
    Already_Existed := not Result;
  end Insert;

  procedure Delete (Obj : in out AVL_Tree; Elem : Item; Found : out Boolean) is
    Result : Boolean;
  begin
    Search_Delete (Obj, Elem, Obj.Rep, Found, Result);
    if Found then
      Obj.Size := Obj.Size - 1;
    end if;
  end Delete;

  function Extent (Obj : in AVL_Tree) return Natural is
  begin
    return Obj.Size;
  end Extent;

  function Is_Null (Obj : in AVL_Tree) return Boolean is
  begin
    return Obj.Rep = null;
  end Is_Null;

  function Is_Member (Obj : in AVL_Tree; Elem : Item) return Boolean is
  begin
    return Search (Obj, Elem, Obj.Rep);
  end Is_Member;

  function Item_Of (Obj : AVL_Tree; Elem : Item) return Item_Ptr is
  begin
    pragma Assert (Obj.Less_Than /= null, "Less_Than function is null!");
    return ItemOf (Obj.Rep, Elem, Obj.Less_Than);
  end Item_Of;

  function Traverse (Obj : in AVL_Tree; Iter_Func : Iteration_Function)
                     return Boolean is
  begin
    return Traverse (Obj.Rep, Iter_Func);
  end Traverse;

  procedure Initialize (Obj : in out AVL_Tree) is
  begin
    null;
  end Initialize;

  procedure Finalize (Obj : in out AVL_Tree) is
  begin
    Clear(Obj);
  end;

end BC.Containers.Trees.AVL;
