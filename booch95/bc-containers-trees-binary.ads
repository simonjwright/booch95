-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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

with Ada.Finalization;
with Bc.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Trees.Binary is

  type Binary_Tree is new Ada.Finalization.Controlled with private;

  type Child_Branch is (Left, Right);

  function Create (From : Binary_Tree) return Binary_Tree;

  function "=" (Left, Right : Binary_Tree) return Boolean;

  procedure Clear (Obj : in out Binary_Tree);
  procedure Insert (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch);
  procedure Append (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch;
                    After : in Child_Branch);
  procedure Remove (Obj : in out Binary_Tree; Child : in Child_Branch);
  procedure Share (Obj : in out Binary_Tree;
                   Share_With : in Binary_Tree;
                   Child : in Child_Branch);
  procedure Swap_Child (Obj : in out Binary_Tree;
                        Swap_WIth : in out Binary_Tree;
                        Child : in Child_Branch);
  procedure Child (Obj : in out Binary_Tree; Child : in Child_Branch);
  procedure Left_Child (Obj : in out Binary_Tree);
  procedure Right_Child (Obj : in out Binary_Tree);
  procedure Parent (Obj : in out Binary_Tree);
  procedure Set_Item (Obj : in out Binary_Tree; Elem : in Item);

  function Has_Children (Obj : in Binary_Tree) return boolean;
  function Is_Null (Obj : in Binary_Tree) return boolean;
  function Is_Shared (Obj : in Binary_Tree) return boolean;
  function Is_Root (Obj : in Binary_Tree) return boolean;
  function Item_At (Obj : in Binary_Tree) return Item;

private

  package Nodes is new Bc.Support.Nodes (Item, Storage_Manager, Storage);

  procedure Purge (Node : in out Nodes.Binary_Node_Ref);

  type Binary_Tree is new Ada.Finalization.Controlled with record
    Rep : Nodes.Binary_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Binary_Tree);
  procedure Adjust (Obj : in out Binary_Tree);
  procedure Finalize (Obj : in out Binary_Tree);

end BC.Containers.Trees.Binary;
