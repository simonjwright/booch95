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
with BC.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Trees.Multiway is

  type Multiway_Tree is private;

  function Create (From : Multiway_Tree) return Multiway_Tree;

  function "=" (Left, Right : Multiway_Tree) return Boolean;

  procedure Clear (Obj : in out Multiway_Tree);
  procedure Insert (Obj : in out Multiway_Tree; Elem : in Item);

  procedure Append (Obj : in out Multiway_Tree;
                    Elem : in Item);
  procedure Append (Obj : in out Multiway_Tree;
                    Elem : in Item;
                    After : Natural);
  procedure Append (Obj : in out Multiway_Tree;
                    From_Tree : in out Multiway_Tree);

  procedure Remove (Obj : in out Multiway_Tree; Index : Natural);
  procedure Share (Obj : in out Multiway_Tree;
                   Share_With : in Multiway_Tree;
                   Child : Natural);
  procedure Swap_Child (Obj : in out Multiway_Tree;
                        Swap_WIth : in out Multiway_Tree;
                        Child : in Natural);
  procedure Child (Obj : in out Multiway_Tree; Child : in Natural);
  procedure Parent (Obj : in out Multiway_Tree);
  procedure Set_Item (Obj : in out Multiway_Tree; Elem : in Item);

  function Arity (Obj : Multiway_Tree) return Natural;

  function Has_Children (Obj : in Multiway_Tree) return Boolean;
  function Is_Null (Obj : in Multiway_Tree) return Boolean;
  function Is_Shared (Obj : in Multiway_Tree) return Boolean;
  function Is_Root (Obj : in Multiway_Tree) return Boolean;
  function Item_At (Obj : in Multiway_Tree) return Item;

private

  package Nodes is new Bc.Support.Nodes (Item, Storage_Manager, Storage);

  type Multiway_Tree is new Ada.Finalization.Controlled with record
    Rep : Nodes.Multiway_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Multiway_Tree);
  procedure Adjust (Obj : in out Multiway_Tree);
  procedure Finalize (Obj : in out Multiway_Tree);

end BC.Containers.Trees.Multiway;
