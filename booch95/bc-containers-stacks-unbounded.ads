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

with BC.Support.Unbounded;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Stacks.Unbounded is

  type Unb_Stack is new Stack with private;
  -- This Stack exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  function "=" (Left, Right : in Unb_Stack) return boolean;
  procedure Clear (Obj : in out Unb_Stack);
  procedure Push (Obj : in out Unb_Stack; Elem : Item);
  procedure Pop (Obj : in out Unb_Stack);
  function Depth (Obj : in Unb_Stack) return Natural;
  function Is_Empty (Obj : in Unb_Stack) return Boolean;
  function Top (Obj : in Unb_Stack) return Item;
  function Top (Obj : in Unb_Stack) return Item_Ptr;

private

  function Item_At (Obj : in Unb_Stack; Index : in Natural) return Item_Ptr;
  function Cardinality (Obj : in Unb_Stack) return Integer;
  procedure Purge (Obj : in out Unb_Stack);
  procedure Add (Obj : in out Unb_Stack; Elem : in out Item);

  package Unb_Stack_Nodes
  is new BC.Support.Unbounded (Item, Item_Ptr, Storage_Manager, Storage);

  type Unb_Stack is new Stack with record
    Rep : Unb_Stack_Nodes.Unb_Node_Ref := new Unb_Stack_Nodes.Unb_Node;
  end record;

  procedure Initialize (Obj : in out Unb_Stack);
  procedure Adjust (Obj : in out Unb_Stack);
  procedure Finalize (Obj : in out Unb_Stack);

end BC.Containers.Stacks.Unbounded;
