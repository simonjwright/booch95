-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
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
-- This file contains the specification of the bounded stack form

with BC.Support.Bounded;
generic
   Maximum_Size : Positive;
   with package Bnd_Stack_Nodes is 
    new BC.Support.Bounded(Item,Item_Ptr,Maximum_Size);
package BC.Containers.Stacks.Bounded is

   -- See spec documentation for Stacks unless overriding comment apply.

   type Bnd_Stack is new Stack with private;

   procedure Clear    (Obj : in out Bnd_Stack);
   procedure Push   (Obj : in out Bnd_Stack; Elem : Item);
   procedure Pop      (Obj : in out Bnd_Stack);
   function Available (Obj : in Bnd_Stack) return Natural;
   -- Indicated number of empty "Item slots" left in Stack

   function Depth    (Obj : in Bnd_Stack) return Natural;
   function Is_Empty  (Obj : in Bnd_Stack) return Boolean;
   function Top     (Obj : in Bnd_Stack) return Item;
   function Top     (Obj : in Bnd_Stack) return Item_Ptr;

   function "="(Left, Right : in Bnd_Stack) return boolean;
private

   function Cardinality(Obj : in Bnd_Stack) return Integer;
   procedure Purge(Obj : in out Bnd_Stack);
   procedure Add(Obj : in out Bnd_Stack; Elem : in out Item);
   function Item_At (Obj : in Bnd_Stack; Index : Natural) return Item_Ptr;

   procedure Adjust(Obj : in out Bnd_Stack);
   procedure Finalize(Obj : in out Bnd_Stack);

   use Bnd_Stack_Nodes;

   type Bnd_Stack is new Stack with record
      Rep : Bnd_Stack_Nodes.Bnd_Node_Ref := new Bnd_Node;
   end record;

end BC.Containers.Stacks.Bounded;

